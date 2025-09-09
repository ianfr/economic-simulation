# See https://developer.hashicorp.com/terraform/tutorials/aws-get-started/aws-create#create-infrastructure
# See https://developer.hashicorp.com/terraform/language/provisioners#file
# See https://registry.terraform.io/providers/hashicorp/aws/latest/docs/resources/route_table

provider "aws" {
  region = "us-east-1"
}

# Find the latest Ubuntu 24.04 AMI
data "aws_ami" "ubuntu" {
  most_recent = true

  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd-gp3/ubuntu-noble-24.04-amd64-server-*"]
  }

  owners = ["099720109477"] # Canonical
}

resource "aws_key_pair" "my_terraform_key" {
  key_name   = "my_aws_key"
  public_key = file("~/.ssh/my_aws_key.pub")
}

resource "aws_vpc" "main" {
  cidr_block           = "10.0.0.0/16"
  enable_dns_hostnames = true
  enable_dns_support   = true
  tags = {
    Name = "main-vpc"
  }
}

# Add internet gateway for public subnet
resource "aws_internet_gateway" "main" {
  vpc_id = aws_vpc.main.id
  tags = {
    Name = "main-igw"
  }
}

# Add public subnet
resource "aws_subnet" "public" {
  vpc_id                  = aws_vpc.main.id
  cidr_block              = "10.0.1.0/24"
  availability_zone       = "us-east-1a"
  map_public_ip_on_launch = true
  tags = {
    Name = "public-subnet"
  }
}

# Add route table for public subnet
resource "aws_route_table" "public" {
  vpc_id = aws_vpc.main.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.main.id
  }

  tags = {
    Name = "public-route-table"
  }
}

# Associate route table with public subnet
resource "aws_route_table_association" "public" {
  subnet_id      = aws_subnet.public.id
  route_table_id = aws_route_table.public.id
}

resource "aws_security_group" "ssh_security_group" {
  name        = "ssh-access-sg"
  description = "Allow SSH inbound traffic"
  vpc_id      = aws_vpc.main.id

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_instance" "boltzmannomics_server" {
  ami                         = data.aws_ami.ubuntu.id
  instance_type               = "t3.large"
  key_name                    = aws_key_pair.my_terraform_key.key_name
  vpc_security_group_ids      = [aws_security_group.ssh_security_group.id]
  subnet_id                   = aws_subnet.public.id
  associate_public_ip_address = true

  root_block_device {
    volume_size = 50 # GiB
    volume_type = "gp3"
  }

  tags = {
    Name = "boltzmannomics"
  }

  # Copy up the entire project since we want to be able to modify the Fortran code anyways
  provisioner "remote-exec" {
    inline = ["mkdir -p /home/ubuntu/economic-simulation"] # Make sure the path exists

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file("~/.ssh/my_aws_key")
      host        = self.public_ip
    }
  }

  provisioner "file" {
    source      = "../../economic-simulation/"
    destination = "/home/ubuntu/economic-simulation/"
    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file("~/.ssh/my_aws_key")
      host        = self.public_ip
    }
  }

  # Update Docker and pull the runtime image from Docker Hub
  provisioner "remote-exec" {
    inline = [
      "sudo chmod +x /home/ubuntu/economic-simulation/terraform/setup.sh",
      "sh /home/ubuntu/economic-simulation/terraform/setup.sh"
    ]

    connection {
      type        = "ssh"
      user        = "ubuntu"
      private_key = file("~/.ssh/my_aws_key")
      host        = self.public_ip
    }
  }
}
