# Cloud Setup: Run in the Cloud on AWS EC2 with Terraform

## Cloud Setup: Boltamannomics + Terraform Overview

Terraform and the AWS CLI are assumed to be installed, with AWS CLI credentials sourced in the current shell.

Configs are located in the `terraform/` directory.

Modify the instance type in `main.tf` as needed for more CPUs and/or to enable GPUs. Changing the disk size is possible too.

A Docker image containing the dependencies needed for running Boltzmannomics is deployed on the EC2 machine for use there. Note that only `nvfortran` is supported and not `gfortran` since I ran into issues with building gfortran 15.1 from source.

## Cloud Setup: Deployment with Terraform and AWS

1. `cd` to the terraform/ folder
2. (One-Time) Generate an SSH key pair with `ssh-keygen -t rsa -b 4096 -f ~/.ssh/my_aws_key`
3. Set the environment variables that hold your AWS CLI secret key credentials
4. (One-Time) Initialize with `terraform init`
5. Check with `terraform validate`
6. Look over the plan with `terraform plan`
7. Deploy with `terraform apply`
    - Pulling the Boltzmannomics Docker container takes a few minutes because of the large NVidia HPC toolkit

## Cloud Setup: Use Boltzmannomics on the Terraform-Configured EC2 Instance

Obtain the public ip address of the instance with `terraform show | grep ip`

Connect via SSH to the instance with `ssh -i ~/.ssh/my_aws_key ubuntu@THE_PUBLIC_IP`

Command to launch the Docker container with the Bolzmannomics framework mounted to /src: 
`cd && sudo docker run --rm -it -v ${PWD}/economic-simulation:/src -w /src --gpus all ifriedri/economic-simulation-runtime:latest`

Note that the `--gpus all` option does no harm if the EC2 machine has no GPU and `nvfortran` also works CPU-only.
