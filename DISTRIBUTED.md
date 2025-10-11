# Boltzmannomics: Local and Distributed Parallelism

## Parallelization with this framework

In general, there are 3 ways to take advantage of parallelism with Boltzmannomics:
1. Running a single simulation at once on a single host ➡️ use `do concurrent` on CPU or GPU
2. Running a large batch of simulations that are each relatively small (a 'study') ➡️ use [Cluster Workload Manager](https://github.com/ianfr/cluster-workload-manager)
3. Running a simulation with a very large population and/or complex agents ➡️ use MPI on a cluster

Summarized: 
1. Single-host std parallelism
2. Batch runs with [Cluster Workload Manager](https://github.com/ianfr/cluster-workload-manager)
3. Distributed simulation with MPI

## Single-host std parallelism

This is the default parallelism mode and the easiest to get started with.

## Batch runs with [Cluster Workload Manager](https://github.com/ianfr/cluster-workload-manager)

### Cluster Workload Manager on a single computer

This is the "standalone" mode referred to [here](https://github.com/ianfr/cluster-workload-manager?tab=readme-ov-file#architecture). Refer to those instructions for how to get the workload manager running.

The `install-to-fileshare.sh` and `cluster-edit-and-run-single.sh` Bash scripts greatly reduce the manual steps involved in running multiple simulations in parallel. Be sure to update `FILESHARE_PATH` in both scripts to reflect your local setup.

**Step 1:** Edit .nml files in the top-level Boltzmannomics directory

**Step 2:** Build and 'install' Boltzmannomics: `source gfortran-environment-vars.sh && ~/fpm build && bash install-to-fileshare.sh`

**Step 3:** Get, build, and launch the Cluster Workload Manager:
- (new terminal) `cd && git clone https://github.com/ianfr/cluster-workload-manager.git && cd cluster-workload-manager`
- `go build -o workload-manager && ./workload-manager -config templates/config-standalone.json`
- (new terminal) `cd ~/cluster-workload-manager/monitor && go build -o monitor && ./monitor -manager localhost:8080 -port 8081`

**Step 4:** Submit jobs to the workload manager. This can be done on the command-line using something similar to the command below. Recall that `cluster-edit-and-run-single.sh` handles creating new folders for you and lets you vary namelist parameters which could be very useful for performing a study.

```bash
curl -X POST http://localhost:8080/api/v1/jobs/submit \
    -H "Content-Type: application/json" \
    -d '{
    "command": "/mnt/c/Users/ianfr/coding-local/economic-simulation/cluster-edit-and-run-single.sh n_agents 1000000",
    "threads": 1,
    "stdout_path": "/home/linuxuser/fshare/study-0.stdout",
    "stderr_path": "/home/linuxuser/fshare/study-0.stderr"
  }'
```

You should see a response like:
```
{"job_id":"bca5a929-ba4a-43f0-9699-6e2157868f2b","message":"Job submitted successfully","status":"queued"}
```

Of course, it's also possible to execute studies by submitting the HTTP requests with a Python script instead of one by one with `curl`.

**Step 5:** Head over to the web UI for the Cluster Workload Manager (at http://localhost:8081 in this example) where you can see when the run completes.

### Cluster Workload Manager on a cluster

Similar workflow to running on a single machine, except with the extra instructions for LAN multi-node setup for the Cluster Workload Manager detailed [here](https://github.com/ianfr/cluster-workload-manager/blob/main/README.md).

#### Cluster setup for Boltzmannomics

1. Set up a fileshare that all compute nodes have access to, mounted to the exact same location on each node
    - On a LAN, NFS should work fine: https://documentation.ubuntu.com/server/how-to/networking/install-nfs/
2. Setup passwordless SSH between every node and every other node
    - https://www.ssh.com/academy/ssh/copy-id
    - It may be necessary to modify ~/.ssh/config to automatically map keys to IP ranges with "Match Host"
3. Ensure all nodes have the fortran environment with the shared libraries etc in the same location
    - `gfortran` with conda: Only the main node has to actually have conda installed. After that, just copy the conda environment to the same path in all the nodes on the cluster
    - `nvfortran`: Follow the same install instructions on all nodes using apt. Used https://developer.nvidia.com/nvidia-hpc-sdk-255-downloads during testing
4. Compile and install the Boltzmannomics executable to the fileshare using *install-to-fileshare.sh*
    - Don't forget to put *.nml configs in the top-level of the repo before running the install script 


## Distributed simulation with MPI

See "**Cluster setup for Boltzmannomics**" section above for pointers on setting up an SSH/MPI cluster.

Define an MPI hostfile for the cluser similar to the one below, and save it in e.g. ~/mpi_workers.txt:

```
localhost slots=2
192.168.1.101 slots=4
192.168.1.102 slots=4
192.168.1.103 slots=4
192.168.1.104 slots=4
192.168.1.105 slots=4
192.168.1.106 slots=4
```

`cd` to where the Boltzmannomics executable was installed to on the fileshare and launch with something like:

`rm out/*; $MPIRUN --hostfile ~/mpi_workers.txt -n 24 ./Boltzmannomics`