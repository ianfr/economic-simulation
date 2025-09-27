# Boltzmannomics on a Cluster

## Parallelization with this framework

In general, there are 3 ways to take advantage of parallelism with Boltzmannomics:
1. Running a single simulation at once on a single host ➡️ use `do concurrent` on CPU or GPU
2. Running a large batch of simulations that are each relatively small (a 'study') ➡️ use GNU parallel combined with *cluster-edit-and-run-single.sh*, optionally on a cluster
3. Running a simulation with a very large population and/or complex agents ➡️ use MPI on a cluster

Summarized: 
1. Single-host std parallelism
2. Batch runs with GNU parallel
3. Distributed simulation with MPI

## Single-host std parallelism

This is the default parallelism mode and the easiest to get started with.

## Batch runs with GNU parallel

TODO

### GNU parallel on a cluster

#### Cluster setup

TODO

#### Running
TODO

## Distributed simulation with MPI

TODO