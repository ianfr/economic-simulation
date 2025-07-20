# Boltzmannomics: A Framework for Agent-Based Economic Simulations Inspired by Statistical Physics

Agent-based economic simulation framework leveraging CUDA Fortran and multicore processing for GPU and CPU parallelism. Models based on analogies with statistical physics such as kinetic exchange, spin flips, magnetization, and more.

Boltzmann PDF fit for simple exchange with debt (see reference 2):

![image](images/50000.step_cash_boltzmann.png "Boltzmann PDF fit for simple exchange with debt")

## Why is this Framework Useful for Researchers?

- **Start right away**: Running included models and changing their parameters is as simple as editing two small text files.
- **Get results fast**: Boltzmannomics uses GPU & CPU parallelized Fortran & Python wherever possible, during runtime and post-processing.
- **Create custom models**: This library is great for researchers developing new models in _modern_ Fortran since there are clear examples & instructions for how to slot new ABMs into the existing object-oriented Boltzmannomics framework.

## Features

- Implemented in modern Fortran (F2003+ OOP, F95+ dynamic arrays, etc.)
    - Uses the F2008 language feature [`do concurrent`](https://developer.nvidia.com/blog/using-fortran-standard-parallel-programming-for-gpu-acceleration/) for GPU and/or CPU parallelism
- Simplified build system using [fpm](https://fpm.fortran-lang.org/index.html)
- Easy for users to build their own custom models with the framework using existing models as a guide
- Python post-processing tools with optional CPU parallelism
    - Histograms for population properties with optional probability distribution fitting
    - Line plots of summary stats for population properties over time
- Available simulation models (see [MODELS.md](MODELS.md) for more details):
    - `SimpleExchange` - Simple random exchange with configurable debt limit
    - `KineticIsing` - Extension of `SimpleExchange` where agents have buy/sell 'spins'

## Commands

From top-level of the repo:
- Copy a pair of .nml files from the templates folder into the top-level directory
- Build: `source nvfortran-environment-vars.sh && ~/fpm build`
- Run: `rm out/*; ~/fpm run`
- Profiling (basic) (Adapted from [this book](https://shop.elsevier.com/books/cuda-fortran-for-scientists-and-engineers/ruetsch/978-0-443-21977-1)):
    - `nsys profile -o nsys.log ~/fpm run`
    - `nsys stats nsys.log.nsys-rep` - look at "CUDA GPU Kernel Summary" which has the `do concurrent` loops listed

From inside the postprocess folder, with the Python env activated (use python-3.12.11-requirements.txt to create env):
- Create histograms for Agent properties: `rm histograms/*; python histograms.py`
    - OPTIONAL: Use the `-j` or `--threads` flag to set the number of parallel threads to use when plotting histograms
    - OPTIONAL: Use the `-b` or `--bins` flag to set the number of bins to use for the histograms
    - OPTIONAL: Use the `--boltzmann` flag to fit a Boltzmann probability distribution function to the histograms
- Create plots of Agent summary stats and simulation metrics over time: `rm stats/*; python stats.py`
    - OPTIONAL: Plot _only_ simulation metrics (like Gini coefficient) with `--metrics` flag
    - OPTIONAL: Plot _only_ Agent population stats `--population` flag

Compilation notes:
- To build with CPU parallelism instead of GPU with `nvfortran`, swap the commented out lines in nvfortran-environment-vars.sh
- To build with `gfortran` with CPU parallelism, use `source gfortran-environment-vars.sh && ~/fpm build`
    - Set the number of cores used with GFORT_N_CORES in gfortran-environment-vars.sh

## Setup Notes

**Tested compiler configurations**:
|              | gfortran `--tree-parallelize-loops` | nvfortran `stdpar=gpu` | nvfortran `stdpar=multicore` |
|--------------|----------|------------------------|------------------------------|
| x64 Ubuntu 24.04        | ✅       | ✅                    | ✅                           |
| arm64        | ❓       | ❓                    | ❓                           | 

Output from `nvfortran --version` on main development machine:

```text
nvfortran 25.5-0 64-bit target on x86-64 Linux -tp alderlake
```

On the dev machine the fortran language server is located in a conda env here:
/home/linuxuser/miniconda3/envs/fortran-fortls 
along with fprettify for code formatting

The fortran package manager (v1.3) is installed at ~/fpm on the dev machine

## Adding More Simulation Models

Due to the abstraction in the code, the process for adding new models is straighforward:
1. Subclass the AbstractSimulation class in a new module / .f90 prefixed with "model_"
    1. Add the new class the factory in to sim_factory.f90
2. Subclass the AbstractConfig class within kinds_config.f90
    1. Add the new class the create_config() factory function in to kinds_config.f90

True OOP is not possible throughout the entire codebase (at the moment) due to the CUDA Fortran restriction on the type of statements allowed inside of a `do concurrent` loop and the project's goal to avoid low-level CUDA programming and directive-based programming like OpenACC and OpenMP. However, steps have still been taken to use abstraction and encapsulation where possible.

## TODOs

### Models

Classic models:
- CCM model - section II.A of reference (3)
- CPT model - section II.A of reference (3)
- BM model (possibly discrete version) - section II.B of reference (3)

Continue going through reference (3) and identify more prebuilt models to include in the library

### Code Updates

- Update the sim_factory_m module with another routine to construct simulators without file I/O
- Add some initial tests. Will likely need to expand the `AbtractSimulation` interface with some "test_" helper routines

## References

1. Colloquium: Statistical mechanics of money, wealth, and income (2009). https://journals.aps.org/rmp/abstract/10.1103/RevModPhys.81.1703
2. Statistical mechanics of money (2000). https://doi.org/10.1007/s100510070114
3. Twenty-five years of random asset exchange modeling (2024). https://link.springer.com/article/10.1140/epjb/s10051-024-00695-3