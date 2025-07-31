# Boltzmannomics: A Framework for Agent-Based Economic Simulations Inspired by Statistical Physics

Agent-based economic simulation framework leveraging CUDA Fortran and multicore processing for GPU and CPU parallelism. Models based on analogies with statistical physics such as kinetic exchange, spin flips, magnetization, and more.

Boltzmann PDF fit for simple exchange with debt (see reference 2):

![image](images/50000.step_cash_boltzmann.png "Boltzmann PDF fit for simple exchange with debt")

## Why is this Framework Useful for Researchers?

- **Start right away**: Running included models and changing their parameters is as simple as editing two small text files.
- **Get results fast**: Boltzmannomics uses GPU & CPU parallelized Fortran & Python wherever possible to support **millions** of agents, during runtime and post-processing.
- **Create custom models**: This library is great for researchers developing new models in _modern_ Fortran since there are clear examples & instructions for how to slot new ABMs into the existing object-oriented Boltzmannomics framework.

## Features

- Implemented in modern Fortran (F2003+ OOP, F95+ dynamic arrays, etc.)
    - Uses the F2008 language feature [`do concurrent`](https://developer.nvidia.com/blog/using-fortran-standard-parallel-programming-for-gpu-acceleration/) and [OpenMP](https://www.openmp.org/wp-content/uploads/OpenMP-4.5-1115-F-web.pdf) for GPU/CPU parallelism
- Simplified build system using [fpm](https://fpm.fortran-lang.org/index.html)
- Easy for users to build their own custom models with the framework using existing models as a guide
- Python post-processing tools with optional CPU parallelism
    - Histograms for population properties with optional probability distribution fitting
    - Line plots of summary stats for population properties over time
- Available simulation models (see [MODELS.md](MODELS.md) for more details):
    - `SimpleExchange` - Simple random exchange with configurable debt limit
    - `KineticIsing` - Extension of `SimpleExchange` where agents have buy/sell 'spins'
    - `CCMExchange` - Extension of `SimpleExchange` where agents have individual propsensities to save
- Builds with `gfortran` 15.1.0+ and `nvfortran`

## Commands

From top-level of the repo:
- Copy a pair of .nml files from the templates folder into the top-level directory
- Build: `source nvfortran-environment-vars.sh && ~/fpm build`
- Run: `rm out/*; ~/fpm run`

From inside the postprocess folder, with the Python env activated (use python-3.12.11-requirements.txt to create env):
- Create histograms for Agent properties: `rm histograms/*; python histograms.py`
    - OPTIONAL: Use the `-j` or `--threads` flag to set the number of parallel threads to use when plotting histograms
    - OPTIONAL: Use the `-b` or `--bins` flag to set the number of bins to use for the histograms
    - OPTIONAL: Use the `--boltzmann` flag to fit a Boltzmann probability distribution function to the histograms
- Create plots of Agent summary stats and simulation metrics over time: `rm stats/*; python stats.py`
    - OPTIONAL: Plot _only_ simulation metrics (like Gini coefficient) with `--metrics` flag
    - OPTIONAL: Plot _only_ Agent population stats `--population` flag

- Profiling (basic) (Adapted from [this book](https://shop.elsevier.com/books/cuda-fortran-for-scientists-and-engineers/ruetsch/978-0-443-21977-1)):
    - `nsys profile -o nsys.log ~/fpm run`
    - `nsys stats nsys.log.nsys-rep` - look at "CUDA GPU Kernel Summary" which has the `do concurrent` loops listed

## Setup Notes

The fortran package manager (v0.12.0+) is assumed to be installed at ~/fpm

**Tested compiler configurations**:
|              | gfortran 15.1.0 (CPU) | nvfortran 25.5-0 |
| --- | ---- | ---- |
| x64 Ubuntu 24.04        | ✅       | ✅ GPU, CPU           | 
| arm64 Ubuntu 24.04       | ❓       | ❓ CPU                    | 

The Nvidia compiler is reccomended, however compatibility is maintained with the open-source `gfortran` compiler to be in line with JOSS reccomendations.
- To get an updated gfortran, create a `conda` environment and run: 
    - `conda install -c conda-forge gfortran=15.1.0`
    - `conda install conda-forge::hdf5`

Compilation notes:
- To build with CPU parallelism instead of GPU with `nvfortran`, swap the commented out lines in nvfortran-environment-vars.sh
- To build with `gfortran` with CPU parallelism, use `source gfortran-environment-vars.sh && ~/fpm build`
    - Set the number of cores used with GFORT_N_CORES in gfortran-environment-vars.sh
    - Set the gfortran 15.1+ compiler path in gfortran-environment-vars.sh
- View `nvfortran` optimization printouts: `source nvfortran-environment-vars.sh && ~/fpm clean && rm -f compile.txt && ~/fpm build --verbose &> compile.txt`

(Optional) for a better development experience use the Modern Fortran VS Code extension. On the main development machine the fortran language server is located in a conda env here: /home/linuxuser/miniconda3/envs/fortran-fortls along with fprettify for code formatting.

## Adding More Simulation Models

Due to the abstraction in the code, the process for adding new models is straighforward:
1. Subclass the AbstractSimulation class in a new module / .f90 prefixed with "model_"
    1. Add the new class the factory in to sim_factory.f90
2. Subclass the AbstractConfig class within kinds_config.f90
    1. Add the new class the create_config() factory function in to kinds_config.f90

True OOP is not possible inside of `do concurrent` loops (at the moment) due to the CUDA Fortran restriction on the type of statements allowed inside of a `do concurrent` loop. However, steps have still been taken to use abstraction and encapsulation where possible. See [this link](https://chatgpt.com/share/687eed45-eef8-8012-af2c-666cf7cd9341) for more information about the restrictions on `do concurrent`.

As we move beyond classic exchange-style models with trivial agent structs to true agent objects (strategy-follwing/history-aware/NNs/etc.), it may prove necessary to rely on OpenMP loops for parallelism.
See:
- nvfortran: https://docs.nvidia.com/hpc-sdk/archive/25.5/compilers/hpc-compilers-user-guide/index.html#using-openmp
- gfortran: 
    - https://gcc.gnu.org/onlinedocs/gfortran/OpenMP.html
    - https://gcc.gnu.org/onlinedocs/gcc/OpenMP-and-OpenACC-Options.html#OpenMP-and-OpenACC-Options

## TODOs

### Models

Classic models:
- CCM model - section II.A of reference (3)
- CPT model - section II.A of reference (3)
- BM model (possibly discrete version) - section II.B of reference (3)

Continue going through reference (3) and identify more prebuilt models to include in the library

### Code Updates

- Move the run() method implementation into the `AbstractSimulation` class directly since that is the same for all models - step() is where the real magic happens
- Benchmarks: gfortran cpu (serial) vs nvidia cpu (parallel) vs nvidia gpu
- Update the sim_factory_m module with another routine to construct simulators without file I/O
- Add some initial tests. Will likely need to expand the `AbtractSimulation` interface with some "test_" helper routines
- Custom HDF5-like binary format instead of CSV for population data 
    - Folder containing binary files
    - Each binary file is directly an unformatted array that was formerly a CSV column
    - Should be readable with numpy np.fromfile
- Add support for the [lfortran](https://lfortran.org/) compiler once [namelist support](https://github.com/lfortran/lfortran/issues/1999) is added. It [supports `do concurrent`](https://lfortran.org/blog/2024/07/lfortran-supports-openmp-pragmas-and-do-concurrent/)!

## References

1. Colloquium: Statistical mechanics of money, wealth, and income (2009). https://journals.aps.org/rmp/abstract/10.1103/RevModPhys.81.1703
2. Statistical mechanics of money (2000). https://doi.org/10.1007/s100510070114
3. Twenty-five years of random asset exchange modeling (2024). https://link.springer.com/article/10.1140/epjb/s10051-024-00695-3

## Other Notes
- Apparently gfortran needs .nml files to end in a newline character... [link](https://stackoverflow.com/a/46249863)
- gfortran 15.1 released in April 2025 finally supports locality spec for `do concurrent`