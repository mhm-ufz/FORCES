# HPC Fortran Module Loads

Module load scripts on HPC Clusters for Fortran Projects at CHS.

## Toolchains at EVE

All these scripts will load:

- the respective fortran compiler and set `FC` env-var (optional MPI support)
- netCDF-Fortran
- CMake
- the MPR Python Environment
- pFUnit - Fortran unit testing framework

### Usage

- GNU 7.3 compiler (`foss/2018b` Toolchain):
  ```bash
  source eve.gfortran73 # or
  source eve.gfortran73MPI
  ```
- GNU 8.3 compiler (`foss/2019b` Toolchain):
  ```bash
  source eve.gfortran83 # or
  source eve.gfortran83MPI
  ```
- GNU 10.2 compiler (`foss/2020b` Toolchain):
  ```bash
  source eve.gfortran102 # or
  source eve.gfortran102MPI
  ```
- Intel 18 compiler (`iomkl/2018b` Toolchain):
  ```bash
  source eve.intel18 # or
  source eve.intel18MPI
  ```
- Intel 19 compiler (`iomkl/2020a` Toolchain):
  ```bash
  source eve.intel19 # or
  source eve.intel19MPI
  ```
- NAG 6.2 compiler:
  ```bash
  source eve.nagfor62
  ```
- PGI 19 compiler:
  ```bash
  source eve.pgi19
  ```

## Toolchains at Juwels

All these scripts will load:

- the respective fortran compiler and set `FC` env-var
- netCDF-Fortran
- CMake

### Usage

- GNU compiler with MPI
  ```bash
  source juwels.gfortranMPI
  ```
- Intel compiler with MPI
  ```bash
  source juwels.intelMPI
  ```
