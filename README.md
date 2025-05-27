# FORCES library

<div align="center">
<img src="https://git.ufz.de/chs/logos/-/raw/master/Forces.png" alt="Forces-LOGO" width="251px" style="width:251px;"/>
</div>

This is the FORTRAN library for Computational Environmental Systems of the

> Department Computational Hydrosystems<br/>
> Helmholtz Centre for Environmental Research - UFZ<br/>
> Permoserstr. 15<br/>
> 04318 Leipzig, Germany

It is a lightweight fork of the `jams_fortran` library maintained by Matthias Cuntz et al: https://github.com/mcuntz/jams_fortran

The `jams_fortran` library was formerly developed at the CHS department at the UFZ and is now released under the [MIT license](https://github.com/mcuntz/jams_fortran/blob/master/LICENSE).

[TOC]

## Using FORCES with your Fortran program
Let's assume, you want to write a fortran program using forces, like this example `test.f90`:
```fortran
program test
  use mo_message, only : message
  implicit none
  call message("This is working!")
end program test
```

You should create a minimal `CMakeLists.txt` file next to the `test.f90` file like this:
```cmake
cmake_minimum_required(VERSION 3.14 FATAL_ERROR)

# get CPM (package manager)
set(CPM_DOWNLOAD_LOCATION "${CMAKE_BINARY_DIR}/cmake/CPM_0.35.0.cmake")
file(DOWNLOAD https://github.com/cpm-cmake/CPM.cmake/releases/download/v0.35.0/CPM.cmake ${CPM_DOWNLOAD_LOCATION})
include(${CPM_DOWNLOAD_LOCATION})

# create project
project(MyProject LANGUAGES Fortran)

# add executable
add_executable(test test.f90)

# add FORCES dependency
CPMAddPackage("https://git.ufz.de/chs/forces.git@0.3.1")

# link dependencies
target_link_libraries(test forces)
```

There, `CPM` (the [cmake package manager](https://github.com/cpm-cmake/CPM.cmake)) is downloaded on the fly and used to get `FORCES` to be able to link against it.

Afterwards you only need to do the following to configure, compile and execute your program:
```bash
cmake -B build
cmake --build build --parallel
./build/test
```

And it will happily write:
```
This is working!
```

If you have the FORCES sources downloaded and you want to link a local executable against it, you can specify a path to CMake with `FORCES_EXE`:
```bash
cmake -B build -D FORCES_EXE=test.f90
cmake --build build --parallel
./build/main
```
You can use this with the examples provided in the `examples/` directory, e.g. `FORCES_EXE=examples/01_grids/01_regridding.f90`.
The executable will be always called `main`.

For a more complex project, prepared for unit-tests, documentation and modules, have a look at the [Fortran Template](https://git.ufz.de/chs/fortran-template).

## Dependencies and Requirements

* Fortran compiler: We support [gfortran](https://gcc.gnu.org/fortran/), [nagfor](https://www.nag.com/content/nag-fortran-compiler) and [ifort](https://www.intel.com/content/www/us/en/developer/tools/oneapi/overview.html)
* Build system: We support [make](https://www.gnu.org/software/make/) and [ninja](https://ninja-build.org/)
* [cmake](https://cmake.org/): Software for build automation
* [NetCDF-Fortran](https://github.com/Unidata/netcdf-fortran): NetCDF I/O for Fortran
* (optional) [fypp](https://github.com/aradi/fypp): Fortran pre-processor written in Python

It is recommended to have a clean installation at a custom location
for a C compiler, a Fortran compiler, the NetCDF C library and the
NetCDF Fortran library with consistent compilers.

We recommend to use a [conda](https://docs.conda.io/en/latest/) environment by using [Miniconda](https://docs.conda.io/en/latest/miniconda.html) to get all dependencies easily:
```bash
conda create -y --prefix ./forces_env
conda activate ./forces_env
conda config --add channels conda-forge
conda config --set channel_priority strict
conda install -y cmake make fortran-compiler netcdf-fortran fypp
```
With this you could now proceed with the example given above.


## License

LGPLv3 (c) 2005-2025 CHS-Developers
