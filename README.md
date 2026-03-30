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
cmake_minimum_required(VERSION 3.18 FATAL_ERROR)

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
cmake -B build -DFORCES_EXE=test.f90
cmake --build build --parallel
./build/main
```
You can use this with the examples provided in the `examples/` directory, e.g. `FORCES_EXE=examples/01_grids/01_regridding.f90`.
The executable will be always called `main`.

## Build Configuration

FORCES uses standard CMake inputs such as `CMAKE_BUILD_TYPE` and a small set of project-specific cache options:

* `FORCES_BUILD_TESTING`: build the FORCES pfUnit tests. Defaults to `OFF`.
* `FORCES_WITH_COVERAGE`: enable GNU coverage instrumentation for FORCES test builds.
* `FORCES_WITH_OpenMP`: enable OpenMP support.
* `FORCES_WITH_MPI`: enable MPI support.
* `FORCES_WITH_NETCDF`: enable NetCDF support.
* `FORCES_WITH_OPTIMIZATION`: include optimization routines.
* `FORCES_ENABLE_NATIVE`: enable host-native tuning for `Release` and `RelWithDebInfo` builds.
* `FORCES_EXE`: build a local executable linked against `forces`.

Typical configure commands:

```bash
# test build
cmake -B build -DCMAKE_BUILD_TYPE=Debug -DFORCES_BUILD_TESTING=ON

# OpenMP build
cmake -B build -DCMAKE_BUILD_TYPE=Release -DFORCES_BUILD_TESTING=ON -DFORCES_WITH_OpenMP=ON

# GNU coverage build
cmake -B build -DCMAKE_BUILD_TYPE=Debug -DFORCES_BUILD_TESTING=ON -DFORCES_WITH_COVERAGE=ON

# local executable build
cmake -B build -DCMAKE_BUILD_TYPE=Debug -DFORCES_EXE=test.f90
```

`FORCES_WITH_COVERAGE` is only meaningful for test builds and requires GNU Fortran.

## Dependencies and Requirements

* Fortran compiler: We support [gfortran](https://gcc.gnu.org/fortran/), [nagfor](https://www.nag.com/content/nag-fortran-compiler) and [ifort](https://www.intel.com/content/www/us/en/developer/tools/oneapi/overview.html)
* Build system: We support [make](https://www.gnu.org/software/make/) and [ninja](https://ninja-build.org/)
* [cmake](https://cmake.org/): Software for build automation
* [NetCDF-C](https://github.com/Unidata/netcdf-c): NetCDF I/O library
* (optional) [fypp](https://github.com/aradi/fypp): Fortran pre-processor written in Python

It is recommended to have a clean installation at a custom location
for a C compiler, a Fortran compiler and the NetCDF C library with consistent compilers.

We recommend to use a [conda](https://docs.conda.io/en/latest/) environment by using [Miniconda](https://docs.conda.io/en/latest/miniconda.html) to get all dependencies easily:
```bash
conda create -y --prefix ./forces_env
conda activate ./forces_env
conda config --add channels conda-forge
conda config --set channel_priority strict
conda install -y cmake make fortran-compiler libnetcdf fypp
```
With this you could now proceed with the example given above.

## Testing with pFUnit

FORCES uses [pFUnit](https://github.com/Goddard-Fortran-Ecosystem/pFUnit) for its unit tests. When `FORCES_BUILD_TESTING=ON`, CMake will look for `PFUNIT`. If pFUnit is not available, configuration still succeeds, but no FORCES tests are added.

You need to provide `pFUnit` to run the tests. It is unfortunately not available on conda yet. To compile it by hand in the current conda environment, you can do the following:

```bash
conda install -y m4
git clone https://github.com/Goddard-Fortran-Ecosystem/pFUnit.git
cmake -DCMAKE_BUILD_TYPE=Release -B pFUnit/build -S pFUnit
cmake --build pFUnit/build --parallel
cmake --install pFUnit/build --prefix $CONDA_PREFIX
rm -rf pFUnit
```

Then you can compile and run the tests and/or calculate coverage in the same environment:

```bash
cmake -B build -DCMAKE_BUILD_TYPE=Debug -DFORCES_BUILD_TESTING=ON -DFORCES_WITH_COVERAGE=ON
cmake --build build --parallel
cmake --build build --target test
cmake --build build --target coverage
```

Run all configured tests with:

```bash
ctest --test-dir build --output-on-failure
```

Run a single pFUnit suite with `ctest -R`, for example:

```bash
ctest --test-dir build --output-on-failure -R test_mo_message
```

If you only want to configure a subset of the pFUnit files, use `FORCES_TEST_GLOB`:

```bash
cmake -B build -DCMAKE_BUILD_TYPE=Debug -DFORCES_BUILD_TESTING=ON -DFORCES_TEST_GLOB='test_mo_message.pf'
```

This is useful when iterating on one module or when you want to avoid configuring optional suites such as NetCDF- or optimization-related tests.

The `coverage` target additionally requires `lcov` to be available on `PATH`.

For a more complex project, prepared for unit-tests, documentation and modules, have a look at the [Fortran Template](https://git.ufz.de/chs/fortran-template).

## License

LGPLv3 (c) 2005-2026 CHS-Developers
