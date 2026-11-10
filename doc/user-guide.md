# Using FORCES {#forces_usage}

Install dependencies, configure the library and integrate it into a Fortran
application. Start with the [README quick start](../README.md); extension and
test workflows are in the [developer guide](development-guide.md).

Commands for building this checkout assume the repository root as working
directory unless an application directory is explicitly mentioned.

[TOC]

## Dependencies and installation

* Fortran compiler: We support [gfortran](https://gcc.gnu.org/fortran/), [nagfor](https://www.nag.com/content/nag-fortran-compiler), Intel classic/IntelLLVM ([ifort/ifx](https://www.intel.com/content/www/us/en/developer/tools/oneapi/overview.html)), and LLVM [flang](https://flang.llvm.org/)
* C compiler: Required for the operating-system and NetCDF backends.
* Build system: We support [make](https://www.gnu.org/software/make/) and [ninja](https://ninja-build.org/)
* [cmake](https://cmake.org/): Software for build automation
* [NetCDF-C](https://github.com/Unidata/netcdf-c): NetCDF I/O library
* (optional) [fypp](https://github.com/aradi/fypp): Fortran pre-processor written in Python

It is recommended to have a clean installation at a custom location
for a C compiler, a Fortran compiler and the NetCDF C library with consistent compilers.

You can install the dependencies in a [Conda](https://docs.conda.io/en/latest/)
environment using [Miniconda](https://docs.conda.io/en/latest/miniconda.html):

```bash
conda create -y --prefix ./forces_env
conda activate ./forces_env
conda config --add channels conda-forge
conda config --set channel_priority strict
conda install -y cmake make fortran-compiler libnetcdf fypp
```

Run the following commands from the repository root to build the library:

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build --parallel
```

The [README quick start](../README.md) adds a small executable to this build.
The integration examples below show how to link your own application.

## Build configuration

FORCES uses standard CMake inputs such as `CMAKE_BUILD_TYPE` and a small set of project-specific cache options:

* `FORCES_BUILD_TESTING`: build the FORCES pfUnit tests. Defaults to `OFF`.
* `FORCES_WITH_COVERAGE`: enable GNU coverage instrumentation for FORCES test builds.
* `FORCES_WITH_OpenMP`: enable OpenMP support.
* `FORCES_OpenMP_LINK_PUBLIC`: propagate OpenMP usage requirements to targets linking against FORCES. Requires `FORCES_WITH_OpenMP=ON` and defaults to `OFF`.
* `FORCES_WITH_MPI`: enable MPI support.
* `FORCES_WITH_NETCDF`: enable NetCDF support.
* `FORCES_WITH_OPTIMIZATION`: include optimization routines.
* `FORCES_ENABLE_NATIVE`: enable host-native tuning for `Release` and `RelWithDebInfo` builds.
* `FORCES_ENABLE_HEAP_ARRAYS`: place compiler-generated Fortran arrays on the heap with Intel Fortran or LLVM Flang. Defaults to `OFF`.
* `FORCES_EXE`: build a local executable linked against `forces`.

Typical configure commands:

```bash
# OpenMP build
cmake -B build -DCMAKE_BUILD_TYPE=Release -DFORCES_WITH_OpenMP=ON

# OpenMP build whose consumers also inherit OpenMP support
cmake -B build -DCMAKE_BUILD_TYPE=Release -DFORCES_WITH_OpenMP=ON -DFORCES_OpenMP_LINK_PUBLIC=ON

# local executable build
cmake -B build -DCMAKE_BUILD_TYPE=Debug -DFORCES_EXE=test.f90
```

For pFUnit test builds and GNU coverage reports, see the
[developer guide](development-guide.md). Coverage requires GNU Fortran, pFUnit
and LCOV 2.0 or newer.

## Integrate FORCES into an application

Applications link against the `forces` CMake target. For example, save this as `test.f90`:

```fortran
program test
  use mo_message, only : message
  implicit none
  call message("This is working!")
end program test
```

To fetch a released version with CPM, put this `CMakeLists.txt` next to `test.f90`:

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
CPMAddPackage("https://git.ufz.de/chs/forces.git@0.11.0")

# link dependencies
target_link_libraries(test forces)
```

This downloads [CPM](https://github.com/cpm-cmake/CPM.cmake) and fetches FORCES.
Choose a release tag or commit appropriate for your application.

Configure, build and run the application from its directory:

```bash
cmake -B build
cmake --build build --parallel
./build/test
```

The program prints:

```text
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

## Messages and logging

Applications use `mo_message` for concatenated messages, warnings and errors:

```fortran
use mo_message, only: message, warn_message, error_message, def_log_scope, use_log

! Configure saved settings once, before parallel work.
def_log_scope = "mhm"
call message("Reading input")                       ! [mhm] Reading input
call message("Unscoped output", scope="")            ! Unscoped output
call warn_message("Missing optional input")          ! [mhm] Missing optional input
call error_message("Recoverable error", raise=.false.)

! Enable explicitly when configuring mo_logging without mo_cli logger options.
use_log = .true.
call message("Reading input")                       ! [mhm][INFO] Reading input
```

`def_log_scope` is an owned, saved allocatable string. It initially remains
unallocated, which means root. An explicit `scope=` overrides it; an empty scope
or `"#"` selects root. Deallocating the default restores root. Do not temporarily
change saved settings around individual calls or mutate them during parallel work.

Plain output adds `[scope] ` for named scopes and has no scope filtering. It
honors `uni=`, `advance=` and `show=`. Logging remains opt-in (`use_log=.false.`
by default); when enabled, logger units and advancing records replace `uni=` and
`advance=`. The global `show_msg`, `show_warn` and `show_err` flags control only
plain output. Logging uses its own level/scope configuration independently of
these flags. An explicit `show=` is honored in both modes; `show=.true.` still
cannot bypass logger level/scope filters. Visibility never changes whether a
requested termination happens.

Normal messages log at INFO, nonterminating warnings at WARN, and nonterminating
errors at ERROR. A warning or error with `raise=.true.` logs at FATAL and
terminates with `ERROR STOP 1` in both output modes. Warnings default to `raise=.false.`; errors default to
`raise=.true.`. FATAL bypasses scope filters, while quiet levels and MPI rank
selection still apply. In particular, `log_level=0` suppresses FATAL text too.
The logging macros themselves do not terminate execution.

All log scopes are visible by default. `mo_logging:log_set_config` accepts
`log_scope_filter=` as a case-insensitive CSV list of glob patterns. Empty or
`"*"` permits all scopes; `"#"` selects only root; `"#,forces*"` selects root and
FORCES scopes. The CLI exposes the same selection through `--log-scope`; a bare
option enables all scopes. CLI help and version text always use plain stdout,
independent of diagnostic settings and logger redirection.

Constructing `cli_parser(add_logger_options=.true.)` adds `--log-file path`
alongside the verbosity, metadata and scope options without changing the message
mode. After help/version handling and argument validation, parsing configures the
logger, creates the requested file with `status='replace'`, and enables
`mo_message::use_log`. It routes all logger severities there.
Existing contents are overwritten; a missing or blank path is
a CLI error. File opens use `recl=65536_i4` for long formatted records. If opening
fails, the logger writes a warning directly to stderr and restores stdout/stderr
routing. Help/version requests do not create or truncate the requested file.
Parsing without logger options leaves `use_log` unchanged. CLI validation failures
use the existing message mode. Configure the parser and saved message settings
before parallel work.

The `mo_forces_message` wrappers default to the `forces` scope independently of
the application default; an explicit `scope=` overrides it. They share all other
options and saved settings with `mo_message` and own no mutable state. The
[developer guide](development-guide.md) describes their module-level imports.
Scientific output, optimization history/restart files and logger failure
fallback output retain their dedicated I/O paths.
Migrated legacy fatal diagnostics terminate with status 1, including cases
that previously used a character or bare STOP and could report success.
