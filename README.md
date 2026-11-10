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
CPMAddPackage("https://git.ufz.de/chs/forces.git@0.11.0")

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

! Enable after configuring mo_logging.
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
FORCES scopes.

Inside FORCES, import only the required routines **once at module level**:

```fortran
module mo_example
  use mo_forces_message, only: message, error_message
  implicit none
contains
  subroutine report_input()
    call message("Reading input")                   ! [forces] Reading input
  end subroutine report_input
end module mo_example
```

These wrappers default to `"forces"` independently of the application's
`def_log_scope`; an explicit scope still overrides that default. They share all
other settings with `mo_message` and do not own mutable state. Scientific output,
optimization history/restart files, and logger failure fallback output retain
their dedicated I/O paths.

## Build Configuration

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
# test build
cmake -B build -DCMAKE_BUILD_TYPE=Debug -DFORCES_BUILD_TESTING=ON

# OpenMP build
cmake -B build -DCMAKE_BUILD_TYPE=Release -DFORCES_BUILD_TESTING=ON -DFORCES_WITH_OpenMP=ON

# OpenMP build whose consumers also inherit OpenMP support
cmake -B build -DCMAKE_BUILD_TYPE=Release -DFORCES_WITH_OpenMP=ON -DFORCES_OpenMP_LINK_PUBLIC=ON

# GNU coverage build
cmake -B build -DCMAKE_BUILD_TYPE=Debug -DFORCES_BUILD_TESTING=ON -DFORCES_WITH_COVERAGE=ON

# local executable build
cmake -B build -DCMAKE_BUILD_TYPE=Debug -DFORCES_EXE=test.f90
```

`FORCES_WITH_COVERAGE` is only meaningful for test builds and requires GNU Fortran,
pFUnit, and LCOV 2.0 or newer.

## Dependencies and Requirements

* Fortran compiler: We support [gfortran](https://gcc.gnu.org/fortran/), [nagfor](https://www.nag.com/content/nag-fortran-compiler), Intel classic/IntelLLVM ([ifort/ifx](https://www.intel.com/content/www/us/en/developer/tools/oneapi/overview.html)), and LLVM [flang](https://flang.llvm.org/)
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

FORCES uses [pFUnit](https://github.com/Goddard-Fortran-Ecosystem/pFUnit) for its unit tests. When `FORCES_BUILD_TESTING=ON`, CMake will look for `PFUNIT`. If pFUnit is not available, ordinary test configuration still succeeds, but no FORCES tests are added. Coverage builds require pFUnit and fail configuration if it is missing.

You need to provide `pFUnit` to run the tests. It is unfortunately not available on conda yet. To compile it by hand in the current conda environment, you can do the following:

```bash
conda install -y m4
git clone https://github.com/Goddard-Fortran-Ecosystem/pFUnit.git
cmake -DCMAKE_BUILD_TYPE=Release -B pFUnit/build -S pFUnit
cmake --build pFUnit/build --parallel
cmake --install pFUnit/build --prefix $CONDA_PREFIX
rm -rf pFUnit
```

Then you can compile and run the tests in the same environment:

```bash
cmake -B build -DCMAKE_BUILD_TYPE=Debug -DFORCES_BUILD_TESTING=ON
cmake --build build --parallel
cmake --build build --target test
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

## Coverage reports

Coverage uses a serial GNU Debug build with pFUnit installed. Install LCOV 2.0 or
newer (`lcov` and `genhtml`) and Python's `lcov_cobertura` before collecting reports.
Use the `gcov` executable matching the compiler's GCC version; set CMake's
`GCOV_PATH` if the default differs.

```bash
cmake -S . -B build_coverage -DCMAKE_BUILD_TYPE=Debug \
  -DFORCES_BUILD_TESTING=ON -DFORCES_WITH_COVERAGE=ON \
  -DFORCES_WITH_OpenMP=OFF
cmake --build build_coverage --parallel 4
cmake --build build_coverage --target coverage
mkdir -p coverage
cp -a build_coverage/coverage/. coverage/
lcov --rc check_data_consistency=0 --add-tracefile build_coverage/coverage.info \
  --substitute "s#^(\Q$PWD\E|\Q$(pwd -P)\E)/##" --output-file coverage/coverage.info
lcov_cobertura coverage/coverage.info --base-dir . --output coverage/coverage.xml
```

The `coverage` target runs CTest and retains unexecuted compiled code in its LCOV
baseline. It excludes test sources, external files, and build-tree sources.
Function/line consistency checks are disabled because GNU Fortran can map distinct
functions to overlapping source lines; the original counters are retained.
Report normalization strips both logical and physical checkout prefixes while
preserving Fortran filename case. The commands stage HTML, LCOV, and Cobertura XML
under `coverage/`; open `coverage/index.html` for the HTML report.

GitLab's `coverage` job uses `chs-fortran-conda/0.6.0` and exposes the HTML report as
a CI artifact. Its Cobertura XML supplies merge-request diff annotations. The
`conditional_coverage` job repeats coverage with NetCDF and optimization disabled,
using a separate build directory and artifact archive. Only the full-feature job
supplies diff annotations. Coverage is not copied to GitLab Pages; artifact expiry
uses the GitLab instance default and the project's latest-successful-artifact policy.

To check the reduced-feature configuration locally:

```bash
cmake -S . -B build_coverage_conditional -DCMAKE_BUILD_TYPE=Debug \
  -DFORCES_BUILD_TESTING=ON -DFORCES_WITH_COVERAGE=ON \
  -DFORCES_WITH_OpenMP=OFF -DFORCES_WITH_NETCDF=OFF -DFORCES_WITH_OPTIMIZATION=OFF
cmake --build build_coverage_conditional --parallel 4
cmake --build build_coverage_conditional --target coverage
```

The GitHub Coverage workflow uses GNU Fortran 13 and builds serial pFUnit 4.12.0.
It retains coverage artifacts for 14 days and uploads LCOV through the Coveralls
GitHub Action using the built-in GitHub token. Enable the FORCES GitHub mirror in
Coveralls to receive reports; no additional repository token secret is needed.

## Development

Read the [developer guide](doc/development-guide.md) for module relationships,
generated-source workflows, data ownership, regression coverage, and documentation
conventions. [AGENTS.md](AGENTS.md) contains the repository-wide coding and workflow
rules used by developers and coding agents.

## Documentation

Generate HTML and LaTeX from the repository root with:

```bash
PROJECT_NUMBER="$(cat version.txt)" doxygen doc/doxygen.config
make -C latex
```

Open `html/index.html` to browse the manual or `latex/refman.pdf` for the PDF.
The Topics view organizes the API by purpose: core utilities and data structures,
spatial data and regridding, dates and time series, file I/O and operating system
support, statistics and numerical methods, and optimization and sampling.
Definitions live in `doc/directories.dox`; module headers declare their topic
with `\ingroup`, and topic pages link to relevant examples.

Documentation builds need a Fortran compiler for `doc/pre-proc.sh`, Graphviz, and
Doxygen 1.18.0, pinned in `doc/rtd-environment.yml`. Export `FC` to select a compiler;
the preprocessor otherwise uses `gfortran` when available. System TeX Live provides
the PDF tools, including `makeindex`, and the `newunicodechar`, `xltabular`, and
`tabularray` packages. The extra stylesheet `doc/forces_api_tables.sty` uses
`longtable` for API parameter tables and `xltabular` for simple documentation tables
to reduce PDF build time; merged cells, captions, and labels retain Doxygen's
standard renderer. The manual retains its source listings and index.

The vendored theme is [doxygen-awesome v2.5.0](https://github.com/jothepro/doxygen-awesome-css/releases/tag/v2.5.0),
copied unchanged under `doc/doxygen-awesome-css/`. The header displays the project
version from `version.txt`. Local and GitLab builds retain Doxygen's local search.

GitLab builds the checked-out commit once for HTML and LaTeX. PDF generation uses
Tectonic, TeX Live's `makeindex`, and a second Tectonic compilation to include the
index. Both Tectonic passes use `-Z continue-on-errors`; recoverable errors remain
in the logs, but unreadable graph PDFs may be omitted from the manual.
The job loads `GCC/13.3.0`, `texlive/20250308`, and
`chs-fortran-conda/0.6.0`, taking `FC` from the Conda module. Graphviz rendering is
limited to four workers. CI sets `CAIRO_DEBUG_PDF=1` during Doxygen generation to
disable Cairo's PDF object streams and avoid the intermittent Tectonic read
failure. HTML, `forces_doc.pdf`, and Doxygen/PDF logs (including
`latex/refman.log`) are exposed as job artifacts, including on failure.
GitLab Pages publishes the `main` branch HTML at its root; the former `stable/`,
`latest/`, and `coverage/` subdirectories are no longer published.

[Read the Docs](https://forces.readthedocs.io/) provides versioned HTML/PDF manuals
and hosted search. Its navigation dropdown offers version selection and the
current version's PDF download on desktop and mobile, preserving the current page
and anchor when switching versions. Connect the FORCES repository to the RTD
project with slug `forces` and activate the desired branches/tags. Disable the
floating flyout under **Settings > Addons > Flyout menu**; the dropdown still
receives its Addons data.

RTD runs the environment, Doxygen, PDF, and publishing phases separately through
`doc/readthedocs_build.sh`. With no argument, the script runs all phases, including
creating the Conda environment. Set `FORCES_DOCS_ENV` to override its default name
`forces-docs`. PDF generation uses system `latexmk -pdf` to schedule compiler,
bibliography, and index passes. The complete log is `doxygen_latex_make.log`; the
last 80 lines are printed on failure. The script stages HTML and `pdf/forces.pdf`
under `$READTHEDOCS_OUTPUT` (or `_readthedocs` locally). Run individual phases with:

```bash
bash doc/readthedocs_build.sh environment
bash doc/readthedocs_build.sh doxygen
bash doc/readthedocs_build.sh pdf
bash doc/readthedocs_build.sh publish
```

For a more complex project, prepared for unit-tests, documentation and modules, have a look at the [Fortran Template](https://git.ufz.de/chs/fortran-template).

## License

LGPLv3 (c) 2005-2026 CHS-Developers
