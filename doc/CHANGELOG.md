# Changelog

[TOC]

All notable changes to **FORCES** will be documented in this file.

## v0.8 - 2025-11
- See the git [diff](https://git.ufz.de/chs/forces/-/compare/v0.7.1...v0.8.0) for details.

### Enhancements and Changes
- `mo_dag` ([100](https://git.ufz.de/chs/forces/-/merge_requests/100))
  - Refactored the DAG module around a new abstract dag_base and added a branching specialization for river-style graphs, aligning terminology on sources/targets, adding traversal handlers, adjacency accessors, and cleanup (destroy) utilities in src/mo_dag.f90.
  - Improved ordering helpers with an explicit order_t%to_root flag, a new order%sort, and a fixed order%reverse for odd level counts; root/leaf level sorts now parallelize with OpenMP and prefix sums for faster scheduling.
  - Added an OpenMP-enabled prefix_sum helper for i8 with shift/start heuristics plus example examples/04_utils/01_prefix_sum.f90 and coverage in src/pf_tests/test_mo_utils.pf; DAG code now uses it for offset building.
  - Updated DAG examples (including new examples/02_dag/02_branching.f90) and expanded src/pf_tests/test_mo_dag.pf to cover branching, traversal, and order direction.
- `mo_grid_io` ([96](https://git.ufz.de/chs/forces/-/merge_requests/96))
  - rewrote mo_grid_io in FYPP so all kind/rank combinations are emitted from a single template, which also let us extend the codebase to layered variables without duplicating logic
  - layered support now includes metadata propagation, z-axis creation, per-layer buffers, and dedicated read/write paths in both output and input datasets
  - introduced id-based update/read APIs with cached var_index helpers, reused in the existing name-based entry points
  - normal reads now call the new non-allocating `readInto` from `mo_netcdf`
  - layered writes stream each slice with explicit start/cnt
  - input_time_index uses a bounds-checked binary search
- `mo_grid` and `mo_grid_scaler`: OpenMP optimizations ([97](https://git.ufz.de/chs/forces/-/merge_requests/97))
  - Reworked src/mo_grid.F90 for performance and API cleanup: removed the unused layered_grid_t (breaking), introduced a data_t container plus mask_from_var/data_from_var helpers to read data without extra copies, added gen_grid/check_factor/coarse_ij utilities, optional mask/area NetCDF outputs, new area-count upscaling mode, OpenMP pack/unpack implementations for all integer kinds, faster mask/cell-id bookkeeping, and periodic/grid-generation fixes.
  - Upgraded src/mo_grid_scaler.f90 for OpenMP-aware grid mapping: validated scaling factors, added cacheable coarse bounds/indices APIs, new weight modes (area vs. count) and coarse_weights, a no-scaling fast path for equal resolutions, generalized downscale_nearest/split over packed/unpacked inputs, and added minloc/maxloc helpers; init now accepts optional weight_mode/cache_bounds (API change).
  - Updated examples and pfunit suites (examples/01_grids/01_regridding.f90, src/pf_tests/test_mo_grid.pf, src/pf_tests/test_mo_grid_scaler.pf) to cover new data container flows, pack/unpack variants, weight-count paths, extra downscaling modes, and tightened tolerances.
- `mo_logging` ([99](https://git.ufz.de/chs/forces/-/merge_requests/99), [101](https://git.ufz.de/chs/forces/-/merge_requests/101))
  - Simplified `mo_logging`:
    - Strip color/terminal detection from `mo_logging`, remove hostname/formatting helpers, and rewrite `logl` to always emit level tags (new `log_text` macro disables them case-by-case) with allocatable strings and Windows-aware `strip_path`.
    - introduced scope tags (`log_scope_filter`, `scope_*` macros)
    - added CSV filtering for tags (with simple globbing support)
    - scope names print before [LEVEL]
    - Propagate `-ffree-line-length-none` as a PUBLIC flag for gfortran targets via `src/CMakeLists.txt` so long log strings compile cleanly.
    - update `mo_cli` options/APIs to match new logging features
    - remove `mo_logging` dependencies from `mo_message`, and drop unused formatting arguments.
  - Expanded `logging.h`:
    - Update macros (`log_write`, `log_plain`, etc.) plus CLI options/APIs to match, remove `mo_logging` dependencies from `mo_message`, and drop unused formatting arguments.
    - `scope_*` macros
    - fine-level aliases for subtrace (`log_fine` family)
    - disabled debug and trace logging now produce write lines starting with "if(.false.)" to let the compiler optimizer remove them (NAG and maybe other pre-processors don't support comment writing with macros)
  - updated logging example (`examples/03_log/01_logging_cli.F90`) with scope logging examples
- `mo_cli` ([101](https://git.ufz.de/chs/forces/-/merge_requests/101))
  - add `--log-scope` to logger options and add a default scope argument
  - `--` separator handling
  - allow empty argument in options
- `mo_os` ([95](https://git.ufz.de/chs/forces/-/merge_requests/95))
  - add `path_cwd`
  - also replace single '\' in path_to_posix
- `mo_glob` ([102](https://git.ufz.de/chs/forces/-/merge_requests/102))
  - Minimal globbing implementation supporting wildcards `"?"` and "`*`"

## v0.7.1 - 2025-09
- See the git [diff](https://git.ufz.de/chs/forces/-/compare/v0.7.0...v0.7.1) for details.

### Bugfixes
- GCC15: procedure pointers should be called with positional arguments ([93](https://git.ufz.de/chs/forces/-/merge_requests/93))
- doc: make all examples visible in the documentation ([92](https://git.ufz.de/chs/forces/-/merge_requests/92))


## v0.7.0 - 2025-09
- See the git [diff](https://git.ufz.de/chs/forces/-/compare/v0.6.1...v0.7.0) for details.

### Enhancements
- added examples folder ([80](https://git.ufz.de/chs/forces/-/merge_requests/80))
- add `mo_dag`: a Directed Acyclic Graph implementation motivated by the routing module of mHM derived from https://github.com/jacobwilliams/daglib ([89](https://git.ufz.de/chs/forces/-/merge_requests/89))
- add `mo_list`: a generic linked list implementation derived from https://github.com/jacobwilliams/flist ([88](https://git.ufz.de/chs/forces/-/merge_requests/88))
- reworked optimization routines: added `mo_optimizee` to provide an abstract base class for optimization targets ([81](https://git.ufz.de/chs/forces/-/merge_requests/81), [83](https://git.ufz.de/chs/forces/-/merge_requests/83), [85](https://git.ufz.de/chs/forces/-/merge_requests/85))
- add `mo_polynomial`: module to deal with polynomials like evaluation, root finding or derivation ([82](https://git.ufz.de/chs/forces/-/merge_requests/82))
- add `mo_grid`: ([80](https://git.ufz.de/chs/forces/-/merge_requests/80), [89](https://git.ufz.de/chs/forces/-/merge_requests/89))
  - added `grid_t` class to hold a uniform grid definition
    - single cell-size
    - Cartesian and Spherical grid support
    - aux-coordinates support
    - support for periodic spherical grids (along lon-axis)
    - NetCDF in/out routines
    - ascii grid in/out routines
    - aux-coords upscaling support (lat-lon coordinates estimated from fine grid by averaging)
    - derive coarse/fine grids based on target resolution or scaling factor
   - supports flipping y-axis (top-down or bottom-up)
- add `mo_grid_io`: ([80](https://git.ufz.de/chs/forces/-/merge_requests/80))
  - handlers for netcdf files based on a grid (`input_dataset`, `output_dataset`)
  - uses `datetime` to select data on time dimension
- add `mo_grid_scaler`: ([80](https://git.ufz.de/chs/forces/-/merge_requests/80))
  - regridder implementation based on grid type
  - can be used for up and down scaling
  - wide range of scaling operators provided
- `mo_message`: ([80](https://git.ufz.de/chs/forces/-/merge_requests/80))
  - add `warn_message` as 3rd option
- `mo_utils`: ([80](https://git.ufz.de/chs/forces/-/merge_requests/80), [89](https://git.ufz.de/chs/forces/-/merge_requests/89))
  - add `flip` and `flipped` for sp/dp/i4/i8 for 1-4 dims
  - add `optval` as helper function for most data types to set default values from optional values (`internal_val = optval(val, default)`)
  - add support for `i8` integers in swap
- `mo_datetime`: ([80](https://git.ufz.de/chs/forces/-/merge_requests/80), [86](https://git.ufz.de/chs/forces/-/merge_requests/86), [89](https://git.ufz.de/chs/forces/-/merge_requests/89))
  - added `decode_cf_time_units`: convert cf time units to a reference `datatime` and a `timedelta` (`"hours since 1990-06-01 12:00"`)
  - added `delta_from_string`: cf string for time delta (like `"hours"`) to `timedelta` (like `timedelta(hours=1)`)
  - add `<step>_start`, `next_new_<step>` and `previous_new_<step>` where step can be year/month/week/day/hour/minute
  - add `add` and `sub` for in-place addition and subtraction of `timedelta`
  - faster date/time comparison (no intermediate type instances)
  - better conversion from string
  - allow 'T' as time separator
  - allow missing entries (e.g. no second given)
- CI/CMake: ([80](https://git.ufz.de/chs/forces/-/merge_requests/80), [89](https://git.ufz.de/chs/forces/-/merge_requests/89))
  - added language `C` to CMake project to catch error when NetCDF is linked against HDF5 (e.g. libnetcdf 4.9.3 on conda forge ([Pull Request](https://github.com/conda-forge/libnetcdf-feedstock/pull/207))) and triggers the FindHDF5 script of CMake which tries to compile a test C program
  - cleaned up compile flags in CMake
  - added explicit support for IntelLLVM
  - add debug jobs for all intel compilers in CI
  - added `FORCES_EXE` option to CMake to compile a singe executable linked against forces

### Changes
- `mo_orderpack`: use `i8` for indexing variables to be able to use huge arrays ([90](https://git.ufz.de/chs/forces/-/merge_requests/90))
- `mo_io`: use plain `read(u,*)` in `loadtxt` to read floats ([89](https://git.ufz.de/chs/forces/-/merge_requests/89))

### Bugfixes
- `mo_logging`: set stderr as default unit ([87](https://git.ufz.de/chs/forces/-/merge_requests/87))


## v0.6.1 - 2024-03
- See the git [diff](https://git.ufz.de/chs/forces/-/compare/v0.6.0...v0.6.1) for details.

### Enhancements
- updates related to the merge of mLM into mHM ([78](https://git.ufz.de/chs/forces/-/merge_requests/78))
  - `mo_temporal_aggregation`: add day2mon_sum; make mon_avg intent(out)
  - `mo_timer`: increase max_timers to 500
  - `mo_optimization_utils`: added reservoir/ lake volume as optional argument to eval_interface
  - `mo_message`: cleanup; add t11..t16 optionals
  - `mo_dds`: Added 'dds_results.out.current' output file
  - `mo_constants`: add ONETHIRD
- updated the CI workflow ([77](https://git.ufz.de/chs/forces/-/merge_requests/77))
  - added doc template
  - refactored coverage job
  - removed unsupported toolchains
  - added new toolchains for intel and conda


## v0.6.0 - 2023-07
- See the git [diff](https://git.ufz.de/chs/forces/-/compare/v0.5.0...v0.6.0) for details.

### Enhancements
- added `mo_datetime` ([76](https://git.ufz.de/chs/forces/-/merge_requests/76))
  - This module provides four types to deal with date and time:
    1. `puredate` : containing year, month and day
    2. `puretime` : containing hour, minute and second
    3. `datetime` : combination of date and time
    4. `timedelta` : difference between two datetimes (or dates) in days and (sub-day) seconds
  - these type can be used in arithmetic operations (+, -, *, /) and can be compared (<, >, <=, >=, ==, /=) where it makes sense


## v0.5.0 - 2023-03
- See the git [diff](https://git.ufz.de/chs/forces/-/compare/v0.4.0...v0.5.0) for details.

### Enhancements
- added capability of conditional compilation ([60](https://git.ufz.de/chs/forces/-/merge_requests/60)). Two cmake options were added, that are enabled by default:
  - `-DFORCES_WITH_OPTIMIZATION=[ON|OFF]`: include all modules used for optimization (sce, mcmc, anneal, dds, errormeasures, likelihood, opt_functions, cost, optimization_types, optimization_utils)
  - `-DFORCES_WITH_NETCDF=[ON|OFF]`: include all modules using NetCDF (netcdf, ncwrite, ncread)
    - Here, `mo_mcmc` throws an error if you want to have a `tmp_file` written out.
    - This option can be used to drop the NetCDF dependency, if netcdf io is not needed for your program

- mo_os ([73](https://git.ufz.de/chs/forces/-/merge_requests/73))
  - added `change_dir` to have a compiler independent `chdir` routine
  - added `get_cwd` to have a compiler independent `getcwd` routine
  - added path constants `curdir`, `pardir`, `sep`, `extsep`, `linesep` and `devnull`
  - added `path_dirname` and `path_basename` (`path_split`'s head and tail)
  - added `path_root` and `path_ext` (`path_splitext`'s root and ext)
  - added `path_stem` (final path component without suffix)
  - added `path_isabs` and `path_isroot` checking functions
  - added `path_normpath` to normalize path
  - added `path_abspath` to get absolute path
  - added `path_parts` subroutine to split path into all components
  - added `path_as_posix` function to replace `\\` with `/` in given path

- mo_string_utils ([73](https://git.ufz.de/chs/forces/-/merge_requests/73))
  - added optional `strip` argument to `startswith`
  - added `endswith` function

### changes

- mo_os ([73](https://git.ufz.de/chs/forces/-/merge_requests/73))
  - `path_splitext`:
    - don't check for folder
    - ignore leading dots in tail of path
    - made root and ext optional
  - `path_split`:
    - remove trailing '/' from head unleass it is root (e.g. '/' or '//' or '///' and so on)
    - made head and tail optional
  - converted `path_[exists|isfile|isdir]` to lowlevel functions
    - original subroutines are now called `check_path_[exists|isfile|isdir]`


## v0.4.0 - 2023-01
- See the git [diff](https://git.ufz.de/chs/forces/-/compare/v0.3.2...v0.4.0) for details.

### Enhancements
- mo_poly: added mo_poly.fypp replacement for mo_poly functions ([55](https://git.ufz.de/chs/forces/-/merge_requests/55))
- mo_poly: added new routines and tests ([55](https://git.ufz.de/chs/forces/-/merge_requests/55))
  - `orientpoly` (calculate orientation of coords in polygon),
  - `mod_pole` (modify coords of grid to include poles on Cartesian coord system) and
  - `mod_shift` (shift longitude values by 180 degrees)
- documentation updates ([58](https://git.ufz.de/chs/forces/-/merge_requests/58), [66](https://git.ufz.de/chs/forces/-/merge_requests/66))
- license clarifications ([66](https://git.ufz.de/chs/forces/-/merge_requests/66))
- mo_message enhancements ([40](https://git.ufz.de/chs/forces/-/merge_requests/40))
  - add global switches in `mo_message` to turn of printing with `message` and `error_message`
    - `SHOW_MSG = .true.`
    - `SHOW_ERR = .true.`
    - `message` and `error_message` got a new argument `show`, defaulting to the global setting
  - add switch to `error_message` to control raising stop error:
    - `raise=[.true.|.false.]` with `.true.` by default
- mo_sentinel: added module for sentinel handling ([64](https://git.ufz.de/chs/forces/-/merge_requests/64))
  - routines to set, get and check sentinels for all types
    ```fortran
    value = get_sentinel(mold)
    call set_sentinel(value)
    state = check_sentinel(value)
    ```
- mo_logging: added logging module based on the [flogging](https://github.com/DaanVanVugt/flogging) project of Daan van Vugt ([63](https://git.ufz.de/chs/forces/-/merge_requests/63))
  - integration with mo_cli to set log-level from command line

### Changes
- mo_os refactoring ([72](https://git.ufz.de/chs/forces/-/merge_requests/72))
  - changed signatures `(path, answer, verbose, raise)` for path_exists, path_isfile and path_isdir
  - respect show_msg and show_err from mo_message
  - simplify inquire logic

### Bugfixes
- mo_kernel: fix for openmp usage (only use openmp if compiled with openmp support) ([59](https://git.ufz.de/chs/forces/-/merge_requests/59))


## v0.3.2 - 2022-06
See the git [diff](https://git.ufz.de/chs/forces/-/compare/v0.3.1...v0.3.2) for details.

### Bugfixes
- CMake: prevent altering CMAKE_MODULE_PATH to not interfere with down-stream packages ([57](https://git.ufz.de/chs/forces/-/merge_requests/57))


## v0.3.1 - 2022-05
See the git [diff](https://git.ufz.de/chs/forces/-/compare/v0.3.0...v0.3.1) for details.

### Enhancements
- added linking example to README ([aa7ebf31](https://git.ufz.de/chs/forces/-/commit/aa7ebf3173c1529bd07a679839669574ee06b2bd))
- added Changelog ([50](https://git.ufz.de/chs/forces/-/merge_requests/50))

### Bugfixes
- CMake: fix source collection for mo_netcdf if preprocessed ([c2430f5b](https://git.ufz.de/chs/forces/-/commit/c2430f5ba234449f475d9a287239d84171a6fb41))


## v0.3.0 - 2022-04
See the git [diff](https://git.ufz.de/chs/forces/-/compare/v0.2.0...v0.3.0) for details.

### Enhancements
- mo_utils: added is_close and short circuits for exact equality check ([46](https://git.ufz.de/chs/forces/-/merge_requests/46))
- mo_eckhardt_filter: add Eckhardt filter and BFI optimization option ([47](https://git.ufz.de/chs/forces/-/merge_requests/47))

### Changes
- update dependencies (cmake-fortran-scripts v1.6, doxygen-awesome v2.0.3) ([49](https://git.ufz.de/chs/forces/-/merge_requests/49))

### Bugfixes
- resolve many compiler warnings ([43](https://git.ufz.de/chs/forces/-/merge_requests/43))


## v0.2.0 - 2022-01
See the git [diff](https://git.ufz.de/chs/forces/-/compare/v0.1.1...v0.2.0) for details.

### Enhancements
- mo_kind: add option to use iso_fortran_env; added qp ([44](https://git.ufz.de/chs/forces/-/merge_requests/44))
- Update documentation strings for modules and functions ([40](https://git.ufz.de/chs/forces/-/merge_requests/40))
- include SMI dependencies ([42](https://git.ufz.de/chs/forces/-/merge_requests/42))
- mo_netcdf: feature to read all attributes of a given netcdf type ([38](https://git.ufz.de/chs/forces/-/merge_requests/38))
- mo_utils: implement ieee arithmetic ([37](https://git.ufz.de/chs/forces/-/merge_requests/37))

### Changes
- Remove mo_finish ([36](https://git.ufz.de/chs/forces/-/merge_requests/36))
- convert all tests to pFUnit tests ([32](https://git.ufz.de/chs/forces/-/merge_requests/32))

### Bugfixes
- CI: Minor fixes ([45](https://git.ufz.de/chs/forces/-/merge_requests/45))


## v0.1.1 - 2021-11
See the git [diff](https://git.ufz.de/chs/forces/-/compare/v0.1.0...v0.1.1) for details.

### Enhancements
- CMake: add traceback flag for debug mode in gfortran and NAG
- add mo_cli: module for building command line interfaces
- add coverage site

### Changes
- FPP/CPP: rename files to be pre-processed to `*.F90`, so pre-processing is done automatically

### Bugfixes
- refactored mo_message.F90


## v0.1.0 - 2021-05
See the git [diff](https://git.ufz.de/chs/forces/-/commits/v0.1.0/) for details.

First version of the lightweight fork of the CHS Fortran library.

### Enhancements
- mo_message: added `error_message`
- added mo_os: file system interactions
- added sorting routines for character arrays
- added setCoordinate routine to mo_netcdf.fypp
- updated CMake workflow
- added version.txt and version_date.txt like for mHM
- added first pfUnit tests
- added `cmake` and `hpc-module-loads` folder as subrepos from CHS repos
- added doxygen documentation
- remove pre-processor directives for unsupported compilers

### Changes
- removed redundant functions in mo_utils.f90 (greaterthan, lessthan)
- removed numerical recipes code
- adopted `mo_netcdf`
- added i8 write_dimension routine to allow for writing i8 weights to netcdf file
- extended mo_netcdf's setDimension functionality to also account for boundaries and attributes directly
- merged folders common and common_mHM_mRM
- based all eps and nodata references on mo_constants
- mo_restart now handles dimension bounds in a more correct way
- moved `nodata_dp` and `eps_dp` to `mo_constants`
- merged lib from mHM and MPR and mo_netcdf
- adapted license and readme file
