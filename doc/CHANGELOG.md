# Changelog

All notable changes to **FORCES** will be documented in this file.


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
