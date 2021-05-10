# Cmake Fortran Scripts

Cmake scripts for Fortran Projects at CHS

## Cmake Modules

Modules under `cmake-modules` provide different functions and macros
to be used in `CMakeLists.txt` files.

### `checkfortranpreprocessor.cmake`
- `get_preproc_flag`: checks for the pre-processor flag of the current compiler (`-fpp` or `-cpp`)
- `cpp_definitions`: adds a compile definition and a corresponding cmake option

Can be included with:
```cmake
include(checkfortranpreprocessor)
```

### `CodeCoverage.cmake`
Taken from: https://github.com/bilke/cmake-modules/blob/master/CodeCoverage.cmake

Can be included and set up with:
```cmake
include(CodeCoverage)
append_coverage_compiler_flags()
SETUP_TARGET_FOR_COVERAGE_LCOV(
  NAME PROJECT_coverage_CI
  EXECUTABLE PROJECT
  DEPENDENCIES PROJECT
  GENHTML_ARGS -t "PROJECT coverage"
)
```

### `compileoptions.cmake`
Adds cmake compile options
- `CMAKE_BUILD_MODULE_SYSTEM_INDEPENDENT`: setting r_path
- `CMAKE_NETCDF_DIR`: separate netcdf path
- `CMAKE_WITH_MPI`: use MPI
- `CMAKE_WITH_OpenMP`: use OpenMP
- `CMAKE_WITH_LAPACK`: use LAPACK bindings
- `CMAKE_WITH_COVERAGE`: Coverage calculation
- will also search for `MPI`, `OpenMP` and `LAPACK` if the option are `ON`

Can be included with:
```cmake
include(compileoptions)
```

### `FindNetCDF.cmake`
Find NetCDF package.

Can be used with:
```cmake
find_package(NetCDF REQUIRED)
```

### `utils.cmake`
Based on: https://github.com/fortran-lang/stdlib/blob/master/cmake/stdlib.cmake
- handles `fypp` pre-processor

Can be included with:
```cmake
include(utils)
```

### `version.cmake`
Provides a function to read version and date from a given files `version.txt` and `version-date.txt`.

Can be included and used with (`PROJECT_VER_DEV` will hold the develop version string):
```cmake
include(version)
get_version(PROJECT_VER PROJECT_VER_DEV PROJECT_DATE)
```

## Cmake Cache Files

Cache files provide templates for specific cmake setups.
Cmake can be controlled with such a file (`exampleFile`) in the following way
```bash
cmake -C exampleFile ..
```

Have a look at the example files in `cmake-cache-files` taken from `mHM`.
