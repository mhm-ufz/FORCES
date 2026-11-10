# Developing FORCES {#forces_development}

FORCES provides reusable Fortran modules for environmental applications, with C
backends for operating system operations and NetCDF interoperability. Development
centres on small public APIs, explicit data ownership, and portable builds. This
guide explains where to extend the library and how to check a change. The root
[repository instructions](../AGENTS.md) define the coding and workflow rules;
the [README](../README.md) covers installation, build options, and publishing.

[TOC]

## Find the relevant modules

The library builds as the `forces` CMake target. Applications link against that
target; `FORCES_EXE` optionally builds a selected program as `main`.

| Location | Purpose |
| --- | --- |
| `src/mo_*.f90`, `src/mo_*.F90` | Fortran modules, with uppercase `.F90` enabling compiler preprocessing. |
| `src/*.fypp`, `src/common.fypp` | Authoritative templates and shared kind/rank definitions for generated modules. |
| `src/*_c.c`, `src/mo_os_backend.c` | C interoperability and platform backends. |
| `include/logging.h` | Application logging macros used with `mo_logging`. |
| `src/pf_tests/`, `src/pf_tests/files/` | pFUnit suites and input fixtures. |
| `examples/` | Standalone programs combining the library's APIs. |
| `doc/` | Doxygen pages, topics, directory descriptions, theme, and HTML/PDF build support. |

Topics organize the modules by functionality while sources remain together in
`src/`:

| Topic | Useful starting points |
| --- | --- |
| [Core utilities and data structures](@ref f_core) | `mo_kind`, `mo_utils`, `mo_dag`, `mo_cli`, `mo_logging`. |
| [Spatial data and regridding](@ref f_spatial) | `mo_grid`, `mo_grid_scaler`, `mo_points`, `mo_spatial_index`. |
| [Dates and time series](@ref f_time) | `mo_datetime`, `mo_timeseries`, `mo_temporal_aggregation`. |
| [File I/O and operating system support](@ref f_io) | `mo_io`, `mo_netcdf`, `mo_grid_io`, `mo_points_io`, `mo_os`. |
| [Statistics and numerical methods](@ref f_statistics) | `mo_moment`, `mo_errormeasures`, `mo_kernel`, `mo_polynomial`. |
| [Optimization and sampling](@ref f_optimization) | `mo_optimizee`, `mo_opt_eval_utils`, `mo_sce`, `mo_mcmc`. |

Read the affected module, its callers, its tests, and a related example before
choosing a signature. `mo_grid_helper.F90` shows stateless helpers;
`mo_spatial_index.f90` shows a type with cached state. Keep compatibility and
scientific conventions explicit when changing either style of API.

## Add or extend public functionality

Use a `mo_*` module under `src/`, keep implementation details private, and export
the intended procedures, types, and constants explicitly. Use `mo_kind` kinds in
public numeric arguments and results. Prefer existing abstractions and helpers
for messages, strings, datasets, grids, and time axes.

Follow a nearby module's header, preserving actual authorship and donor licenses.
Its `\file` name must match the source filename and case; `\copydoc` must name the
module. Add the appropriate `\ingroup` immediately before the module declaration.
Document a procedure's meaning, units, optional defaults, shapes, and error
contract, with argument descriptions beside their declarations. Explain who owns
any persistent storage and when it becomes invalid.

New ordinary `mo_*.f90`/`mo_*.F90` modules are discovered by
`src/CMakeLists.txt`; reconfigure CMake to pick them up. Register new C helpers
with `target_sources`. A feature controlled by NetCDF or optimization options
also needs source exclusions and corresponding test exclusions. Keep changes to
those lists in the same patch as the new source.

For C bindings, use interoperable kinds and `iso_c_binding`; define buffer
capacity, null termination, and error/status handling explicitly. NetCDF APIs
build on NetCDF-C through `mo_netcdf_wrapper` and its `ncw_*` procedures. Follow
the existing dataset layers when adding higher-level I/O.

## Work with generated modules

If a module has a `.fypp` source, edit that template. The checked-in `.f90` is a
generated fallback used when Fypp is unavailable and is also read by Doxygen.
Commit the template and its regenerated output together.

The normal build uses `-DVERSION90`; `src/common.fypp` supplies kinds and rank
limits. Regenerate a changed template from the repository root, for example:

```bash
fypp -DVERSION90 src/mo_grid_io.fypp src/mo_grid_io.f90
```

Use the same command for each affected template. If changing shared definitions
in `common.fypp`, regenerate all template-backed modules. Review the output
diff for unrelated changes. Builds using `CMAKE_MAXIMUM_RANK` pass that rank as
`MAXRANK` instead; retain the checked-in fallback's normal configuration unless
intentionally changing it.

New templates must be added to the Fypp source list and the fallback-source
exclusion lists in `src/CMakeLists.txt`. CMake writes its generated sources into
the build tree, so an ordinary build does not refresh the tracked fallback files.

## Preserve data and ownership contracts

A `grid_t` owns geometry, orientation, and an active-cell mask. Gridded values
may be full `(nx, ny)` arrays or packed active cells of length `grid%ncells`.
Packing follows Fortran `(x, y)` order with the first index varying fastest;
different masks give different packed indices. Use the grid helpers to retain
that mapping:

```fortran
! field_xy and packed are already allocated for the grid.
call grid%pack_into(field_xy, packed)
call grid%unpack_into(packed, field_xy)
```

Grid derivation accepts `llcorner=[xll, yll]` in grid coordinates and
`dims=[nx, ny]` in target cell counts on every `gen_*grid` and `derive_*grid`
entrypoint. The origin defaults to the source lower-left corner, and displacement
must remain an integer number of fine cells. Offsets are derived internally for
index mapping. For example:

```fortran
call source%gen_grid(target, target_resolution=resolution, &
  llcorner=[xll, yll], dims=[nx, ny], estimate_aux=.false.)
```

With `dims`, the requested rectangle is used directly: source cells may be cropped,
and target cells are active wherever they overlap active source cells. Unsupported
padding stays inactive. Without `dims`, derivation keeps the requested/default
origin and chooses the smallest dimensions covering the complete source rectangle.
An origin excluding source cells is rejected.

Use `source%mask_extent` to determine dimensions covering complete active source
cells before deriving a grid:

```fortran
! Keep the source lower-left corner.
call source%mask_extent(resolution, dims)

! Find the smallest rectangle around the active mask.
call source%mask_extent(resolution, dims, llcorner=corner)
call source%gen_grid(target, target_resolution=resolution, llcorner=corner, dims=dims)

! Keep a specific lower-left corner.
call source%mask_extent(resolution, dims, origin=corner)
```

Requesting `llcorner` without `origin` selects and returns the exact geographic
lower-left corner of the active mask bounding box, independent of row storage
direction. Supplying neither retains the source origin. Supplying `origin` fixes
the corner; it is also returned through `llcorner` if requested. Dimensions round
up to whole target cells from that corner. An empty mask uses full source bounds.
The helper validates integer resolution ratios, fine-cell alignment, coverage of
all required source cells, and representable dimensions; optional `tol` has the
same meaning as in grid derivation. It does not constrain auxiliary estimation.
For nonperiodic sources, the automatically chosen mask rectangle supports
auxiliary estimation, including partial coarse edge cells. A fixed origin can
introduce unsupported padding; periodic longitude restrictions are described below.

Use `estimate_aux=.false.` to allow padding with no source overlap. With auxiliary
estimation enabled, every coarse output cell must overlap the source rectangle,
and the entire fine output rectangle must lie inside the source. Cropping is
supported when source coordinates cover the output.

Periodic longitude seam wrapping is not implemented by the scaler or grid
derivation. A periodic source can supply a shifted regional target inside its
stored longitude interval; matching periodic states are not required. The scaler
rejects active target cells whose complete longitude footprint extends outside
that interval, including partially overlapping coarse cells, even with
`fill_missing=.true.`. Inactive regional target padding is permitted. Two global
periodic grids must share their longitude interval.

Derivation from a periodic source rejects any requested rectangle extending
outside the source longitude interval, including inactive padding and
`estimate_aux=.false.`. Latitude padding follows the ordinary rules. Without
`dims`, the output retains the complete 360-degree longitude extent and the
resolution must be compatible with that extent. Explicit dimensions may select a
regional rectangle inside the interval. `mask_extent` still returns ordinary
bounding-box geometry; target-cell rounding may yield longitude padding rejected
by derivation from a periodic source.

Point values follow the point set's ordering, and a spatial mapping must preserve
the correspondence between coordinates, IDs, and values. A regridder or index
caches information derived from its source and destination support; rebuild it
when that support changes. Keep owning grids and point sets alive for as long as
another object borrows them.

Time-series code distinguishes instants from intervals and interval means from
interval sums. Preserve time units, bounds, timestamp location, and nodata
handling when changing temporal I/O or resampling. Read the corresponding tests
before modifying those conversions.

FORCES diagnostics use one module-level `use mo_forces_message, only: ...` import
per consuming module; remove repeated procedure-level imports when extending a
module. The wrappers supply the `forces` scope without changing application
message defaults. Keep the dependency direction from foundations through
`mo_logging`, `mo_message` and `mo_forces_message` to diagnostic callers. Logger
internals must not depend on higher-level string, append, or OS utilities.
Plain CLI presentation and scientific data-file output retain dedicated writers.
Keep all parts of a diagnostic at the same severity, including follow-up details;
combine a fatal explanation into one terminating call when possible. Ordinary
conditional message calls use `if (condition) &` followed by `call ...` on the
next line. Logging macros require block IF guards because their expansion
contains its own IF. Prefer source lines within 132 columns and break outside
character literals; use complete concatenated literals if a string must be split.

Follow each type's existing initialization, reset, close, and destruction API.
Do not reallocate storage while another object points into it. A copied derived
type with pointer components retains aliases to the same targets. Prepare
reusable scratch outside repeated numerical work and keep thread-called kernels
reentrant. Preserve units and numerical order unless the change explicitly
requires different scientific behavior.

## Add regression coverage

Place focused pFUnit suites in `src/pf_tests/`, using a name such as
`test_mo_feature.pf`. CMake derives the CTest name from the filename and discovers
suites through `FORCES_TEST_GLOB`; reconfigure when adding a suite. Extend the
existing optional-feature exclusions when appropriate.

Check meaningful behavior: supported kinds and ranks, boundary conditions,
optional arguments, masks and ordering, nodata, repeated initialization, and
ownership lifetimes affected by the change. Numerical refactors need comparison
with pre-change output or an independent reference and justified tolerances.
For a fatal error path, check termination and the expected diagnostic in an
isolated process. Keep pFUnit assertions on a single physical line.

Compilation and test execution require an explicit user request in an agent
session. The following commands are for use when authorized, from the repository
root with pFUnit installed:

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Debug -DFORCES_BUILD_TESTING=ON
cmake --build build --parallel 4
ctest --test-dir build --output-on-failure -R '^test_mo_utils$'
```

Replace the pattern with the affected suite. To configure only that suite, add
`-DFORCES_TEST_GLOB='test_mo_utils.pf'` to the configure command. Ordinary test
configuration can succeed without pFUnit while adding no tests; check the CMake
output and follow the README's **Testing with pFUnit** instructions. Coverage
configuration requires GNU Fortran and pFUnit; see **Coverage reports** in the
README for LCOV/Cobertura commands.

Choose validation to match the change:

| Change | Checks when authorized |
| --- | --- |
| Prose or guidance | Review the diff, links, paths, and commands. |
| Public numerical behavior | Debug build and affected pFUnit suites, with independent numerical checks. |
| Template, kind, or rank changes | Regenerate fallbacks, inspect diffs, and exercise affected generic variants. |
| Optional-feature or C backend changes | Build the affected enabled/disabled configuration and check relevant platform paths. |
| OpenMP behavior | Exercise affected suites with one and multiple threads, compare results, and check serial fallback. |
| Documentation structure or PDF support | Generate Doxygen and inspect affected links/warnings; compile the PDF when its infrastructure changes. |

For OpenMP checks, use a separate build directory so the serial configuration is
retained:

```bash
cmake -S . -B build_openmp -DCMAKE_BUILD_TYPE=Debug \
  -DFORCES_BUILD_TESTING=ON -DFORCES_WITH_OpenMP=ON
cmake --build build_openmp --parallel 4
OMP_NUM_THREADS=1 ctest --test-dir build_openmp --output-on-failure -R '^test_mo_utils$'
OMP_NUM_THREADS=4 ctest --test-dir build_openmp --output-on-failure -R '^test_mo_utils$'
```

Ensure the selected cases actually enter parallel work. Preserve barriers and
data dependencies, use disjoint writes, and keep serial builds working. Keep the
full compiler, Release, and coverage matrix in CI rather than repeating it for
every local change.

## Maintain documentation and examples

Define topic and directory descriptions in `doc/directories.dox`. Keep the six
functional topics independent of the physical source layout. Attach C helpers
and headers to the relevant topic, and preserve public module memberships in
generated fallbacks as well as their templates.

An example needs three linked descriptions: `\file` and `\example` blocks each
copy the program's documentation, while the program block describes what it does:

```fortran
!> \file    02_dag/02_branching.f90
!> \copydoc dag_branching

!> \example 02_dag/02_branching.f90
!> \copydoc dag_branching

!> \brief Demonstrate level sorting of a branching graph.
program dag_branching
```

Use paths relative to `examples/` and exact filename case. Describe input files,
outputs, and optional dependencies. `FORCES_EXE` selects the source to build as
`main`; examples using fixtures expect the repository root as working directory.
The Examples index is populated automatically from `\example` entries; its
introduction is customized in `doc/doxygen-layout.xml`. Keep that index automatic
as examples are added. Topic pages can highlight relevant examples.

Generate documentation from the repository root with the pinned Doxygen 1.18.0
environment:

```bash
PROJECT_NUMBER="$(cat version.txt)" doxygen doc/doxygen.config
make -C latex
```

Inspect affected topic pages, example links, and Doxygen warnings. The README
documents the RTD environment, system TeX dependencies, and GitLab publishing.
Keep the vendored theme unchanged except during an intentional dependency
update. Exclude generated HTML, PDFs, logs, build products, and runtime outputs
from commits; retain factual attribution and existing fixture data.
