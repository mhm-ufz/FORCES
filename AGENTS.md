# FORCES repository instructions {#repository_instructions}

These rules apply throughout the repository to new or touched code. Read the
[developer guide](doc/development-guide.md) for module relationships and extension
workflows, and the [user guide](doc/user-guide.md) for dependencies and build options.
The [README](README.md) provides the quick start.

## Workflow

- Inspect nearby modules, callers, tests, and build options before choosing an API.
  Clarify an underspecified request before planning and propose a short file-level
  plan before implementation.
- Keep changes scoped. Preserve scientific units, defaults, numerical conventions,
  and calculation order unless the requested behavior requires a change.
- Do not run compilation or tests unless the user explicitly asks in the current
  session. Otherwise perform static review and provide the exact commands to run.
- Create commits only when explicitly requested. Do not install dependencies,
  push branches, or refactor unrelated modules without an explicit request.
- At handoff, report changed files, validation commands, results, and checks that
  were not run. State unavailable dependencies or coverage explicitly.

## Modules and public APIs

- Add new public functionality as a `mo_*` module under `src/`. Use `implicit none`,
  private-by-default module contents, and explicit public exports.
- Use kinds from `mo_kind` and typed literals such as `0_i8` and `1.0_dp`.
  Give dummy arguments explicit `intent`; prefer `use ..., only:`.
- Reuse `mo_message`, `mo_string_utils`, `mo_io`, and the existing grid, point,
  time-series, and NetCDF abstractions. Follow existing error/status contracts;
  use `error_message` or `warn_message` where surrounding code does.
- Prefer caller- or type-owned mutable state. Document owned storage and borrowed
  pointers, their lifetime, and any initialization, reset, close, or destroy steps.
- Prefix type-bound implementations consistently with their owning type. Keep
  dummy declarations together before local variables; document each argument
  except `self`/`this` on its declaration with `!<`.
- Import `mo_logging` without `only` when using `logging.h`; its macros reference
  module names beyond the apparent call site. Guard logging macros with block
  `if ... then ... end if`, never a one-line logical `if`.
- Use `iso_c_binding` for C interoperability. Keep platform details in the C
  backends and expose caller-owned buffers with explicit capacity and termination
  semantics. NetCDF support depends on NetCDF-C; use `mo_netcdf_wrapper`'s `ncw_*`
  interface instead of introducing a NetCDF-Fortran dependency.

## Fortran correctness and data conventions

- Do not rely on `.and.`, `.or.`, or `merge()` to guard invalid accesses; Fortran
  need not short-circuit. Check presence, allocation, association, and bounds in
  separate statements before dereferencing.
- An unallocated allocatable or disassociated pointer actual can be absent for a
  compatible optional dummy that is neither allocatable nor a pointer. Use
  `present()` before accessing such dummies.
- Mutable local declaration initialization implies `SAVE`. Initialize scratch in
  executable statements and keep routines called from threads reentrant.
- `intent(out)` can deallocate allocatable components on entry; intrinsic
  assignment can reallocate storage. Do not invalidate targets while borrowed
  pointers remain associated. A dummy's `target` attribute alone does not ensure
  persistent caller-owned storage; pointer assignment and derived-type copying
  do not transfer ownership.
- Preserve `(x, y)` grid orientation and mask packing in Fortran order. Use
  `grid%pack_into`/`unpack_into` or the corresponding allocating helpers rather
  than reshaping masked data. Keep point-set ordering distinct from packed cells.
- Preserve time units, bounds, timestamp location, and instant versus interval
  semantics. Resampling interval means and accumulated values requires different
  aggregation rules; use the existing time-axis and resampling helpers.
- Prepare reusable scratch outside substantial repeated work. Accept strided
  assumed-shape arguments where possible; avoid unnecessary contiguity copies.
- Extend arrays of NetCDF variable metadata with `mo_netcdf_utils:add_var` rather
  than in-place array constructors involving allocatable derived-type components.

## Templates and build integration

- `.fypp` files are authoritative where present. Edit them and regenerate their
  tracked `.f90` outputs together; never hand-edit generated counterparts. Follow
  `src/common.fypp` and the flags in `src/CMakeLists.txt` for kinds and ranks.
- `src/CMakeLists.txt` discovers `mo_*.f90` and `mo_*.F90` with a glob. Register new
  templates in its Fypp list and generated-source exclusion lists, and C helpers
  with `target_sources`. Reconfigure after adding source files or test suites.
- Preserve NetCDF, optimization, MPI, and OpenMP build options. Update source and
  pFUnit exclusions when adding functionality behind an optional feature.
- Keep consumer builds portable and link through the `forces` CMake target.
  Preserve opt-in native tuning and the separate public OpenMP propagation option.
- Treat `cmake/`, `hpc-module-loads/`, and `doc/doxygen-awesome-css/` as external
  projects. Change them only for requested dependency or workflow updates, and
  preserve their licenses and provenance.

## OpenMP and numerical changes

- Parallelize substantial independent work with explicit data sharing and disjoint
  writes. Prefer `schedule(static)` for regular loops; preserve barriers and
  dependencies and avoid per-cell or nested parallel regions.
- Keep serial builds functional, including conditional `omp_lib` imports and
  calls. Reuse prepared workspaces and preserve documented RNG seeding/stream
  behavior.
- When testing is authorized, check serial behavior and one and multiple OpenMP
  threads, exercise the parallel path, and compare scientific results with
  justified tolerances. Support performance claims with representative timings.

## Documentation and examples

- Follow nearby file/module headers: exact filename and case, `\copydoc` target,
  factual authorship, dates, copyright, and license. Preserve donor attribution;
  do not invent authors or dates.
- Every production module declares one primary `\ingroup` immediately before
  its module declaration. Define topics and directory descriptions centrally in
  `doc/directories.dox`, using these exact IDs:

  | Topic | Group ID |
  | --- | --- |
  | Core utilities and data structures | `f_core` |
  | Spatial data and regridding | `f_spatial` |
  | Dates and time series | `f_time` |
  | File I/O and operating system support | `f_io` |
  | Statistics and numerical methods | `f_statistics` |
  | Optimization and sampling | `f_optimization` |

- Group C helper files and headers with their related functionality. Document C
  arguments, results, buffer ownership, truncation, and platform differences.
- Document types with `!> \class type_name`, procedures with `\brief`, and
  non-obvious assumptions with short block comments.
- Examples need matching `\file` and `\example` paths relative to `examples/`,
  each with `\copydoc` pointing to the program's documented name. Document input
  files, outputs, and optional dependencies; run fixture-based examples from the
  repository root.
- Keep the Examples index automatic. Customize its introduction in
  `doc/doxygen-layout.xml`; do not replace it with a manually maintained catalogue.
- Keep documentation generation rooted at the repository top. Use the pinned
  Doxygen environment and HTML/PDF commands in the
  [documentation guide](doc/documentation-guide.md). Preserve figures,
  source listings, licenses, release history, and functional RTD/GitLab navigation.

## Validation

- Add focused pFUnit regressions under `src/pf_tests/` for new public behavior.
  Suite names follow the `.pf` filename; `FORCES_TEST_GLOB` selects configured
  suites, while `ctest -R` selects tests to execute.
- Keep every pFUnit `@assert...` on one physical source line. Test boundary cases,
  supported kinds/ranks, masks and ordering, optional arguments, and error paths
  relevant to the change. Numerical refactors need an independent reference or
  pre-change comparison; successful execution alone is insufficient.
- Choose focused checks from the developer guide when compilation/testing is
  authorized. Keep full compiler/Release/coverage matrices in CI; do not run model
  examples solely for prose edits.
- Ordinary test configuration may succeed without pFUnit while adding no FORCES
  tests. Report this explicitly and point to "Testing with pFUnit" in the
  [developer guide](doc/development-guide.md). Coverage configuration requires
  GNU Fortran and pFUnit and fails if pFUnit is unavailable; preserve that distinction.
- Exclude generated HTML/PDF, build products, logs, and runtime NetCDF outputs
  from commits. Keep test fixtures immutable unless updating them is part of the
  requested change.
