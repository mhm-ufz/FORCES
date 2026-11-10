# Building and publishing the documentation {#forces_documentation}

Generate the HTML/PDF manual and maintain the GitLab and Read the Docs
publishing workflows. Run all commands from the repository root. For writing
module and example documentation, see the
[developer guide](development-guide.md).

[TOC]

## Generate HTML and PDF

Generate HTML and LaTeX from the repository root with:

```bash
PROJECT_NUMBER="$(cat version.txt)" doxygen doc/doxygen.config
make -C latex
```

Open `html/index.html` to browse the manual or `latex/refman.pdf` for the PDF.
The Library sections view organizes the API by purpose: core utilities and data
structures, spatial data and regridding, dates and time series, file I/O and
operating system support, statistics and numerical methods, and optimization and
sampling.
Definitions live in `doc/directories.dox`; module headers declare their topic
with `\ingroup`, and topic pages link to relevant examples.

The mobile menu starts with FORCES library, Guides, About, Library sections,
API reference and Examples. FORCES library opens the README, with
requirements and build/run instructions under its first Quick start section.
The desktop tree starts with FORCES library and uses the README heading
for the page entry; the layout title controls the tab/mobile label. Local CSS in
`doc/html_files/header.css` hides the outer FORCES project row and removes its
indentation, leaving Doxygen's tree structure and navigation logic intact.
The Guides menu links to the usage, development and documentation pages; API
reference groups the generated Fortran module, derived type and source file
indices. About links to the changelog, license and repository
instructions, which are also linked from the main page.
Guides, About and API reference use `url="[none]"` so their labels
expand or collapse the group without opening a generated landing page.
Maintain this structure in `doc/doxygen-layout.xml`, using page IDs or Markdown
file references and keeping the library sections, examples and API indices
automatic. Add new standalone pages to the appropriate navigation group. Check
both the mobile menu and desktop navigation tree after layout changes.

Give standalone Markdown pages an explicit ID on their first heading, such as
`# Changelog {#changelog}`, to obtain a readable filename (`changelog.html`).
The guides already use `forces_usage`, `forces_development` and
`forces_documentation`; project pages use `changelog`, `license` and
`repository_instructions`. Link to these IDs with `@ref` in the layout, and keep
relative Markdown links in the guides so they also work in the repository.
`MARKDOWN_ID_STYLE = GITHUB` produces readable section anchors such as
`index.html#quick-start`. Add explicit heading IDs when an anchor must remain
stable across heading changes; duplicate heading text may require a suffix in
automatically generated IDs. Doxygen continues to name API pages and member
anchors. Existing external links to the former generated page names and numbered
section anchors need updating.

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

## GitLab publishing

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

## Read the Docs

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
