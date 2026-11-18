Dear XXX,

We've contacted Francois Rousseau and Bruno Drolet about their package rNest. They were OK with us taking over rNest as long as they keep their version untouched in Francois GitHub repository and we mention their work.
They also agreed on the change of license so we swhicthe to MIT starting in v1.0.0.

Last veriosn of the package was in Mach 29, 2017, almost 10 years ago, some package wer rNest only builds on R 3.2.0  because it                                                                  │
│ depends on rgdal, rgeos and maptools, which were removed from CRAN. nge.



1. Dependencies: DESCRIPTION

- Depends: R (>= 4.1.0); everything else in Imports: tcltk, svMisc, splancs,
  maps, mapdata, data.table, sp, sf, plyr, RColorBrewer, yaml, grDevices,
  graphics, stats, utils, methods. Drop rgdal, rgeos, maptools.
- Suggests: testthat (>= 3.0.0); Config/testthat/edition: 3;
  Encoding: UTF-8; Roxygen: list(markdown = TRUE); RoxygenNote;
  License: GPL-2 (currently the invalid GPL 2.0).

2. Code changes that keep the logic

- readOGR(dsn, layer) → as(sf::st_read(dsn, layer, quiet = TRUE, stringsAsFactors = TRUE), "Spatial"): same SpatialPolygonsDataFrame, with
  factors as readOGR gave on R 3.2.
- TRUERUE → TRUE (3 places).
- windows(w, h) → dev.new(width = w, height = h) (named: X11's first
  positional argument is the display).
- data() calls made while the package isn't attached: add package =
  (data(nestwatch, package = "rNest") etc. in rNest_tcltk(), still loading
  into the global environment as now; data(worldHiresMapEnv, package = "mapdata")
  in active.nest()).
- R 4 runtime differences found by the step 0 comparison (e.g. a missing
  stringsAsFactors = TRUE, a length > 1 if): fix each one to reproduce the
  R 3.2.0 result, with a short comment, and list them in the final report.

3. NAMESPACE and documentation with roxygen2

- R/rNest-package.R: package doc + @importFrom for every function used from
  each package, generated from a codetools scan of R/ (same approach as the
  B refactor checks), to avoid whole-namespace conflicts (plyr/data.table).
- R/globals.R: utils::globalVariables() for the datasets the code reads from
  the global environment and the NSE column names (list from the codetools scan:
  nestwatch, predictions, species_code, Species_ID, ...).
- Convert man/*.Rd/*.rd with Rd2roxygen into roxygen blocks above the
  exported functions; dataset docs go to R/data.R. Fix usage mismatches (e.g.
  fromtk = FALSE ... in active.nest.Rd); wrap GUI examples in \dontrun{}.
  Generate man/ and NAMESPACE with roxygen2::roxygenise(); exports stay
  the same six: chronology, active.nest, nesting.activity, nesting.calendar,
  rNest, rNid.

4. Tests: testthat 3rd edition

- Drop context() calls; keep tests/testthat.R. Adjust only for edition-3
  strictness (e.g. names/attributes in expect_equal), not expected values.

5. Docker and CI

- inst/Dockerfile → rocker/r-ver:4.6.2 (CRAN snapshot pinned by rocker):
  apt libgdal-dev libgeos-dev libproj-dev libudunits2-dev libsqlite3-dev
  - Tcl/Tk runtime; install deps with pak::local_install_deps(); build runs
    R CMD build + R CMD check --no-manual and fails on errors/warnings. Keep the
    X11 usage notes. Check capabilities("tcltk") first; if r-ver lacks it, add
    tk-dev/use the matching rocker base.
- .github/workflows/R-CMD-check.yaml from r-lib/actions v2 (check-standard:
  ubuntu, macOS, windows, R release). Not pushed.
- .Rbuildignore: .github, ABOUT.md (exists), inst/report only if it
  breaks the check (it's your untracked work; I'll ask then).

Verification

1. Docker build on R 4.6.2: R CMD check with 0 errors, 0 warnings; remaining
   notes reported.
2. Rerun the step 0 script on R 4.6.2 and compare with the R 3.2.0 results
   (all.equal); every difference is either fixed or reported.
3. App on R 4.6.2 (Xvfb in a throwaway container): main window in English and
   French, the single-nest windows, the driver's button presses with the same
   calendars/messages/saved objects as on R 3.2.0, a user shapefile read through
   sf, and prediction/observation calendars now drawn (no windows() error).
4. Nothing is committed unless you ask; I'll propose commit splits
   (deps/code, roxygen, tests, Docker/CI).
