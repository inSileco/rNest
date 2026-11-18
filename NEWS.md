# rNest 1.0.0

rNest is now maintained by inSileco. The original package by Francois Rousseu
and Bruno Drolet is still available, unchanged, at
[frousseu/rNest](https://github.com/frousseu/rNest).

## Breaking changes

* rNest now requires R >= 4.2.0 and installs on current versions of R.
* Dependencies removed from CRAN (rgdal, rgeos, maptools) are dropped: spatial
  layers are read with sf. Packages previously listed in `Depends` are now in
  `Imports`, so they are no longer attached with rNest.
* The license changed from GPL 2.0 to MIT, with the agreement of the original
  authors.

## Other changes

* Documentation and `NAMESPACE` are generated with roxygen2.
* New "Get started" vignette (`vignette("rNest")`) and a pkgdown site.
* `windows()` calls are replaced by a cross-platform plot device whose size is
  capped to fit the screen; the windows of the Tcl/Tk interface were resized.
* `data()` calls name the package they load from (fixes the mapdata call).
* Data sets were resaved, with smaller files.
* New tests compare results with reference outputs produced by v0.2.2 on
  R 3.2.0 (`inst/old_results/`).
* Continuous integration with GitHub Actions (R CMD check on Linux and Windows).



# rNest 0.2.2

Last release of the original code base, which depends on rgdal, rgeos and
maptools and only runs on old versions of R (R 3.2.0, see the Dockerfile in
that tag).

* Core functions: `active.nest()` and `chronology()` estimate the chronology of
  each nest (first egg and fledging dates) from nest visits and species nesting
  parameters; `nesting.activity()` and `nesting.calendar()` compute and draw
  nesting calendars.
* `rNest()` (English) and `rNid()` (French) open a Tcl/Tk interface, which also
  offers nesting calendars predicted by ecodistrict across Canada.
* 0.2.1–0.2.2: code reformatted, dead code removed, interface text moved to
  YAML files, the Tcl/Tk app gathered in a single file, and a minimal test
  suite added.
