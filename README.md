# rNest
<!-- badges: start -->
[![R-CMD-check](https://github.com/inSileco/rNest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/inSileco/rNest/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


**A package for backcalculating first egg and fledging dates from nest observations in birds.**

This package was initialy based on private data from the Project NestWatch database and only an excerpt is given as an example in this version. In its current version, the package can mostly be used to backcalculate first egg and nest departure dates from a set of nest observations and nesting parameters of species invloved. The user has to provide his on data.

## Installation

```r
pak("inSileco/rNest")
library(rNest)
```

## Basic usage

Estimate the chronology of every nest (initiation and fledging dates) from
nest visits and the nesting parameters of each species:

```r
data(nestwatch)
data(nestingparameters)
nests <- active.nest(nestwatch, nparam = nestingparameters, progress = FALSE)
nests[, c("sp", "id", "initiation", "fledging", "warning")]
```

Then compute the proportion of nests active on each day of the season and
draw it as a nesting calendar:

```r
activity <- nesting.activity(nests)
nesting.calendar(activity)
```

The same tools are available in a Tcl/Tk user interface, which also offers
nesting calendars predicted by ecodistrict across Canada:

```r
rNest() # English interface
rNid()  # French interface
```

See `vignette("rNest")` for a longer walkthrough.



## Previous version

rNest was written by Francois Rousseu and Bruno Drolet; their original
package is still available, unchanged, at
[frousseu/rNest](https://github.com/frousseu/rNest).

The last release of that code base in this repository is tagged
[v0.2.2](https://github.com/inSileco/rNest/tree/v0.2.2). It depends on
rgdal, rgeos and maptools, which were removed from CRAN, so it cannot be
installed on a current version of R. It still runs on R 3.2.0 with the
Dockerfile shipped in that tag (`inst/Dockerfile`), which installs the
dependencies from the CRAN archive and runs the test suite during the build:

```sh
git clone https://github.com/inSileco/rNest.git
cd rNest
git checkout v0.2.2
docker build -f inst/Dockerfile -t rnest .
docker run --rm -it rnest
```

The `rNest()` interface needs an X server; on Linux, run the container with:

```sh
xhost +local:docker
docker run --rm -it -e DISPLAY=$DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix rnest
```
