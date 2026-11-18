# Results recorded with rNest v0.2.2 on R 3.2.0, by
# inst/old_results/golden.R. The cases that need the ecodistricts layer built
# by the app are not tested here.
ref_file <- system.file("old_results", "golden_r320.rds", package = "rNest")
ref <- if (nzchar(ref_file)) readRDS(ref_file)

# The reference was recorded in a latin1 locale: read its text as UTF-8.
as_utf8 <- function(x) {
  fix <- function(v) {
    w <- !is.na(v) & grepl("[^\\x01-\\x7F]", v, perl = TRUE, useBytes = TRUE)
    v[w] <- iconv(v[w], "latin1", "UTF-8")
    v
  }
  if (is.factor(x)) {
    levels(x) <- fix(levels(x))
  } else if (is.character(x)) {
    x[] <- fix(x)
  } else if (is.list(x)) {
    a <- attributes(x)
    x <- lapply(x, as_utf8)
    attributes(x) <- a
    if (!is.null(names(x))) names(x) <- fix(names(x))
  }
  x
}

expect_reference <- function(object, expected) {
  if (inherits(expected, "golden_error")) {
    expect_error(object, unclass(expected), fixed = TRUE)
  } else {
    expect_equal(object, as_utf8(expected))
  }
}

pr <- get(data(predictions, package = "rNest", envir = environment()))

test_that("chronology() gives the results of R 3.2.0", {
  skip_if(is.null(ref), "reference results not installed")
  ids <- unique(nw$nest_id)
  expect_equal(length(ids), length(ref$chronology))
  for (i in seq_along(ids)) {
    expect_reference(
      chronology(nw[nw$nest_id == ids[i], ], nparam = np),
      ref$chronology[[i]]
    )
  }
  expect_reference(
    chronology(nw[nw$nest_id == 43359, ], nparam = np, test = TRUE),
    ref$chronology_test
  )
})

# active.nest() cases that don't need a layer
an_cases <- list(
  all = function() active.nest(nw, nparam = np, progress = FALSE),
  precalc = function() {
    active.nest(nw, nparam = np, useprecalc = TRUE, progress = FALSE)
  },
  sp = function() {
    active.nest(nw, sp = c("WAVI", "MAWR"), nparam = np, progress = FALSE)
  },
  year = function() active.nest(nw, year = 1974, nparam = np, progress = FALSE),
  unknown_sp = function() {
    active.nest(nw, sp = "ZZZZ", nparam = np, progress = FALSE)
  }
)

test_that("active.nest() gives the results of R 3.2.0", {
  skip_if(is.null(ref), "reference results not installed")
  for (n in names(an_cases)) {
    expect_reference(suppressWarnings(an_cases[[n]]()), ref$active.nest[[n]])
  }
})

test_that("nesting.activity() gives the results of R 3.2.0", {
  skip_if(is.null(ref), "reference results not installed")
  for (n in setdiff(names(an_cases), "unknown_sp")) {
    a <- suppressWarnings(an_cases[[n]]())
    r <- ref$nesting.activity[[n]]
    expect_reference(nesting.activity(a), r$default)
    expect_reference(nesting.activity(a, by = "year"), r$year)
    expect_reference(nesting.activity(a, by = "region"), r$region)
    expect_reference(
      nesting.activity(
        a,
        n = TRUE,
        dates = FALSE,
        by = "sp",
        chronology = FALSE
      ),
      r$app
    )
  }
})

test_that("nesting.pred() gives the results of R 3.2.0", {
  skip_if(is.null(ref), "reference results not installed")
  groups <- list(a = c("COLO", "CAGO"), b = "YWAR")
  orders <- c("", "eco", "spgr")
  for (i in seq_along(orders)) {
    expect_reference(
      rNest:::nesting.pred(pr, c(429, 431), groups, orders[i]),
      ref$nesting.pred[[i]]
    )
  }
})

test_that("species_groups() gives the results of R 3.2.0", {
  skip_if(is.null(ref), "reference results not installed")
  expect_reference(rNest:::species_groups(), ref$species_groups)
})
