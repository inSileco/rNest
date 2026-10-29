context("active.nest")

test_that("active.nest() gives the same dates as the precalculated data", {
  calc <- active.nest(nw, nparam = np, progress = FALSE)
  prec <- active.nest(nw, nparam = np, useprecalc = TRUE, progress = FALSE)
  expect_equal(nrow(calc), length(unique(nw$nest_id)))
  expect_equal(calc$id, prec$id)
  expect_equal(calc$initiation, as.Date(prec$initiation))
  expect_equal(calc$fledging, as.Date(prec$fledging))
  expect_equal(as.numeric(calc$len_active), as.numeric(prec$len_active))
})

test_that("active.nest() filters by species and year", {
  ans <- active.nest(nw, sp = "AMRO", year = 1999, nparam = np,
    progress = FALSE)
  expect_equal(unique(ans$sp), "AMRO")
  expect_equal(format(ans$initiation, "%Y"), "1999")
})

test_that("active.nest() fails on unknown species", {
  expect_error(
    active.nest(nw, sp = "ZZZZ", nparam = np, progress = FALSE),
    "No nests found"
  )
})
