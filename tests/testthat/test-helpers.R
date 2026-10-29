context("internal helpers")

test_that("span() counts days, both ends included", {
  d <- as.Date(c("2000-05-01", "2000-05-10"))
  expect_equal(rNest:::span(d), 10)
  expect_equal(rNest:::span(d[1], as.Date("2000-05-03")), 3)
  expect_equal(rNest:::span(as.Date(character(0))), 0)
})

test_that("filldate() fills every day between the first and last date", {
  ans <- rNest:::filldate(c("2000-05-01", "2000-05-04"))
  expect_equal(ans, seq(as.Date("2000-05-01"), by = 1, length.out = 4))
})

test_that("hatching() detects an increase after a non-negative start", {
  expect_true(rNest:::hatching(c(0, 2, 1)))
  expect_false(rNest:::hatching(c(2, 1, 0)))
  expect_false(rNest:::hatching(c(-1, 2)))
})

test_that("sub_list() maps codes to their group name", {
  l <- list(nest = c("NN", "AN"), hatch = "HY")
  expect_equal(rNest:::sub_list(c("AN", "HY", "XX"), l),
    c("nest", "hatch", NA))
  expect_error(rNest:::sub_list("AN", list("AN")), "Not a named list")
  expect_error(rNest:::sub_list("AN", list(a = "AN", b = "AN")),
    "more than once")
})
