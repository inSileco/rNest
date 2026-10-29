context("nesting.activity")

an <- active.nest(nw, nparam = np, progress = FALSE)

test_that("nesting.activity() gives the share of active nests per date", {
  ans <- nesting.activity(an)
  expect_equal(ans$date[1], "nb_nest")
  # one column per species with at least one dated nest
  expect_equal(
    sort(names(ans)[-1]),
    sort(unique(an$sp[!is.na(an$initiation)]))
  )
  expect_equal(sum(unlist(ans[1, -1])), sum(!is.na(an$initiation)))
  props <- as.matrix(ans[-1, -1])
  expect_true(all(props >= 0 & props <= 1))
})

test_that("nesting.activity() can pool by year", {
  ans <- nesting.activity(an, by = "year")
  expect_equal(
    sort(names(ans)[-1]),
    sort(unique(format(an$initiation[!is.na(an$initiation)], "%Y")))
  )
})

test_that("nesting.calendar() draws a calendar", {
  if (.Platform$OS.type != "windows") {
    skip("nesting.calendar() opens a windows() device")
  }
  nesting.calendar(nesting.activity(an))
  expect_true(TRUE)
})
