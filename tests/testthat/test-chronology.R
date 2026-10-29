context("chronology")

test_that("chronology() matches the dates stored in nestwatch", {
  # nests without eggs or young are checked separately below
  for (i in unique(nw$nest_id[!is.na(nw$initiation)])) {
    y <- nw[nw$nest_id == i, ]
    ans <- chronology(y, nparam = np)
    expect_equal(ans[[1]], as.Date(y$initiation[1]), info = i)
    expect_equal(ans[[2]], as.Date(y$fledging[1]), info = i)
  }
})

test_that("chronology() returns NA dates for nests without eggs or young", {
  y <- nw[nw$nest_id == 102658, ]
  ans <- chronology(y, nparam = np)
  expect_true(is.na(ans[[1]]))
  expect_true(is.na(ans[[2]]))
})

test_that("chronology(test = TRUE) returns the day-by-day stages", {
  ans <- chronology(nw[nw$nest_id == 43359, ], nparam = np, test = TRUE)
  expect_is(ans, "data.frame")
  expect_true(all(c("date", "STAGE", "nest_param") %in% names(ans)))
  expect_true("laying" %in% ans$STAGE)
})
