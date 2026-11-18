pr <- get(data(predictions, package = "rNest", envir = environment()))
groups <- list(a = c("COLO", "CAGO"), b = "YWAR")
cal <- rNest:::nesting.pred(pr, c(429, 431), groups)

test_that("nesting.pred() returns one column per ecodistrict and group", {
  expect_equal(dim(cal), c(366, 5))
  expect_equal(
    names(cal),
    c("date", "429___a", "429___b", "431___a", "431___b")
  )
  expect_equal(cal$date[1:3], c("nb_nest", "01-01", "01-02"))
  expect_equal(cal$date[366], "12-31")
})

test_that("nesting.pred() counts nests and gives proportions of active nests", {
  expect_equal(unlist(cal[1, -1], use.names = FALSE), c(2, 1, 2, 1))
  p <- as.matrix(cal[-1, -1])
  expect_true(all(p >= 0 & p <= 1))
  # nests are active in summer, not in winter
  expect_true(all(p[1, ] == 0))
  expect_true(all(apply(p, 2, max) > 0))
})

test_that("nesting.pred() orders columns by ecodistrict or by group", {
  expect_equal(
    names(rNest:::nesting.pred(pr, c(429, 431), groups, "eco")),
    c("date", "431___b", "431___a", "429___b", "429___a")
  )
  expect_equal(
    names(rNest:::nesting.pred(pr, c(429, 431), groups, "spgr")),
    c("date", "431___b", "429___b", "431___a", "429___a")
  )
})

test_that("nesting.pred() returns only dates when no species is predicted", {
  empty <- rNest:::nesting.pred(pr, 429, list(a = "none"), "eco")
  expect_equal(names(empty), "date")
})
