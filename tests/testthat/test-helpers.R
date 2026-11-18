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
  expect_equal(rNest:::sub_list(c("AN", "HY", "XX"), l), c("nest", "hatch", NA))
  expect_error(rNest:::sub_list("AN", list("AN")), "Not a named list")
  expect_error(
    rNest:::sub_list("AN", list(a = "AN", b = "AN")),
    "more than once"
  )
})


test_that("in_polygon() keeps the points inside a polygon, as splancs did", {
  square <- cbind(c(0, 1, 1, 0), c(0, 0, 1, 1))
  expect_equal(
    rNest:::in_polygon(c(0.5, 1.5, -0.1, 0.99), c(0.5, 0.5, 0.5, 0.01), square),
    c(TRUE, FALSE, FALSE, TRUE)
  )
  # closing the ring changes nothing
  expect_equal(
    rNest:::in_polygon(c(0.5, 1.5), c(0.5, 0.5), rbind(square, square[1, ])),
    c(TRUE, FALSE)
  )
  # concave (L-shaped)
  ell <- cbind(c(0, 2, 2, 1, 1, 0), c(0, 0, 1, 1, 2, 2))
  expect_equal(
    rNest:::in_polygon(c(0.5, 1.5, 1.5, 0.5), c(0.5, 0.5, 1.5, 1.5), ell),
    c(TRUE, TRUE, FALSE, TRUE)
  )
  # self-intersecting bow-tie: both lobes are inside
  bowtie <- cbind(c(0, 2, 2, 0), c(0, 2, 0, 2))
  expect_equal(
    rNest:::in_polygon(c(1.8, 0.2, 1, 1), c(1, 1, 0.2, 1.8), bowtie),
    c(TRUE, TRUE, FALSE, FALSE)
  )
  # pentagram: the centre is inside (nonzero winding, as splancs::inout())
  a <- pi / 2 + 4 * pi / 5 * 0:4
  star <- cbind(cos(a), sin(a))
  expect_true(rNest:::in_polygon(0, 0, star))
  # fewer than 3 vertices
  expect_equal(rNest:::in_polygon(c(0, 1), c(0, 1), square[1:2, ]), c(FALSE, FALSE))
  expect_equal(rNest:::in_polygon(0, 0, matrix(numeric(0), ncol = 2)), FALSE)
})
