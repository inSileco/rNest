groups <- rNest:::species_groups()
bg <- get(data(birdgroupings, package = "rNest", envir = environment()))

test_that("species groupings are read with their types", {
  expect_equal(
    names(groups),
    c(
      "bird_status",
      "bird_type",
      "habitat_type",
      "nest_location",
      "model_output",
      "bird_group"
    )
  )
  # unquoted Yes/No would be read as booleans
  expect_identical(groups$bird_status$values, c("Yes", "No"))
  expect_equal(groups$model_output$values, c(1, 0))
  expect_identical(groups$model_output$labels, c("accepted", "rejected"))
})

test_that("each grouping reads existing columns and labels every value", {
  for (g in groups) {
    expect_true(all(g$columns %in% names(bg)))
    expect_equal(length(g$labels), length(g$values))
  }
})

test_that("translated group labels match the groupings", {
  for (lang in c("en", "fr")) {
    lgroupsl <- rNest:::i18n("tk.nesting.calendar", lang)$lgroupsl
    expect_equal(
      unname(lengths(lgroupsl)),
      unname(lengths(lapply(groups, `[[`, "labels")))
    )
  }
})
