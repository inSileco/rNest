context("i18n")

keys <- function(x, prefix = "") {
  if (!is.list(x) || is.null(names(x))) {
    return(prefix)
  }
  unlist(lapply(names(x), function(n) {
    # group names are themselves translated, so only the parent key counts
    if (n == "lgroupsl") {
      return(paste(prefix, n, sep = "/"))
    }
    keys(x[[n]], paste(prefix, n, sep = "/"))
  }))
}

test_that("English and French files have the same keys", {
  en <- yaml::yaml.load_file(system.file("i18n", "en.yml", package = "rNest"))
  fr <- yaml::yaml.load_file(system.file("i18n", "fr.yml", package = "rNest"))
  expect_equal(sort(keys(en)), sort(keys(fr)))
})

test_that("i18n() returns a section in the requested language", {
  expect_equal(rNest:::i18n("rNest_tcltk", "en")$warninglabl, "Warning")
  expect_equal(rNest:::i18n("rNest_tcltk", "fr")$warninglabl, "Avertissement")
})
