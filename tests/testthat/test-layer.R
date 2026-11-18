# Results recorded with the sp implementation (rNest 1.0.0), before the move
# to sf, with the ecodistricts layer built by the app.
ref_layer <- readRDS(test_path("fixtures", "layer_regions_sp.rds"))
eco <- get(data(ecodistrictscsv, package = "rNest", envir = environment()))
eco <- build_ecodistricts(eco)

run_layer <- function(layer, ...) {
  tryCatch(
    active.nest(nw, nparam = np, progress = FALSE, layer = layer, ...),
    error = function(e) {
      structure(conditionMessage(e), class = "golden_error")
    }
  )
}

test_that("build_ecodistricts() builds one valid feature per ecodistrict", {
  expect_s3_class(eco, "sf")
  expect_equal(nrow(eco), 1239)
  expect_equal(sf::st_crs(eco), sf::st_crs(4269))
  expect_true(all(sf::st_is_valid(eco)))
})

test_that("active.nest() assigns the regions of a layer as with sp", {
  cases <- list(
    first_col = run_layer(eco),
    eco = run_layer(eco, id = "ecodistrict"),
    multi = run_layer(eco, id = c("provincesEN", "breeding_zone")),
    bz = run_layer(eco, id = "breeding_zone")
  )
  cases$region <- run_layer(
    eco,
    id = "breeding_zone",
    region = unique(cases$bz$region)[1]
  )
  cases$region_none <- run_layer(eco, id = "breeding_zone", region = "zzz")
  for (n in names(ref_layer)) {
    expect_equal(cases[[n]], ref_layer[[n]], label = n)
  }
})

test_that("a layer can be given as a shapefile path", {
  skip_on_cran()
  d <- tempfile()
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  f <- file.path(d, "eco.shp")
  suppressWarnings(sf::st_write(eco, f, quiet = TRUE))
  expect_equal(
    run_layer(sub("\\.shp$", "", f))$region,
    run_layer(eco)$region
  )
})

test_that("a layer can be given as an sp object", {
  skip_if_not_installed("sp")
  expect_equal(
    run_layer(sf::as_Spatial(eco), id = "breeding_zone"),
    run_layer(eco, id = "breeding_zone")
  )
})
