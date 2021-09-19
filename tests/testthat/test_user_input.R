test_that("the arguments are of the right class", {
  # Load the R objects to be used in tests
  x <- readRDS(file= system.file("extdata", "watersheds.rds", package = "hypsoLoop", mustWork = TRUE))
  y <- readRDS(file= system.file("extdata", "DEM.rds", package = "hypsoLoop", mustWork = TRUE))
  dummy <- readRDS(file= system.file("extdata", "watersheds_df.rds", package = "hypsoLoop", mustWork = TRUE))
  #Test errors
  expect_error(test_arguments(dummy, y), "X has to be a spatial object", fixed=TRUE)
  expect_error(test_arguments(x, dummy), "y has to be a raster object", fixed=TRUE)
})

