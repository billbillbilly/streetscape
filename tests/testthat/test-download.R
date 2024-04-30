Sys.setenv(USE_CORES = 1)
testthat::test_that("runs correctly", {
  #Load in data
  data('scdataframe')
  out <- scdataframe$download_data(items=c('image'))
  testthat::expect_type(out, "character")
})
