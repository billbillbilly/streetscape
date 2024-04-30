source("utils.R")
testthat::test_that("runs correctly", {
  #Load in data
  data('scdataframe')
  skip_if_no_mvt()
  scdataframe$decodeDetection()
  testthat::expect_type(scdataframe$data$segmentation, "list")
})
