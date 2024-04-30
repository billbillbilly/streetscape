testthat::test_that("runs correctly", {
  #Load in data
  data('scdataframe')
  scdataframe$decodeDetection()
  testthat::expect_type(scdataframe$data$segmentation, "list")
})
