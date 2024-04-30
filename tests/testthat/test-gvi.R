testthat::test_that("runs correctly", {
  #Load in data
  data('scdataframe')
  scdataframe$gvi()
  testthat::expect_type(scdataframe$data$GVI, "double")
})
