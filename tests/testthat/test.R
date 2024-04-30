testthat::test_that("runs correctly", {
  #Load in data
  data('scdataframe')
  testthat::expect_type(scdataframe$data, "list")
})
