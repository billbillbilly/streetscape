testthat::test_that("runs correctly", {
  #Load in data
  data('scdataframe')
  out <- scdataframe$get_mask(1)
  testthat::expect_type(out, "list")
})
