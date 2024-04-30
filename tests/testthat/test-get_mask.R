source("utils.R")
testthat::test_that("runs correctly", {
  #Load in data
  data('scdataframe')
  skip_if_no_mvt()
  out <- scdataframe$get_mask(1)
  testthat::expect_type(out, "list")
})
