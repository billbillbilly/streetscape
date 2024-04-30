testthat::test_that("runs correctly", {
  #Load in data
  data('scdataframe')
  map <- scdataframe$mapPreview('meta')
  testthat::expect_type(map, "S4")
})
