Sys.setenv(USE_CORES = 1)
testthat::test_that("runs correctly", {
  #Load in data
  data('scdataframe')
  questions <- 'which one is more beautiful?'
  header <- "Please review the following picture(s):"
  out <- streetscape::strview2pwc(scdataframe, k=1, header, questions)
  testthat::expect_type(out, "character")
})
