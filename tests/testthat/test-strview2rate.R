Sys.setenv(USE_CORES = 1)
testthat::test_that("runs correctly", {
  #Load in data
  data('scdataframe')
  questions = c('1. To what existence you can feel pleasant if you were in this environment',
                '2. To what existence you can feel safe if you were in this environment')
  choices <- list(c('Unpleasant','Less pleasant', 'More pleasant', 'Pleasant'),
                  c('Unsafe', 'Less safe','Safer','Safe'))
  header <- "Please review the following picture(s):"
  out <- streetscape::strview2rate(scdataframe, header, questions, choices)
  testthat::expect_type(out, "character")
})
