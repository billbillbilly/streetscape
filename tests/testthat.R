library(testthat)
library(streetscape)

Sys.setenv(USE_CORES = 1)
testthat::test_check("streetscape")
