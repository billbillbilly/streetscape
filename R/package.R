mapbox_vector_tile <- NULL
.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
  if ("r-mvt" %in% reticulate::virtualenv_list()) {
    if (Sys.getenv("RETICULATE_PYTHON") != reticulate::virtualenv_python("r-mvt")) {
      Sys.setenv(RETICULATE_PYTHON = reticulate::virtualenv_python("r-mvt"))
    }
    if (reticulate::py_config()['python'] != reticulate::virtualenv_python("r-mvt")) {
      reticulate::use_virtualenv("r-mvt", required = TRUE)
    }
  }

  tryCatch(
    mapbox_vector_tile <<- reticulate::import(
      "mapbox_vector_tile",
      delay_load = list(TRUE, environment = "r-mvt")
    )
  )
}
