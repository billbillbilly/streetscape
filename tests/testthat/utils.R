skip_if_no_mvt <- function() {
  have_mvt <- reticulate::py_module_available("mapbox_vector_tile")
  if (!have_mvt) {
    skip()
  }
}
