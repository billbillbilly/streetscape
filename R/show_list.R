#' Field and Filter List
#' @name available-field-and-filter
#'
#' @title available-field-and-filter

#' @description available_field provides a list of available fields.
#' @return dataframe, including field names and their descriptions.
#' @note More information about fields and filter
#' at \url{https://www.mapillary.com/developer/api-documentation}
#' @export
#' @rdname available-field-and-filter
available_field <- function(){
  row_n <- c(
    'creator',
    'make',
    'model',
    'captured_at',
    'is_pano',
    'compass_angle',
    'computed_compass_angle',
    'thumb_256_url',
    'thumb_1024_url',
    'thumb_2048_url',
    'thumb_original_url',
    'height',
    'width',
    'computed_geometry',
    'altitude',
    'computed_altitude',
    'detections.value',
    'detections.geometry'
  )
  explain <- c(
    "string, the username who owns and uploaded the image.",
    "string, the manufacturer name of the camera device.",
    "string, the model or product series name of the camera device.",
    "timestamp, capture time.",
    "boolean, a true or false filter for whether an image is 360 degree panorama. Must be used with creator_username.",
    "float, original compass angle of the image.",
    "float, compass angle after running image processing.",
    "string, URL to the 256px wide thumbnail.",
    "string, URL to the 1024px wide thumbnail.",
    "string, URL to the 2048px wide thumbnail.",
    "string, URL to the original wide thumbnail.",
    "int, height of the original image uploaded.",
    "int, width of the original image uploaded.",
    "longitude and latitude, location after running image processing.",
    "float, original altitude from camera Exif calculated from sea level.",
    "float, altitude after running image processing, from sea level.",
    "string, name of the image segmentation",
    "string, base64 encoded string of the image segmentation coordinates."
  )
  df <- cbind(row_n, explain)
  return(as.data.frame(df))
}


#' @description available_filter provides a list of available filters.
#' @return dataframe, including filter names and their descriptions.
#' @export
#' @rdname available-field-and-filter
available_filter <- function(){
  row_n <- c(
    'creator_username',
    'is_pano',
    'make',
    'model',
    'start_captured_at',
    'end_captured_at',
    'detections.value'
  )
  explain <- c(
    "string, the username who owns and uploaded the image.",
    "boolean, a true or false filter for whether an image is 360 degree panorama. Must be used with creator_username.",
    "string, the manufacturer name of the camera device. Spaces are allowed.",
    "string, the model or product series name of the camera device. Spaces are allowed.",
    "string, filter images captured after. For example: 2022-08-16T16:42:46Z",
    "string, filter images captured before. For example: 2022-08-16T16:42:46Z",
    "string, what kind of object the detection represents."
  )
  df <- cbind(row_n, explain)
  return(as.data.frame(df))
}

