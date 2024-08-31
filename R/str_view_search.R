#' strview_search
#' @name strview_search

#' @title strview_search
#'
#' @description strview_searchByGeo: Search for and download the meta information
#' of street view images via Mapillary API (See detials) based on coordinates
#' of a spatial point with a given distance or a bounding box.
#' @param x numeric, indicating Longtitude degree of the center point.
#' @param y numeric, indicating latitude degree of the center point.
#' @param r numeric, indicating search distance (meter or feet) for LiDAR data.
#' @param epsg numeric, the EPSG code specifying the coordinate reference system.
#' @param bbox vector, a bounding box defining the geographical area for downloading data.
#' @param token character, API token of Mapillary.
#' @param limit numeric, indicating the number of returns. The maximum is 2000.
#' @param fields vector, a vector of fields indicates the information of images to be
#' retrieved (See details). 'is_pano', 'thumb_256_url', 'thumb_original_url', 'height',
#' 'width', 'computed_geometry', 'computed_altitude', and 'detections'
#' are retrieved as a default setting.
#' @param ... indicating filters (see details)
#' @return For strview_searchByGeo(), a StreetscapeDataFrame returned combining
#' a dataframe of the image information.
#'
#' @seealso [available_field()] [available_filter()] [see_streetscape_class()]
#'
#' @note If there is no street view images within the search area,
#' the function only returns an integer 0.
#'
#' @details To request an API token of Mapillary,
#' please create your access token at https://mapillary.com/developer.
#' For 'fields', one can review all available fields in this package
#' by calling streetscape::field_list().
#'
#' @importFrom sp CRS
#' @importFrom mapview mapview
#'
#' @examples
#' \donttest{
#' bbox <- c(-83.751812,42.272984,-83.741255,42.279716)
#' if (isTRUE(file.exists("streetscape_token.sysdata"))) {
#'  data <- streetscape::strview_searchByGeo(bbox = bbox,
#'                                           epsg = 2253,
#'                                           token = "",
#'                                           is_pano = TRUE)
#'  data <- streetscape::strview_searchByGeo(x = -83.741289,
#'                                           y = 42.270146,
#'                                           r = 100,
#'                                           epsg = 2253,
#'                                           token = "",
#'                                           is_pano = TRUE)
#' }
#' }
#'
#' @export
#' @rdname strview_search
strview_searchByGeo <- function(x,y,r,
                                epsg,
                                bbox,
                                token= "",
                                limit= 10,
                                fields = c(),
                                ...){
  # check token
  if (token != "") {
    token <- create_check_token(token, ifsave = FALSE)
  } else if (token == "") {
    token <- create_check_token(token, ifsave = TRUE)
  }

  # check arguments
  if (missing(epsg)) {
    stop("epsg is missing. Please set epsg code")
  }
  if (length(fields) != 0 && !inherits(fields,"character")) {
    stop("fields has to be a vector of characters")
  }
  if (length(fields) > 0) {
    validate_fields(fields)
  }
  if (length(list(...)) > 0) {
    validate_filters(list(...))
  }
  suppressWarnings(
    proj <- sp::CRS(paste0("+init=epsg:", epsg))
  )
  suppressWarnings(
    longlat <- sp::CRS("+proj=longlat")
  )

  # create bbox
  if (missing(bbox)) {
    if (missing(x) || missing(y) || missing(r)) {
      stop("please specify x, y, and r, or bbox")
    } else {
      bbox <- pt2bbox(x, y, r, proj, longlat)[[1]]
    }
  }
  original_timeout <- getOption('timeout')
  on.exit(options(timeout = original_timeout), add = TRUE)
  options(timeout=9999)
  fields <- c(fields,
              c('is_pano', 'thumb_256_url', 'thumb_1024_url',
                'thumb_2048_url', 'thumb_original_url',
                'height', 'width', 'computed_geometry',
                'computed_altitude', 'detections.geometry',
                'detections.value'))
  fields <- unique(fields)
  response <- request_img_meta(token, bbox, limit, fields, ...)
  if (inherits(response, "numeric")) {
    return(0)
  }
  dfs <- 0
  tryCatch(
    dfs <- lapply(response$data, function(x) {
      xy <- longlat2proj(x$computed_geometry$coordinates[[1]],
                         x$computed_geometry$coordinates[[2]],
                         proj,
                         longlat)
      data.frame(
        id = return_field(x, 'id'),
        username = ifelse('creator' %in% names(x),
                          return_field(x$creator, 'username'), NA),
        creator_id = ifelse('creator' %in% names(x),
                            return_field(x$creator, 'id'), NA),
        make = return_field(x, 'make'),
        model = return_field(x, 'model'),
        captured_at = format(as.POSIXct(return_field(x, 'captured_at')/1000,
                                 origin="1970-01-01"),
                             format='%Y-%m-%d %H:%M:%S'),
        is_pano = x$is_pano,
        compass_angle = return_field(x, 'compass_angle'),
        computed_compass_angle = return_field(x, 'computed_compass_angle'),
        thumb_256_url = x$thumb_256_url,
        thumb_1024_url = return_field(x, 'thumb_1024_url'),
        thumb_2048_url = return_field(x, 'thumb_2048_url'),
        thumb_original_url = return_field(x, 'thumb_original_url'),
        height = x$height,
        width = x$width,
        lon = x$computed_geometry$coordinates[[1]],
        lat = x$computed_geometry$coordinates[[2]],
        coordinates.x = xy[1],
        coordinates.y = xy[2],
        altitude = return_field(x, 'altitude'),
        computed_altitude = x$computed_altitude,
        detections = I(ifelse('detections' %in% names(x),
                              return_detections(x$detections$data), NA))
      )
    }),
    error = function(e){e}
  )
  if (inherits(dfs, "numeric")) {
    return(0)
  }
  combined_df <- do.call(rbind, dfs)
  combined_df <- combined_df[!combined_df$coordinates.x == "null", ]
  if (nrow(combined_df) == 0) {
    return(0)
  }
  rownames(combined_df) <- seq(nrow(combined_df))
  if ('creator' %in% fields) {
    fields <- c('username', 'creator_id', fields)
  }
  fields <- fields[! fields%in% c("detections.geometry",
                                  "detections.value",
                                  "computed_geometry",
                                  "creator")]
  fields <- c('id','lon', 'lat',
              'coordinates.x',
              'coordinates.y',
              'detections', fields)
  combined_df <- combined_df[,fields]
  combined_df <- StreetscapeDataFrame$new(
    data = combined_df,
    epsg = epsg
  )
  return(combined_df)
}

#' @description strview_search_nnb: Search for the nearest (within 10m buffer)
#' available street view images and download meta information
#' via Mapillary API (See detials) given coordinates of a spatial point.
#' @param x numeric, indicating Longtitude degree of the center point.
#' @param y numeric, indicating latitude degree of the center point.
#' @param epsg numeric, the EPSG code specifying the coordinate reference system.
#' @param token character, API token of Mapillary.
#' @param fields vector, a vector of fields indicates the information of images
#' to be retrieved (See details). 'is_pano', 'thumb_256_url', 'height', 'width',
#' 'computed_geometry', 'computed_altitude', and 'detections' are retrieved
#' as a default setting.
#' @return For strview_search_nnb(), a StreetscapeDataFrame with one-row dataframe
#' will be returned if there is any available images near to the given point
#' @importFrom rlang .data
#' @importFrom sf st_crs
#' @importFrom dplyr filter
#' @examples
#' \donttest{
#' if (isTRUE(file.exists("streetscape_token.sysdata"))) {
#'  data <- streetscape::strview_search_nnb(
#'          x = -83.743460634278,
#'          y = 42.277848830294,
#'          epsg = 2253,
#'          token = '')
#' }
#' }
#' @export
#' @rdname strview_search
strview_search_nnb <- function(x,
                               y,
                               epsg,
                               token= "",
                               fields = c(),
                               ...){
  if (missing(x) || missing(y) || missing(epsg)) {
    stop("please specify x, y, and epsg")
  }
  suppressWarnings(
    proj <- sp::CRS(paste0("+init=epsg:", epsg))
  )
  suppressWarnings(
    longlat <- sp::CRS("+proj=longlat")
  )
  r <- 10
  epsg_unit <- sf::st_crs(epsg)$units
  if (epsg_unit == 'ft') {
    r <- 33
  }
  df <- strview_searchByGeo(x=x,y=y,r=r,
                            epsg=epsg,
                            token=token,
                            limit=5,
                            fields=fields,
                            ...)
  if (inherits(df, "numeric")) {
    return(0)
  }
  df$data$distance <- 0
  xy <- longlat2proj(x, y, proj, longlat)
  df$data$distance <- sqrt((df$data$coordinates.x-xy[1])^2 + (df$data$coordinates.y-xy[2])^2)
  data_ <- df$data
  data_ <- data_[order(data_$distance, decreasing = FALSE), ]
  data_ <- data_[,!(names(data_) %in% c('distance'))]
  df$data <- data_[1,]
  # data_ <- df$data
  # df$data <- data_ %>%
  #   dplyr::filter(.data$distance == min(data_$distance))
  return(df)
}

#' @description strview_search_osm: Search for street view images by
#' sampling locations along the OSM road lines and download meta information
#' via Mapillary API (See detials) given a bounding box.
#' @param x numeric, indicating Longtitude degree of the center point.
#' @param y numeric, indicating latitude degree of the center point.
#' @param epsg numeric, the EPSG code specifying the coordinate reference system.
#' @param token character, API token of Mapillary.
#' @param fields vector, a vector of fields indicates the information of images
#' to be retrieved (See details). 'is_pano', 'thumb_256_url', 'height', 'width',
#' 'computed_geometry', 'computed_altitude', and 'detections' are retrieved
#' as a default setting.
#' @param size numeric, (approximate) number of locations sampled on OSM spatial lines
#' (this is for strview_search_osm only).
#' @return For strview_search_osm(), a StreetscapeDataFrame that combines the information
#' of street views from all sampled points along the OSM lines within
#' the specified bounding box.
#' @examples
#' \donttest{
#' bbox <- c(-83.752041,42.274896,-83.740711,42.281945)
#' if (isTRUE(file.exists("streetscape_token.sysdata"))) {
#'  data <- streetscape::strview_search_osm(
#'          bbox = bbox,
#'          epsg = 2253,
#'          token = '',
#'          size = 100)
#' }
#' }
#' @importFrom sf st_transform as_Spatial
#' @importFrom sf st_union st_sample st_coordinates
#' @export
#' @rdname strview_search
strview_search_osm <- function(bbox,
                               epsg,
                               token,
                               fields = c(),
                               size, ...){
  # check token
  if (token != "") {
    token <- create_check_token(token, ifsave = FALSE)
  } else if (token == "") {
    token <- create_check_token(token, ifsave = TRUE)
  }

  if (missing(bbox)) {
    stop("please specify bbox")
  }
  if (missing(epsg)) {
    stop("please specify epsg")
  }
  if (missing(size)) {
    stop("please specify size")
  }
  query <- osmdata::opq(bbox = bbox, timeout = 20000) %>%
    osmdata::add_osm_feature(
      key = "highway",
      value = c('primary',
                'secondary',
                'tertiary',
                'residential')
    )
  osm_data <- try({
    osmdata::osmdata_sf(query)
  }, silent = TRUE)
  if (length(class(osm_data)) == 3) {
    lines <- osm_data$osm_lines$geometry
    lines <- sf::st_union(lines)
    suppressMessages(
      samples <- sf::st_sample(lines, size = size, type = "regular")
    )
    coords <- sf::st_coordinates(samples)
    # lines <- sf::st_transform(lines, epsg)
    # lines <- sf::as_Spatial(lines)
    # samples <- sp::spsample(lines,
    #                         n = size,
    #                         type = "regular")
    # samples <- sp::spTransform(
    #   samples,
    #   sp::CRS("+proj=longlat +datum=WGS84")
    # )
    # coords <- samples@coords
    dfs <- list()
    n <- 1
    for (i in 1:nrow(coords)) {
      df <- strview_search_nnb(x=coords[i,1],
                               y=coords[i,2],
                               epsg,
                               token = token,
                               fields,
                               ...)
      if (!inherits(df, "numeric")) {
        dfs[[n]] <- df$data
        n <- n+1
      }
    }
    if (length(dfs) == 0) {
      return(0)
    }
    combined_df <- do.call(rbind, dfs)
    combined_df <- combined_df[!duplicated(combined_df[,'id']),]
    combined_df <- StreetscapeDataFrame$new(data = combined_df,
                                            epsg = epsg)
    return(combined_df)
  } else {
    return(0)
  }
}

#' @description strview_search_multi: Search for and download the meta information
#' of street view images via Mapillary API (See detials) based on multiple coordinates
#' @param viewpoints sf or matrix, indicating multiple degress-based coordinates
#' for searching available street views (this is for strview_search_multi only).
#' @param epsg numeric, the EPSG code specifying the coordinate reference system.
#' @param token character, API token of Mapillary.
#' @param fields vector, a vector of fields indicates the information of images
#' to be retrieved (See details). 'is_pano', 'thumb_256_url', 'height', 'width',
#' 'computed_geometry', 'computed_altitude', and 'detections' are retrieved
#' as a default setting.
#' @return For strview_search_multi(), a StreetscapeDataFrame that combines the information
#' of street views based on the coordinates of multiple spatial points
#' @examples
#' \donttest{
#' x <- c(-83.752041, -83.740711)
#' y <- c(42.274896, 42.281945)
#' viewpoints <- cbind(x, y)
#' if (isTRUE(file.exists("streetscape_token.sysdata"))) {
#'  data <- streetscape::strview_search_multi(
#'          viewpoints = viewpoints,
#'          epsg = 2253,
#'          token = '')
#' }
#' }
#'
#' @export
#' @rdname strview_search
#'
strview_search_multi <- function(viewpoints,
                                 epsg,
                                 token,
                                 fields = c(),
                                 ...) {
  # check token
  if (token != "") {
    token <- create_check_token(token, ifsave = FALSE)
  } else if (token == "") {
    token <- create_check_token(token, ifsave = TRUE)
  }

  if (missing(viewpoints)) {
    stop("viewpoints is missing!")
  }
  if (!(class(viewpoints)[1] == "matrix")) {
    if (class(viewpoints)[1] == "sf") {
      viewpoints <- sf::st_coordinates(viewpoints)
    } else {
      stop("If input viewpoints is not a matrix, it has to be sf point(s)")
    }
  }
  dfs <- list()
  n <- 1
  for (i in 1:nrow(viewpoints)) {
    df <- strview_search_nnb(x=viewpoints[i,1],
                             y=viewpoints[i,2],
                             epsg,
                             token = token,
                             fields,
                             ...)
    if (!inherits(df, "numeric")) {
      dfs[[n]] <- df$data
      n <- n+1
    }
  }
  if (length(dfs) == 0) {
    return(0)
  }
  combined_df <- do.call(rbind, dfs)
  combined_df <- StreetscapeDataFrame$new(data = combined_df,
                                          epsg = epsg)
  return(combined_df)
}
