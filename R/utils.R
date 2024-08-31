#' @importFrom httr GET content status_code
#' @importFrom sp SpatialPoints spTransform
#' @importFrom dplyr %>%
#' @importFrom cli cli_ul

#' @noMd
longlat2proj <- function(x, y, proj, longlat) {
  #coor <- data.frame(lon=as.numeric(x), lat=as.numeric(y))
  if (is.null(x) || is.null(y)) {
    return(c(NULL, NULL))
  }
  coor <- matrix(c(x,y), 1)
  pt <- sp::SpatialPoints(coor, proj4string=longlat)
  pt <- sp::spTransform(pt, proj)
  return(c(pt@coords[1,1], pt@coords[1,2]))
}

#' @noMd
pt2bbox <- function(x, y, r, proj, longlat){
  xy <- longlat2proj(x, y, proj, longlat)
  xmin <- xy[1] - r
  xmax <- xy[1] + r
  ymin <- xy[2] - r
  ymax <- xy[2] + r
  coor_ <- data.frame(lon=c(xmin, xmax), lat=c(ymin, ymax))
  pt_ <- sp::SpatialPoints(coor_, proj)
  pt_ <- sp::spTransform(pt_, CRSobj=longlat)
  return(list(c(pt_@coords[1,1],
                pt_@coords[1,2],
                pt_@coords[2,1],
                pt_@coords[2,2]),
              c(xmin, ymin, xmax, ymax)))
}

#' @noMd
convertBbox <- function(bbox, proj, longlat) {
  coor <- data.frame(lon=c(bbox[1],bbox[3]),
                     lat=c(bbox[2],bbox[4]))
  pt <- sp::SpatialPoints(coor, proj4string=longlat)
  pt <- sp::spTransform(pt, proj)
  xmin <- pt@coords[1,1]
  ymin <- pt@coords[1,2]
  xmax <- pt@coords[2,1]
  ymax <- pt@coords[2,2]
  return(list(bbox,c(xmin, ymin, xmax, ymax)))
}

#' @noMd
get_filters <- function(...) {
  input_components <- list(...)
  m <- cbind(names(input_components),
             as.vector(input_components))
  m_list <- as.list(data.frame(t(m)))
  filters <- lapply(m_list, function(x){
    sprintf('%s=%s',x[[1]], x[[2]])
  })
  filters <- paste(as.vector(filters), collapse="&")
  return(filters)
}

#' @noMd
create_check_token <- function(api_token, ifsave = TRUE) {
  if (api_token == "") {
    if (isFALSE(file.exists("streetscape_token.sysdata"))) {
      cli::cli_ul(
        "Create a Mapillary API Token at https://www.mapillary.com/developer/api-documentation")
      utils::browseURL("https://www.mapillary.com/developer/api-documentation")
      cli::cli_ul("Enter your Mapillary API Token:")

      api_token <- readline()

    } else {

      api_token <- utils::read.table("streetscape_token.sysdata", stringsAsFactors = FALSE)[1, 1]
    }
  }

  # check token
  base_url <- paste("https://graph.mapillary.com/images?access_token=",
                    api_token,
                    sep = "")

  if (httr::GET(base_url)$status != 200) {
    return_message <- httr::GET(base_url) %>% httr::content()
    stop(paste0('code:', return_message$error$code, '. ',
                return_message$error$message, '. ',
                "Invalid API Token"))
  } else if (httr::GET(base_url)$status == 200) {
    if (isFALSE(file.exists("streetscape_token.sysdata"))) {
      # cli::cli_ul("Save your Mapillary API Token? y (yes) / n (no)")
      if (ifsave) {
        cli::cli_ul("You can save your Mapillary API Token so that you do not need to specify it every time call the function")
        ifsave <- readline("Save your Mapillary API Token??? y (yes) / n (no): ")
        if (ifsave == 'y' | ifsave == 'yes') {
          # save token
          utils::write.table(api_token,
                             file = "streetscape_token.sysdata",
                             col.names = FALSE,
                             row.names = FALSE)
        }
      }
    }
    return(api_token)
  }
}

#' @noMd
request_img_meta <- function(token, bbox, limit, fields, ...) {
  bbox <- sprintf('%s,%s,%s,%s',
                  bbox[1],
                  bbox[2],
                  bbox[3],
                  bbox[4])
  limit <- sprintf('limit=%s', limit)
  fields <- paste(fields, collapse=",")
  filters <- get_filters(...)
  url <- sprintf(
    'https://graph.mapillary.com/images?fields=%s&access_token=%s&bbox=%s&%s',
    fields, token, bbox, limit
  )
  if (filters != "") {
    url <- paste0(url, "&", filters)
  }
  response <- httr::GET(url) %>%
    httr::content()
  # Check if the response is empty
  if (length(response$data) == 0 || identical(response$data, list())) {
    return(0)
  }
  return(response)
}

#' @noMd
return_field <- function(list, field_name) {
  if (field_name %in% names(list)) {
    return(list[[field_name]])
  }
  return(NA)
}

#' @noMd
return_detections <- function(data) {
  labels <- c()
  geos <- c()
  for (i in 1:length(data)) {
    labels <- c(labels, data[[i]]$value)
    geos <- c(geos, data[[i]]$geometry)
  }
  geos <- as.list(geos)
  names(geos) <- labels
  return(list(geos))
}

#' @noMd
return_mask <- function(id, token) {
  url <- sprintf(
    'https://graph.mapillary.com/%s/detections?access_token=%s&fields=value,geometry',
    id, token
  )
  response <- httr::GET(url) %>%
    httr::content()
  if (length(response$data) == 0 || identical(response$data, list())) {
    return(0)
  }
  return(response)
}

#' @title install_mvt
#' @description install_mvt is a wrapped function of py_install
#' in the reticulate package for installing the python package
#' mapbox_vector_tile, which will be installed in a virtual environment -
#' "r-mvt".
#' @param envname The name, or full path, of the environment
#' in which Python packages are to be installed.
#' @param method character, indicating installation method.
#' @return None
#' @importFrom reticulate py_install virtualenv_exists virtualenv_remove
#' @export
install_mvt <- function(envname = "r-mvt", method = "auto") {
  new_env <- identical(envname, "r-mvt")
  if (new_env && reticulate::virtualenv_exists(envname)) {
    reticulate::virtualenv_remove(envname, confirm = FALSE)
  }
  reticulate::py_install(
    "mapbox_vector_tile",
    envname = envname,
    method = method,
    python_version = as.character(reticulate::py_config()['version']),
    pip = TRUE
  )
  # system(paste(reticulate::py_discover_config()[['python']],'-m pip install',
  #              paste(pkgs,collapse = ' ')
  # ))
}

#' @title decode_detections
#' @description convert Mapillary object detection into sf polygons
#' @param detections_string character, an endcoded string of semantic segmentation,
#' for example, "Gmt4AgoGbXB5L=="
#' @return sf polygon
#' @importFrom reticulate import py
#' @importFrom sf st_polygon st_sf st_set_crs st_sfc
#' @examples
#' \donttest{
#' detection <- readLines(system.file('detection.txt', package = 'streetscape'))
#' streetscape::decode_detections(detection)
#' }
#' @export
decode_detections <- function(detections_string) {
  if (reticulate::py_module_available("mapbox_vector_tile")){
    mvt <- reticulate::import("mapbox_vector_tile",
                              convert = TRUE)
  } else {
    message <- paste0("please install python package mapbox_vector_tile",
                      "by calling streetscape::install_mvt().",
                      "Then, restart R session and reload the streetscape package")
    stop(message)
  }
  decode_string <- enc2utf8(detections_string)
  cmd <- sprintf("import base64; decoded_data = base64.b64decode('%s')",
                 decode_string)
  reticulate::py_run_string(cmd)
  decode_geometry <- mvt$decode(reticulate::py$decoded_data)
  coordinates <- decode_geometry[[1]]$features[[1]]$geometry$coordinates
  if (length(coordinates) == 0 ) {
    return(0)
  }
  coords <- coordinates[[1]]
  coords_matrix <- do.call(rbind, coords)
  polygon <- sf::st_polygon(list(coords_matrix))
  polygon <- sf::st_sfc(polygon)
  #polygon_object <- sf::st_sf(geometry = sf::st_sfc(polygon))
  #polygon_object <- sf::st_set_crs(polygon_object, 4326)
  return(polygon)
}

#' @noMd
# calculate total percentage of each semantic segmentation
# for a street view image
segmentation_calculator <- function(sf_polygons) {
  out <- lapply(sf_polygons, function(x){
    if (inherits(x, "logical")) {
      return(NA)
    } else {
      polydf <- do.call(rbind, x)
      polydf <- data.frame(label = polydf$label,
                           percentage = polydf$percentage)
      res <- polydf %>%
        dplyr::group_by(.data$label) %>%
        dplyr::summarise(total_percentage = sum(.data$percentage,
                                                na.rm = TRUE))
      res <- I(list(res))
      return(res)
    }
  })
  out <- do.call(rbind, out)
  out <- as.data.frame(out)
  names(out) <- 'segment_perc'
  return(out)
}

#' @noMd
read_segment_perc <- function(segment_perc) {
  segmentations <- c()
  for (i in 1:length(segment_perc)) {
    if(inherits(segment_perc[[i]], "logical")) {
      segmentations <- c(segmentations, NA)
    } else {
      seg <- paste(
        "<div>",
        segment_perc[[i]]$label,
        ": ",
        segment_perc[[i]]$total_percentage,
        "</div>")
      seg <- paste(seg, collapse="")
      segmentations <- c(segmentations, seg)
    }
  }
  return(segmentations)
}

#' @noMd
validate_fields <- function(input_fields) {
  valid_fields <- as.vector(available_field()[[1]])
  invalid_fields <- base::setdiff(input_fields,
                                  valid_fields)
  if (length(invalid_fields) > 0) {
    stop("Invalid field(s): ",
         paste(invalid_fields, collapse = ", "))
  }
}

#' @noMd
validate_filters <- function(input_filters) {
  valid_filters <- as.vector(available_filter()[,1])
  invalid_filters <- base::setdiff(names(input_filters),
                                   valid_filters)
  if (length(invalid_filters) > 0) {
    stop("Invalid filter(s): ",
         paste(invalid_filters, collapse = ", "))
  }
}

#' @importFrom OpenImageR readImage
#' @noMd
img2gvi <- function(url) {
  #--------------------Segment street view-----------------------
  img_segmentation <- g_seg(url)
  #--------------------GreenView_Calculate-----------------------
  if (inherits(img_segmentation, "numeric")) {
    return(img_segmentation)
  }
  return(gv_calc(img_segmentation))
}

#' @noMd
g_seg <- function(url) {
  # download image
  original_timeout <- getOption('timeout')
  on.exit(options(timeout = original_timeout), add = TRUE)
  options(timeout=9999)
  temp_file <- tempfile(fileext = ".jpg")

  #writeBin(url, temp_file)
  # mode <- 'w'
  # if (isTRUE(Sys.info()[1]=="Windows") == TRUE){
  #   mode <- 'wb'
  # }

  # check url
  response <- httr::GET(url)
  if (httr::status_code(response) != 200) {
    return(0)
  }
  if (isTRUE(Sys.info()[1]=="Windows") == TRUE){
    download.file(url, temp_file, method='auto', quiet = TRUE, mode = 'wb')
  } else {
    download.file(url, temp_file, method='auto', quiet = TRUE)
  }
  # read and segment the image
  img <- OpenImageR::readImage(temp_file)
  init <- SuperpixelImageSegmentation::Image_Segmentation$new()
  suppressWarnings({
    ss <- init$spixel_segmentation(
      input_image = img,
      superpixel = 600,
      AP_data = TRUE,
      use_median = TRUE,
      sim_wL = 3,
      sim_wA = 10,
      sim_wB = 10,
      sim_color_radius = 3,
      verbose = FALSE
    )
  })
  #OpenImageR::imageShow(ss$AP_image_data)
  on.exit(unlink(temp_file), add = TRUE)
  return(ss$AP_image_data)
}

# compute GVI using algorithm based on comparing difference between G and B,G
#' @noMd
gv_calc <- function(img_segmentation) {
  total <- dim(img_segmentation)[1] * dim(img_segmentation)[2]
  # get R, G, and B color bands
  R <- img_segmentation[,,1]
  G <- img_segmentation[,,2]
  B <- img_segmentation[,,3]
  redThreImgU <- R < 0.6
  greenThreImgU <- G < 0.9
  blueThreImgU <- B < 0.6
  shadowRedU <- R < 0.3
  shadowGreenU <- G < 0.3
  shadowBlueU <- B < 0.3
  threImgU <-  redThreImgU * blueThreImgU * greenThreImgU
  imgShadow <- shadowRedU * shadowGreenU * shadowBlueU
  # calculate the difference between G band with others
  g_r_dif <- G - R
  g_b_dif <- G - B
  ExG <- (g_r_dif + g_b_dif)/(G + R + B)
  # v <- as.vector(ExG)
  # v <- v[!is.na(v)]
  #threshold <- max(v)
  greenImg <- (ExG > 0.05)*threImgU + (ExG > 0.05)*imgShadow
  greenNum <- length(which(greenImg != 0))
  #greenNum <- length(which(ExG == threshold))
  # raster::plot(raster::raster(ExG > 0.05))
  remove(greenImg)
  return(greenNum/total)
}

#' @noMd
get_choices <- function(choices) {
  out <- ''
  for(i in 1:length(choices)) {
    out <- paste(out, '[[Choice]]', choices[i],sep='\n')
  }
  return(out)
}

#' @noMd
make_questions <- function(questions, choices, id, binary){
  #label <- sprintf('[[Question:MC:SingleAnswer:Horizontal:%s]]', id)
  out <- c()
  if (length(questions) != length(choices) && !binary) {
    stop('The lengths of questions and choices do not match')
  }
  for (i in 1:length(questions)) {
    out <- paste(
      out,
      '[[Question:MC:SingleAnswer:Horizontal]]',
      sprintf('[[ID:%s_%s]]',id, i),
      questions[i],
      "[[AdvancedChoices]]",
      if (!binary) {
        get_choices(choices[[i]])
      } else {
        paste("[[Choice]]", choices[1],
              "[[Choice]]", choices[2], sep='\n')
      },
      sep = '\n'
    )
  }
  return(out)
}

#' @noMd
set_workers <- function() {
  num_workers <- as.numeric(parallelly::availableCores()-1)
  num_workers <- Sys.getenv("USE_CORES", num_workers)
  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(chk) && chk == "TRUE") {
    num_workers <- 1
  }
  if (isTRUE(Sys.info()[1]=="Windows") == TRUE){
    num_workers <- 1
  }
  return(num_workers)
}

#' @importFrom pbmcapply pbmclapply
#' @importFrom parallelly availableCores
#' @noMd
img_survey <- function(data, header, questions, choices){
  num_workers <- set_workers()
  header_label <- "[[Question:DB]]"
  body <- pbmcapply::pbmclapply(
    1:nrow(data$data),
    function(x){
      img <- paste0("<img src=", "'", data$data$thumb_original_url[x], "'>")
      q <- make_questions(questions,
                          choices,
                          data$data$id[x],
                          binary = FALSE)
      paste(
        header_label,
        header,
        img,
        q,
        "[[PageBreak]]\n",
        sep = '\n'
      )
    },
    mc.cores = num_workers
  )
  body <- do.call(paste, body)

  body <- paste(
    "[[AdvancedFormat]]",
    "[[Block:MC Block]]",
    body,
    sep = '\n'
  )
  return(body)
}

#' @noMd
img_survey_pwc <- function(data, header, questions){
  num_workers <- set_workers()
  choices <- c('left', 'right')
  header_label <- "[[Question:DB]]"
  body <- pbmcapply::pbmclapply(
    1:nrow(data),
    function(x){
      img1 <- paste0("<img src=", "'", data[x,1],
                     "'style='flex: 1; object-fit: cover; width:50%'>")
      img2 <- paste0("<img src=", "'", data[x,2],
                     "'style='flex: 1; object-fit: cover; width:50%'>")
      img <- paste0("<div style='display: flex;flex-direction: row;'>",
                    img1, img2, "</div>")
      id <- paste0(data[x,3], '_', data[x,4])
      q <- make_questions(questions, choices, id, binary = TRUE)
      paste(
        header_label,
        header,
        img,
        q,
        "[[PageBreak]]\n",
        sep = '\n'
      )
    },
    mc.cores = num_workers
  )
  body <- do.call(paste, body)

  body <- paste(
    "[[AdvancedFormat]]",
    "[[Block:MC Block]]",
    body,
    sep = '\n'
  )
  return(body)
}
