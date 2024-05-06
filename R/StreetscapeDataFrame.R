#' @title Streetscape-Class
#' @description The output of strview_search family
#' functions is constructed in this data format -
#' A specialized data frame for streetscape package for
#' initializing the object with streetscape data and
#' extracting and decoding segmentation information
#' of streetscape dataframe.
#' @name Streetscape-class
#' @docType class
#' @importFrom utils download.file
#' @exportClass StreetscapeDataFrame
#' @field data A data frame containing metadata of Mapillary
#' street view images
#' @field epsg A numeric epsg code
#' @section Class Methods:
#' \subsection{Method list}{
#'  \itemize{
#'   \item \href{#method-decodeDetection}{\code{StreetscapeDataFrame$decodeDetection()}}
#'   \item \href{#method-gvi}{\code{StreetscapeDataFrame$gvi()}}
#'   \item \href{#method-get_mask}{\code{StreetscapeDataFrame$get_mask()}}
#'   \item \href{#method-mapPreview}{\code{StreetscapeDataFrame$mapPreview()}}
#'   \item \href{#method-download_data}{\code{StreetscapeDataFrame$download_data()}}
#'  }
#' }
#' \if{html}{\out{<hr>}}
#' \if{html}{\out{<a id="method-decodeDetection"></a>}}
#' \subsection{Method \code{decodeDetection()}}{
#'  \subsection{Usage}{
#'  \code{scdataframe$decodeDetection()}
#'  }
#' }
#' \if{html}{\out{<hr>}}
#' \if{html}{\out{<a id="method-gvi"></a>}}
#' \subsection{Method \code{gvi()}}{
#'  \subsection{Usage}{
#'  \code{scdataframe$gvi(level = 1)}
#'  }
#'  \subsection{Arguments}{
#'   \describe{
#'   \item{\code{level}}{numeric, indicating the resolution level of images
#'   for calculating the green view index.
#'   1 - the 256px wide thumbnail;
#'   2 - the 1024px wide thumbnail;
#'   3 - the 2048px wide thumbnail;
#'   4 - the original wide thumbnail.
#'   The default is level = 1
#'   }
#'   }
#'  }
#' }
#' \if{html}{\out{<hr>}}
#' \if{html}{\out{<a id="method-get_mask"></a>}}
#' \subsection{Method \code{get_mask()}}{
#'  \subsection{Usage}{
#'  \code{scdataframe$get_mask(index = 1)}
#'  }
#'  \subsection{Arguments}{
#'   \describe{
#'   \item{\code{index}}{numeric, the row index of the dataframe
#'   of StreetscapeDataFrame class}
#'   }
#'  }
#' }
#' \if{html}{\out{<hr>}}
#' \if{html}{\out{<a id="method-mapPreview"></a>}}
#' \subsection{Method \code{mapPreview()}}{
#'  \subsection{Usage}{
#'  \code{scdataframe$mapPreview(maptype = 'meta')}
#'  }
#'  \subsection{Arguments}{
#'   \describe{
#'   \item{\code{maptype}}{character or character,
#'   specifying what type of information to be mapped:
#'   'meta' - image meta,
#'   'seg' - segmentation proportion,
#'   and 'gvi' - GVI".}
#'   \item{\code{fields}}{vector (optional), a vector of fields
#'   indicates the information of images to be included
#'   for the 'meta' map. The fields of 'id', 'is_pano',
#'   'height', 'width', 'lon', and 'lat' are already included}
#'   }
#'  }
#' }
#' \if{html}{\out{<hr>}}
#' \if{html}{\out{<a id="method-download_data"></a>}}
#' \subsection{Method \code{download_data()}}{
#'  \subsection{Usage}{
#'  \code{scdataframe$download_data(path = 'path/to/download',
#'  items = c('image', 'mask'))}
#'  }
#'  \subsection{Arguments}{
#'   \describe{
#'   \item{\code{path}}{character, directory for downloading street view images
#'   or segmentation masks or both}
#'   \item{\code{items}}{character or vector, specifying what to download:
#'   'image' - 'original street view image;
#'   'mask' - semantic segmentation (sf objects in .geojson format)"}
#'   }
#'  }
#' }
#' @importFrom methods setRefClass
#' @importFrom dplyr mutate
#' @importFrom graphics hist
#' @importFrom methods new
#' @importFrom utils tail
#' @importFrom utils ?

StreetscapeDataFrame <- setRefClass(
  "StreetscapeDataFrame",
  fields = list(
    data = "data.frame",
    epsg = "numeric"
  ),
  methods = list(
    initialize = function(data, epsg) {
      .self$data <- data
      .self$epsg <- epsg
    },
    mapPreview = function(maptype = 'meta', fields = c()) {
      "Plot data points in an ineractive map view"
      if (missing(maptype)) {
        stop("please specify maptype")
      }
      # if (!(maptype %in% c('meta', 'seg', 'gvi'))) {
      #   stop("maptype has to be 'meta', 'seg', or 'gvi'")
      # }
      # map meta view
      if (length(base::setdiff('meta', maptype)) == 0) {
        # validate fields
        if (length(fields) > 0) {
          validate_filters()
        }
        suppressWarnings(
          view1 <- sp::SpatialPointsDataFrame(
            coords = .self$data[, c('coordinates.x', 'coordinates.y')],
            data = .self$data[, unique(c('id', 'is_pano', 'height',
                                         'width', 'lon', 'lat',
                                         fields))],
            proj4string = sp::CRS(paste0("+init=epsg:", .self$epsg))
          )
        )
        view1@data$image_link <- sprintf(
          "<a href='https://www.mapillary.com/embed?image_key=%s&style=photo'>click</a>",
          .self$data$id
        )
        if (length(maptype) == 1) {
          map <- mapview::mapview(view1, legend = TRUE)
          return(map)
        }
      }

      if (length(base::setdiff('seg', maptype)) == 0) {
        if ('segment_perc' %in% names(.self$data)) {
          suppressWarnings(
            view2 <- sp::SpatialPointsDataFrame(
              coords = .self$data[, c('coordinates.x', 'coordinates.y')],
              data = data.frame(id = .self$data[, 'id']),
              proj4string = sp::CRS(paste0("+init=epsg:", .self$epsg))
            )
          )
          view2@data$segmentation <- sprintf(
            "<div>%s</div>",
            read_segment_perc(.self$data$segment_perc)
          )
          view2@data$image_link <- sprintf(
            "<a href='https://www.mapillary.com/embed?image_key=%s&style=photo'>click</a>",
            .self$data$id
          )
          if (length(maptype) == 1) {
            map <- mapview::mapview(view2, legend = TRUE)
            return(map)
          }
        } else {
          stop("please call method 'decodeDetection()'
               before call this method with maptype 'seg'")
        }
      }

      if (length(base::setdiff('gvi', maptype)) == 0) {
        if ('GVI' %in% names(.self$data)) {
          suppressWarnings(
            view3 <- sp::SpatialPointsDataFrame(
              coords = .self$data[, c('coordinates.x', 'coordinates.y')],
              data = .self$data[, c('id','GVI')],
              proj4string = sp::CRS(paste0("+init=epsg:", .self$epsg))
            )
          )
          view3@data$image_link <- sprintf(
            "<a href='https://www.mapillary.com/embed?image_key=%s&style=photo'>click</a>",
            .self$data$id
          )
          if (length(maptype) == 1) {
            map <- mapview::mapview(view3, zcol="GVI", legend = TRUE)
            return(map)
          }
        } else {
          stop("please call method 'gvi()'
               before call this method with maptype 'gvi'")
        }
      }
      # data_ <- .self$data
      # images@data <- data_ %>%
      #   dplyr::mutate(image_link = paste0("<a href='", .data$thumb_256_url,"'>", "click", "</a>"))
    },
    decodeDetection = function() {
      "Regenerate a dataframe with decoded segmentation.
      'detections' column will be updated and
      a new column 'segmentation' will be added."
      # check if mapbox_vector_tile module is available
      if (reticulate::py_module_available("mapbox_vector_tile")){
        # import the package
        mvt <- reticulate::import("mapbox_vector_tile",
                                  convert = TRUE)
      } else {
        message <- paste0(
          "1. please install python package 'mapbox_vector_tile'",
          "by calling streetscape::install_mvt(). ",
          "2. Then, restart R session and reload the streetscape package"
        )
        stop(message)
      }
      # read dataframe as a list
      data_list <- split(.self$data,seq(nrow(.self$data)))
      # create a new list with decoded segmentation
      new_data_list <- lapply(data_list, function(x) {
        detections <- x$detections[[1]]
        if (inherits(detections, "logical")) {
          out <- data.frame(
            segmentation = NA
          )
          return(out)
        } else {
          decoded_segmentation <- list()
          # decoded segmentation
          for (i in 1:length(detections)) {
            detections_string <- detections[[i]]
            decode_string <- enc2utf8(detections_string)
            cmd <- sprintf(
              "import base64; decoded_data = base64.b64decode('%s')",
              decode_string
            )
            reticulate::py_run_string(cmd)
            # decode
            decode_geometry <- mvt$decode(reticulate::py$decoded_data)
            #coordinates <- decode_geometry[[1]]$features[[1]]$geometry$coordinates
            extent <- decode_geometry[[1]]$extent
            if (length(decode_geometry) == 0 || is.null(decode_geometry[[1]]$features)) {
              next  # Skip if no features found
            }

            features <- decode_geometry[[1]]$features
            if (length(features) == 0 || is.null(features[[1]]$geometry$coordinates)) {
              next  # Skip if coordinates are missing
            }

            coordinates <- features[[1]]$geometry$coordinates
            if (length(coordinates) == 0 || is.null(coordinates[[1]])) {
              next  # Skip if first coordinate is missing
            }
            coords <- coordinates[[1]]
            # Normalize and transform coordinates
            coords <- lapply(coords, function(coord) {
              c(x = (coord[1] / extent) * x$width,
                y = (coord[2] / extent) * x$height)
            })
            coords_matrix <- do.call(rbind, coords)
            polygon <- sf::st_polygon(list(coords_matrix))
            polygon <- sf::st_sfc(polygon)
            polygon_area <- sf::st_area(polygon)
            polygon_object <- sf::st_sf(
              geometry = polygon,
              percentage = polygon_area/(x$width*x$height),
              label = names(detections)[i]
            )
            # polygon_object <- sf::st_set_crs(polygon_object, 4326)
            decoded_segmentation[[i]] <- polygon_object
          }
          out <- data.frame(
            segmentation = I(list(decoded_segmentation))
          )
          return(out)
        }
      })
      combined_df <- do.call(rbind, new_data_list)
      rownames(combined_df) <- seq(nrow(combined_df))
      # combined_df <- combined_df[,names(.self$data)]
      # calculate percentage of each type of semantic segmentation
      segment_perc <- segmentation_calculator(combined_df$segmentation)
      .self$data <- cbind(.self$data, combined_df, segment_perc)
      #summary(.self$data )
    },
    gvi = function(level) {
      "Calculate green view index (GVI) for each collected image by
      segmenting green pixels and quantifing the percentage in
      street view images. This method adds a new column of
      greeness percetage to the dataframe"
      if (missing(level)) {
        level <- 1
      }
      image_resolution <- c('thumb_256_url', 'thumb_1024_url',
                            'thumb_2048_url', 'thumb_original_url')
      # initialize a gvi column
      .self$data$GVI <- 0
      for (i in 1:nrow(.self$data)) {
        url <- .self$data[i, image_resolution[level]]
        .self$data[i,'GVI'] <- img2gvi(url)
      }
    },
    get_mask = function(index) {
      "Convert the semantic segmentation of a street view image
      from the StreetscapeDataFrame into sf polygons"
      if(missing(index)) {
        stop("please specify index")
      }
      # extract sf polygons
      code <- .self$data$detections[index]
      if (inherits(code[[1]], "logical")) {
        return(list('No semantic segmentation for this image'))
      } else {
        code <- code[[1]]
        polygons <- lapply(
          1:length(code),
          function(x) {
            polygon <- decode_detections(code[[x]])
            if (!inherits(polygon, 'numeric')) {
              polygon <- sf::st_sf(
                geometry = polygon,
                label = names(code[x])
              )
            } else {
              NA
            }
          }
        )
        polygons <- polygons[!is.na(polygons)]
      }
      polygons <- do.call(rbind, polygons)
      return(polygons)
    },
    download_data = function(path, items) {
      "Download street view images (and segmentations in sf format if applicable)"
      if(missing(path)) {
        return("please specify path")
      }
      if(missing(items)) {
        return("please specify items")
      }
      # set up folders
      f_dir <- paste0(path, "/sc_data")
      img_dir <- paste0(path, "/sc_data/images")
      mask_dir <- paste0(path, "/sc_data/masks")
      dir.create(file.path(f_dir))
      if (length(base::setdiff(items, c('image','mask'))) == 0) {
        if (length(items) == 1) {
          if (items == 'image') {
            dir.create(file.path(img_dir))
          } else if (items == 'mask') {
            dir.create(file.path(mask_dir))
          }
        } else {
          dir.create(file.path(img_dir))
          dir.create(file.path(mask_dir))
        }
      } else {
        stop("'items' has to be 'image', 'mask' or c('image', 'mask')")
      }
      # read dataframe as a list
      data_list <- split(.self$data,seq(nrow(.self$data)))
      num_workers <- set_workers()
      # download
      suppressWarnings(
        pbmcapply::pbmclapply(
          1:length(data_list),
          function(x){
            this_row <- data_list[[x]]
            if ("image" %in% items) {
              # download images
              try(download.file(
                this_row$thumb_original_url,
                paste0(img_dir, "/", this_row$id, "_img.png"),
                method = 'auto',
                quiet = TRUE)
              )
            }
            if ("mask" %in% items) {
              # download masks
              m <- .self$get_mask(x)
              if (inherits(m, "sf")) {
                sf::write_sf(m, dsn=paste0(mask_dir, "/", this_row$id, "_mask.geojson"))
                #save(m, file=paste0(mask_dir, "/", this_row$id, "_mask.RData"))
              }
            }
            return(TRUE)
          },
          mc.cores = num_workers
        )
      )
      return('Completed')
    }
  )
)
