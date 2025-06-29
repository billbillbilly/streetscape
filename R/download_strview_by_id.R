#' download_stv_by_id
#' @description Download Mapillary street view images and retrieve their metadata
#' by image id. The image url retrieved by [strview_searchByGeo()],
#' [strview_search_nnb()], or [strview_search_osm()] will expire after a period
#' of time.
#'
#' @param ids numeric or character or vector.
#' @param fields vector, Additional fields of image metadata to retrieve.
#' Default includes: 'is_pano', 'height', 'width', 'computed_geometry',
#' 'computed_altitude', and 'detections'.
#' @param token character, API token of Mapillary.
#' @param download logical. Whether to download the images.
#' @param size numeric. The size of images to download:
#' \itemize{
#'   \item 1 - 256px wide thumbnail
#'   \item 2 - 1024px wide thumbnail
#'   \item 3 - 2048px wide thumbnail
#'   \item 4 - Original size thumbnail
#' }
#' Default is 1.
#' @param img_dir character. The directory for downloading street view images.
#' Required if \code{download = TRUE}
#' @param parallel logical. Whether to parallelize the download process for speed.
#' The default is `FALSE`.
#'
#' @return A data.frame with metadata of the requested image IDs.
#' If \code{download = TRUE}, the selected image sizes will be saved in
#' \code{img_dir}.
#'
#' @note More information about fields at
#' \url{https://www.mapillary.com/developer/api-documentation}
#'
#' @examples
#' \donttest{
#' ids <- c(527765141928031, 1437871829899771)
#' download_stv_by_id(ids = ids, token = 'token')
#' }
#'
#' @importFrom cli cli_progress_update cli_progress_done cli_alert_success cli_alert_info cli_progress_bar
#' @export
download_stv_by_id <- function(ids = NULL,
                               fields = c(),
                               token = NULL,
                               download = FALSE,
                               size = 1,
                               img_dir = NULL,
                               parallel = FALSE){
  if (is.null(ids)) stop('id is missing')
  if (is.null(token)) stop('token is missing')

  img_sizes <- c('thumb_256_url',
                'thumb_1024_url',
                'thumb_2048_url',
                'thumb_original_url')
  size <- img_sizes[size]
  fields <- c(fields, img_sizes)

  original_timeout <- getOption('timeout')
  on.exit(options(timeout = original_timeout), add = TRUE)
  options(timeout=9999)

  cli::cli_alert_info('Searching for data by ids...')
  if (length(ids) == 1) {
    response <- request_img_by_id(ids, fields, token)
    response <- do.call(cbind, response)
  } else if (length(ids) > 1) {
    res <- list()
    cli::cli_progress_bar('Searching', total = length(ids))
    for (i in 1:length(ids)) {
      cli::cli_progress_update()
      response <- request_img_by_id(ids[i], fields, token)
      response <- do.call(cbind, response)
      res[[length(res)+1]] <- response
    }
    cli::cli_progress_done()
    response <- do.call(rbind, res)
  }

  if (download) {
    if (is.null(img_dir)) {
      base::warning("Missing path - 'img_dir' for saving images")
    } else {
      d_mode <- if (Sys.info()[["sysname"]] == "Windows") "wb" else "auto"
      urls <- response[, size]
      if (!parallel) {
        cli::cli_progress_bar("Downloading", total = length(urls))
        for (i in 1:length(urls)) {
          cli::cli_progress_update()
          utils::download.file(
            urls[i],
            paste0(img_dir, "/", response[i,'id'], "_img.png"),
            method = d_mode,
            quiet = TRUE
          )
        }
        cli::cli_progress_done()
      } else {
        suppressWarnings(
          pbmcapply::pbmclapply(
            1:length(urls),
            function(x){
              utils::download.file(
                urls[x],
                paste0(img_dir, "/", response[x,'id'], ".png"),
                method = d_mode,
                quiet = TRUE
              )
            },
            mc.cores = set_workers()
          )
        )
      }
      cli::cli_alert_success('Finished downloading images')
    }
  }
  return(as.data.frame(response))
}

