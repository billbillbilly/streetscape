#' strview2qualtrics
#' @name strview2qualtrics
#' @title strview2qualtrics

#' @description strview2rate: pack street views as a Qualtrics survey file that
#' can be imported to Qualtrics platform
#' @param df StreetscapeDataFrame
#' @param header character, indicating the task for a question.
#' For example, "Please review the following picture:"
#' @param questions vector, a list of questions (see details)
#' @param choices list, a list of choices (see details)
#' (this is only for strview2rate)
#' @param file character, indicating the directory and file name
#' (without extension) for saving the Qualtrics survey file
#' @details For strview2rate(), the lengths of questions and choices must match.
#' For example, when questions = c('1. To what existence you can feel pleasant
#' if you were in this environment', '2. To what existence you can feel safe
#' if you were in this environment'), choices could be list(c('Unpleasant',
#' 'Less pleasant', 'More pleasant', 'Pleasant'), c('Unsafe', 'Less safe', 'Safer',
#' 'Safe'))
#' For strview2pwc, the choices are always c('left', 'right') for the coparison
#' purposes.
#'
#' @examples
#' data('scdataframe')
#' header <- "Please review the following picture(s):"
#' questions <- c('1. To what extent you feel pleasant if you were in this environment',
#'               '2. To what extent you feel safe if you were in this environment')
#' choices <- list(c('Unpleasant','Less pleasant', 'Pleasant', 'More pleasant'),
#'                 c('Unsafe', 'Less safe','Safe', 'Safer'))
#' txt <- streetscape::strview2rate(scdataframe, header, questions, choices)
#'
#' @return character if argument 'file' is not specified
#'
#' @export
#' @rdname strview2qualtrics

strview2rate <- function(df,
                         header,
                         questions,
                         choices,
                         file) {
  txt <- img_survey(df, header, questions, choices)
  if (missing(file)) {
    return(txt)
  } else {
    writeLines(txt, paste0(file, ".txt"))
  }
}

#' @description strview2pwc: pack street views as a Qualtrics survey file for
#' pair-wised comparison
#' @param k numeric, indicating how many street views each street view will
#' be paired with for pair-wised comparison (this is only for strview2pwc)
#' @return character if argument 'file' is not specified
#' @examples
#' data('scdataframe')
#' header <- "Please review the following picture(s):"
#' questions <- 'which one is more beautiful?'
#' txt <- streetscape::strview2pwc(scdataframe, k=1, header, questions)
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr join_by
#' @importFrom dplyr %>%
#' @export
#' @rdname strview2qualtrics
#'
strview2pwc <- function(df,
                        k,
                        header,
                        questions,
                        file
                        ) {
  pw <- quickPWCR::randompair(unique(df$data$thumb_original_url), k)
  pw <- pw %>%
    dplyr::left_join(df$data[, c('id','thumb_original_url')], by=c('left' = 'thumb_original_url')) %>%
    dplyr::left_join(df$data[, c('id','thumb_original_url')], by=c('right' = 'thumb_original_url'))
  names(pw) <- c('left', 'right', 'left.id', 'right.id')
  txt <- img_survey_pwc(pw, header, questions)
  if (missing(file)) {
    return(txt)
  } else {
    writeLines(txt, paste0(file, ".txt"))
  }
}
