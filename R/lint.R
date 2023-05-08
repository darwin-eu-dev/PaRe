#' lintRepo
#'
#' @param repo (`Repository`)
#'   Repository object.
#'
#' @return (`data.frame()`)
#'   Data.frame containing lint messages.
#' @export
lintRepo <- function(repo) {
  tempDir <- tempdir()
  tempFile <- tempfile()

  files <- repo$getRFiles()

  messages <- dplyr::bind_rows(lapply(files, function(file) {
    tempFile <- tempfile(pattern = file$getName(), tmpdir = tempDir)
    writeLines(text = file$getLines(), con = tempFile)

    data.frame(lintr::lint(
      filename = tempFile,
      linters = lintr::linters_with_defaults(
        lintr::object_name_linter(styles = "camelCase")))) %>%
      dplyr::mutate(filename = file$getName())
  }))
  return(messages)
}


#' lintScore
#'
#' Function that scores the lintr output as a percentage per message type
#' (style, warning, error). Lintr messages / lines assessed * 100
#'
#' @param messages (`data.frame()`)
#'   Data.frame containing lintr messages i.e. from \link[PaRe]{lintRepo}
#'
#' @return (`data.frame()`)
#'     A tibble of percentage scores per type of Lint message.
#' @export
lintScore <- function(repo, messages) {
  files <- repo$getRFiles()

  nLines <- sum(unlist(lapply(files, function(file) {
    file$getNLines()
  })))

  pct <- messages %>%
    dplyr::group_by(.data$type) %>%
    dplyr::tally() %>%
    dplyr::summarise(.data$type, pct = round(n / nLines * 100, 2))

  if (nrow(pct) == 0) {
    message(glue::glue("{nrow(pct)} Lintr messages found"))
    return(NULL)
  }
  return(pct)
}
