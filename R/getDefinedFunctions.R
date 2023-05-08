#' getDefinedFunctions
#'
#' @param repo
#' <\link[PaRe]{Repository}> object
#'
#' @return
#' <\link[base]{data.frame}>
#' @export
getDefinedFunctions <- function(repo) {
  files <- repo$getRFiles()

  dplyr::bind_rows(lapply(files, function(file) {
    df <- file$getFunctionTable()

    if (!is.null(df)) {
      df %>%
        dplyr::mutate(fileName = file$getName())
    }
  }))
}
