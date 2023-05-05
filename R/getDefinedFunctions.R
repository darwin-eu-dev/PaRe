#' getDefinedFunctions
#'
#' @param repo (`PaRe::Repository`)
#' \link[PaRe]{Repository} object
#'
#' @return (`data.frame()`)
#' data.frame
#' @export
getDefinedFunctions <- function(repo) {
  files <- repo$getFiles()

  dplyr::bind_rows(lapply(files, function(file) {
    df <- file$getFunctionTable()

    if (!is.null(df)) {
      df %>%
        dplyr::mutate(fileName = file$getName())
    }
  }))
}
