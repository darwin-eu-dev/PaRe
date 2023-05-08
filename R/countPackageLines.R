#' countPackageLines
#'
#' @param repo
#' <\link[PaRe]{Repository}> object.
#'
#' @return
#' <\link[dplyr]{tibble}> containing the amount of lines per file in the
#' Repository object.
#' @export
countPackageLines <- function(repo) {
  files <- repo$getFiles()
  files <- Filter(Negate(is.null), files)

  data.frame(lapply(files, function(fileType) {
    sum(unlist(lapply(fileType, function(file) {
      file$getNLines()
    })))
  })) %>%
    dplyr::tibble()
}
