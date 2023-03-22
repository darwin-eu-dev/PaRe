#' pareWhiteSpace
#'
#' @param filePath Path to file.
#' @param outPath Default NULL: Path to output file. If NULL file will be
#' overwritten.
#'
#' @return NULL
#' @export
pareWhiteSpace <- function(filePath, outPath = NULL) {
  lines <- readLines(filePath)

  if (!is.null(outPath)) {
    writeLines(trimws(lines, which = "right"), con = outPath)
  } else {
    writeLines(trimws(lines, which = "right"), con = filePath)
  }
}

#' pareWhiteSpacePkg
#'
#' @param pkgPath Path to package
#' @param overwrite Default: TRUE boolean to overwrite original files or not.
#'
#' @return NULL
#' @export
pareWhiteSpacePkg <- function(pkgPath = "./", overwrite = TRUE) {
  files <- list.files(file.path(pkgPath, "R"), full.names = TRUE)
  if (overwrite) {
    lapply(files, pareWhiteSpace)
  } else {
    lapply(files, function(file) {
      tmpFile <- tempfile(pattern = basename(file))
      message(glue::glue("New file is written to: {tmpFile}"))
      pareWhiteSpace(file, tmpFile)
    })
  }
  return(invisible(NULL))
}
