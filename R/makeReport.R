#' makeReport
#'
#' Uses rmarkdown's render function to render a html-report of the given
#' package.
#'
#' @param pkgPath Path to package folder.
#' @param outputFile Path to html-file.
#' @param showCode Default: FALSE; Boolean to show code or not in the report.
#'
#' @import rmarkdown
#'
#' @export
#'
#' @return NULL
#' @examples
#' if (interactive()) {
#'   # Define pkgPath; Glue 1.6.2.9000 comes with the package.
#'   pkgPath <- system.file(package = "PaRe", "glue")
#'
#'   # Define outputFile as a temp file.
#'   outputFile <- tempfile(fileext = ".html")
#'
#'   makeReport(pkgPath, outputFile)
#' }
makeReport <- function(pkgPath, outputFile, showCode = FALSE) {
  # Normalize paths
  pkgPath <- normalizePath(pkgPath)
  outputFile <- normalizePath(outputFile, mustWork = FALSE)
  writeLines("", con = outputFile)

  # Render report.Rmd
  rmarkdown::render(
    input = system.file(package = "PaRe", "rmd", "report.Rmd"),
    output_file = outputFile,
    params = list(pkgPath = pkgPath, showCode = showCode))
}
