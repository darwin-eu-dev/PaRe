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
  if (checkInstalled(pkgPath)) {
    # Normalize paths
    pkgPath <- normalizePath(pkgPath)
    outputFile <- normalizePath(outputFile, mustWork = FALSE)
    writeLines("", con = outputFile)

    desc <- desc::description$new(file = pkgPath)
    pkgName <- glue::glue("{desc$get_field('Package')} [{desc$get_version()}]")

    # Render report.Rmd
    rmarkdown::render(
      input = system.file(package = "PaRe", "rmd", "report.Rmd"),
      output_file = outputFile,
      params = list(pkgName = pkgName, pkgPath = pkgPath, showCode = showCode))
  }
}

#' checkInstalled
#'
#' Checks if suggested packages are installed.
#'
#' @param pkgPath Path to package
#'
#' @return Boolean depending if suggested packages are installed.
checkInstalled <- function(pkgPath) {
  desc <- desc::description$new(package = "PaRe")

  reqs <- desc$get_deps() %>%
    dplyr::filter(.data$type == "Suggests") %>%
    dplyr::select("package") %>%
    unlist()

  installed <- unlist(lapply(reqs, FUN = require, character.only = TRUE, quietly = TRUE))

  if (any(!installed)) {
    cli::cli_alert_warning(glue::glue(
      "The following packages are required but not installed: {cli::style_bold(paste0(reqs[!installed], collapse = ', '))}."))
    return(FALSE)
  }
  return(TRUE)
}
