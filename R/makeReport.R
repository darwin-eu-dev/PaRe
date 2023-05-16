#' makeReport
#'
#' Uses rmarkdown's render function to render a html-report of the given
#' package.
#'
#' @param repo
#' <\link[PaRe]{Repository}> object.
#' @param outputFile
#' <\link[base]{character}> Path to html-file.
#' @param showCode
#' <\link[base]{logical}> Default: FALSE; Boolean to show code or not in the report.
#'
#' @export
#'
#' @return
#' `NULL`
makeReport <- function(repo, outputFile, showCode = FALSE) {
  if (checkInstalled()) {
    outputFile <- normalizePath(outputFile, mustWork = FALSE)
    writeLines("", con = outputFile)

    desc <- repo$getDescription()
    pkgName <- glue::glue("{desc$get_field('Package')} [{desc$get_version()}]")

    # Render report.Rmd
    rmarkdown::render(
      input = system.file(package = "PaRe", "rmd", "report.Rmd"),
      output_file = outputFile,
      params = list(pkgName = pkgName, repo = repo, showCode = showCode))
  }
}

#' checkInstalled
#'
#' Checks if suggested packages are installed.
#'
#' @return
#' <\link[base]{logical}> Boolean depending if suggested packages are installed.
checkInstalled <- function() {
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
