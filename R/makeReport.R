#' makeReport
#'
#' Uses rmarkdown's render function to render a html-report of the given package.
#'
#' @export
#'
#' @param repo (\link[PaRe]{Repository})\cr
#' Repository object.
#' @param outputFile (\link[base]{character})\cr
#' Path to html-file.
#' @param showCode (\link[base]{logical}: FALSE)\cr
#' Logical to show code or not in the report.
#'
#' @return (`NULL`)
#'
#' @examples
#' \dontrun{
#' fetchedRepo <- tryCatch(
#'   {
#'     # Set dir to clone repository to.
#'     tempDir <- tempdir()
#'     pathToRepo <- file.path(tempDir, "glue")
#'
#'     # Clone repo
#'     git2r::clone(
#'       url = "https://github.com/darwin-eu/IncidencePrevalence.git",
#'       local_path = pathToRepo
#'     )
#'
#'     # Create instance of Repository object.
#'     repo <- PaRe::Repository$new(path = pathToRepo)
#'
#'     # Set fetchedRepo to TRUE if all goes well.
#'     TRUE
#'   },
#'   error = function(e) {
#'     # Set fetchedRepo to FALSE if an error is encountered.
#'     FALSE
#'   },
#'   warning = function(w) {
#'     # Set fetchedRepo to FALSE if a warning is encountered.
#'     FALSE
#'   }
#' )
#'
#' if (fetchedRepo) {
#'   # Run makeReport on the Repository object.
#'   makeReport(repo = repo, outputFile = tempfile())
#' }
#' }
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
      params = list(pkgName = pkgName, repo = repo, showCode = showCode)
    )
  }
}

#' checkInstalled
#'
#' Checks if suggested packages are installed.
#'
#' @return \link[base]{logical}\cr
#' Logical depending if suggested packages are installed.
checkInstalled <- function() {
  desc <- desc::description$new(package = "PaRe")

  reqs <- desc$get_deps() %>%
    dplyr::filter(.data$type == "Suggests") %>%
    dplyr::select("package") %>%
    unlist()

  installed <- unlist(lapply(reqs, FUN = require, character.only = TRUE, quietly = TRUE))

  if (any(!installed)) {
    cli::cli_alert_warning(glue::glue(
      "The following packages are required but not installed: {cli::style_bold(paste0(reqs[!installed], collapse = ', '))}."
    ))
    return(FALSE)
  }
  return(TRUE)
}
