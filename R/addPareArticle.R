#' addPareArticle
#'
#' Writes an Rmd-file to `./vignettes/articles/PaReReport.Rmd`. The relative
#' path is dictated by the specified path in the \link[PaRe]{Repository} object.
#'
#' @param repo (\link[PaRe]{Repository})
#' Repository object.
#'
#' @return `NULL`
#' Writes Rmd-file to ./vignettes/articles/PaReReport.Rmd
#'
#' @export
#'
#' @examples
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
#'   addPaReArticle(repo)
#' }
addPareArticle <- function(repo) {
  injected <- readLines(
    con = system.file(package = "PaRe", "rmd", "ReportInjectable.Rmd")
  ) |>
    gsub(
      pattern = "#!P_TITLE",
      replacement = sprintf("%s [%s]", repo$getName(), repo$getDescription()$get_version())
    ) |>
    gsub(
      pattern = "#!P_PATH",
      replacement = gsub(pattern = "\\\\", replacement = "/", repo$getPath())
    )

  articlePath <- file.path(repo$getPath(), "vignettes", "articles")

  if (!dir.exists(articlePath)) {
    dir.create(articlePath)
  }

  writeLines(
    text = injected,
    con = file.path(articlePath, "PareReport.Rmd")
  )
  return(invisible(NULL))
}
