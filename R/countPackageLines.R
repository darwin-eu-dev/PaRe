#' countPackageLines
#'
#' Counts the package lines of a \link[PaRe]{Repository} object.
#'
#' @export
#'
#' @param repo (\link[PaRe]{Repository})\cr
#' Repository object.
#'
#' @return (\link[dplyr]{tibble}\cr)
#' Tibble containing the amount of lines per file in the Repository object.
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
#'       url = "https://github.com/tidyverse/glue.git",
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
#'   # Run countPackageLines on the Repository object.
#'   countPackageLines(repo = repo)
#' }
countPackageLines <- function(repo) {
  files <- repo$getFiles()
  files <- Filter(Negate(is.null), files)

  data.frame(lapply(files, function(fileType) {
    sum(unlist(lapply(fileType, function(file) {
      file$getNLines()
    })))
  })) |>
    data.table::data.table()
}
