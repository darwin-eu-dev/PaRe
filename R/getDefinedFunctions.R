#' getDefinedFunctions
#'
#' Gets all the defined functions from a \link[PaRe]{Repository} object.
#'
#' @export
#'
#' @param repo (\link[PaRe]{Repository})\cr
#' Repository object.
#'
#' @return (\link[base]{data.frame})\cr
#' \enumerate{
#'   \item (\link[base]{character}) name
#'   \item (\link[base]{integer}) lineStart
#'   \item (\link[base]{numeric}) lineEnd
#'   \item (\link[base]{integer}) nArgs
#'   \item (\link[base]{integer}) cycloComp
#'   \item (\link[base]{character}) fileName
#' }
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
#'   repo <- PaRe::Repository$new(pathToRepo)
#'
#'   getDefinedFunctions(repo)
#' }
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
