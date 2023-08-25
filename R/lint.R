#' lintRepo
#'
#' Get all the lintr messages of the \link[PaRe]{Repository} object.
#'
#' @export
#'
#' @param repo (\link[PaRe]{Repository})
#'
#' @return (\link[base]{data.frame})
#' |        column |              data type | description                                 |
#' | ------------- | ---------------------- | ------------------------------------------- |
#' |      filename | \link[base]{character} | Name of the file                            |
#' |   line_number |    \link[base]{double} | Line in which the message was found         |
#' | column_number |    \link[base]{double} | Column in which the message was found       |
#' |          type | \link[base]{character} | Type of message                             |
#' |       message | \link[base]{character} | Style, warning, or error message            |
#' |          line | \link[base]{character} | Line of code in which the message was found |
#' |        linter | \link[base]{character} | Linter used                                 |
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
#'   # Run lintRepo on the Repository object.
#'   messages <- lintRepo(repo = repo)
#' }
lintRepo <- function(repo) {
  if (is.null(repo$getCluster())) {
    return(data.table::rbindlist(lapply(
      X = repo$getRFiles(),
      FUN = lintFile,
      repo = repo
    )))
  } else {
    return(data.table::rbindlist(parallel::parLapply(
      cl = repo$getCluster(),
      X = repo$getRFiles(),
      fun = lintFile,
      repo = repo
    )))
  }
}

lintFile <- function(file, repo) {
  lintr::lint(
    text = file$getLines(),
    filename = file.path(repo$getPath(), file$getFilePath()),
    linters = lintr::linters_with_defaults(
      lintr::object_name_linter(styles = "camelCase")
    )
  ) |> data.frame()
}

#' lintScore
#'
#' Function that scores the lintr output as a percentage per message type
#' (style, warning, error). Lintr messages / lines assessed * 100
#'
#' @export
#'
#' @param repo (\link[PaRe]{Repository})\cr
#' Repository object.
#' @param messages (\link[base]{data.frame})\cr
#' Data frame containing lintr messages. See \link[PaRe]{lintRepo}.
#'
#' @return (\link[dplyr]{tibble})
#' \describe{
#'   \item{type}{(\link[base]{character}) Type of message.}
#'   \item{pct}{(\link[base]{double}) Score.}
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
#'   messages <- lintRepo(repo = repo)
#'
#'   # Run lintScore on the Repository object.
#'   lintScore(repo = repo, messages = messages)
#' }
lintScore <- function(repo, messages) {
  files <- repo$getRFiles()

  nLines <- sum(unlist(lapply(files, function(file) {
    file$getNLines()
  })))

  # pct <- messages %>%
  #   dplyr::group_by(.data$type) %>%
  #   dplyr::tally() %>%
  #   dplyr::summarise(.data$type, pct = round(.data$n / nLines * 100, 2))
  #
  # d <- data.table::data.table(messages)

  x <- d[
    j = data.table::data.table(table(type))][
      , pct := round(N / nLines * 100, 2), by = type]

  if (nrow(pct) == 0) {
    message(sprintf("%s Lintr messages found", nrow(pct)))
    return(NULL)
  }
  return(pct)
}
