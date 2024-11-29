# Copyright 2024 DARWIN EU®
#
# This file is part of PaRe
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

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
  tempDir <- tempdir()
  tempFile <- tempfile()

  files <- repo$getRFiles()

  messages <- dplyr::bind_rows(lapply(files, function(file) {
    tempFile <- tempfile(pattern = file$getName(), tmpdir = tempDir)
    writeLines(text = file$getLines(), con = tempFile)

    data.frame(lintr::lint(
      filename = tempFile,
      linters = lintr::linters_with_defaults(
        lintr::object_name_linter(styles = "camelCase")
      )
    )) %>%
      dplyr::mutate(filename = file$getName())
  }))
  return(messages)
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

  pct <- messages %>%
    dplyr::group_by(.data$type) %>%
    dplyr::tally() %>%
    dplyr::summarise(.data$type, pct = round(.data$n / nLines * 100, 2))

  if (nrow(pct) == 0) {
    message(glue::glue("{nrow(pct)} Lintr messages found"))
    return(NULL)
  }
  return(pct)
}
