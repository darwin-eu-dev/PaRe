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

#' getDefinedFunctions
#'
#' Gets all the defined functions from a \link[PaRe]{Repository} object.
#'
#' @export
#'
#' @param repo (\link[PaRe]{Repository})\cr
#' Repository object.
#'
#' @return (\link[base]{data.frame})
#' |    column |              data type |
#' | --------- | ---------------------- |
#' |      name | \link[base]{character} |
#' | lineStart |   \link[base]{integer} |
#' |   lineEnd |   \link[base]{numeric} |
#' |     nArgs |   \link[base]{integer} |
#' | cycloComp |   \link[base]{integer} |
#' |  fileName | \link[base]{character} |
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
