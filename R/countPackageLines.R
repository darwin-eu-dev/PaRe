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
  })) %>%
    dplyr::tibble()
}
