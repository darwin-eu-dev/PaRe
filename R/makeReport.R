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
#' @param nThreads (`numeric(1)`: 1)
#' Number of threads to use to fetch permitted packages
#'
#' @return (`NULL`)
#'
#' @examples
#' \donttest{
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
makeReport <- function(repo, outputFile, showCode = FALSE, nThreads = 1) {
  if (checkInstalled()) {
    outputFile <- normalizePath(outputFile, mustWork = FALSE)
    writeLines("", con = outputFile)

    desc <- repo$getDescription()
    pkgName <- glue::glue("{desc$get_field('Package')} [{desc$get_version()}]")

    # Render report.Rmd
    rmarkdown::render(
      input = system.file(package = "PaRe", "rmd", "report.Rmd"),
      output_file = outputFile,
      params = list(pkgName = pkgName, repo = repo, showCode = showCode, nThreads = nThreads)
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
