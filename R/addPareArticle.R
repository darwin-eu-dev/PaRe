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
  ) %>%
    gsub(
      pattern = "#!P_TITLE",
      replacement = glue::glue("{repo$getName()} [{repo$getDescription()$get_version()}]")
    ) %>%
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
