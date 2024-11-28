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

#' getGraphData
#'
#' Get the dependency interactions as a graph representation.
#'
#' @export
#'
#' @param repo (\link[PaRe]{Repository})\cr
#' Repository object.
#' @param packageTypes (\link[base]{c}: `c("Imports")`) of (\link[base]{character})
#' Any of the following options may be included in a vector: \itemize{
#'   \item "imports"
#'   \item "depends"
#'   \item "suggests"
#'   \item "enhances"
#'   \item "linkingto"
#' }
#' @param nThreads (`numeric(1)`: 1)
#' Number of threads to use to fetch permitted packages
#'
#' @return (\link[tidygraph]{as_tbl_graph})
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
#'   # Run getGraphData on the Repository object.
#'   if (interactive()) {
#'     getGraphData(repo = repo, packageTypes = c("Imports"))
#'   }
#' }
getGraphData <- function(repo, packageTypes = c("Imports"), nThreads = 1) {
  deps <- repo$getDescription()$get_deps() %>%
    dplyr::filter(tolower(.data$type) %in% tolower(packageTypes)) %>%
    dplyr::pull(.data$package)

  remoteRef <- repo$getDescription()$get_remotes()
  deps[deps %in% basename(remoteRef)] <- remoteRef[basename(remoteRef) %in% deps]

  # Get all dependencies using pak
  data <- getParDeps(pkgs = deps, nThreads = nThreads)

  # Add current package
  data <- data %>%
    dplyr::add_row(
      ref = repo$getName(),
      package = repo$getName(),
      deps = list(dplyr::tibble(ref = deps, type = "Imports", package = deps)),
      .before = TRUE
    )

  # Reformat dependencies to long format
  pkgDeps <- dplyr::bind_rows(lapply(X = 1:nrow(data), FUN = function(row) {
    deps <- data[["deps"]][[row]][["package"]]
    pkg <- unlist(rep(data[row, ]["package"], length(deps)))
    type <- tolower(data[["deps"]][[row]][["type"]])
    dplyr::tibble(pkg = pkg, deps = deps, type = type, op = "", version = "")
  }))

  pkgDeps <- pkgDeps %>%
    dplyr::filter(tolower(.data$type) %in% tolower(packageTypes)) %>%
    dplyr::select("pkg", "deps")

  # Convert tibble to graph
  netData <- igraph::graph_from_data_frame(
    d = pkgDeps,
    directed = TRUE
  )
  return(netData)
}
