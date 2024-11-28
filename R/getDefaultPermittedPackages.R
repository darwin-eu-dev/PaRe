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

getParDeps <- function(pkgs, nThreads) {
  deps <- if (nThreads > 1) {
    cl <- parallel::makeCluster(nThreads)
    on.exit(parallel::stopCluster(cl = cl))
    parallel::clusterEvalQ(cl = cl, expr = {library("pak", character.only = TRUE)})

    parallel::parLapply(cl = cl, X = pkgs, fun = function(pkg) {
      tryCatch({
        res <- pak::pkg_deps(pkg = pkg)
        return(res)
      }, error = function(e) {
        return(NULL)
      }, warning = function(w) {
        return(res)
      })
    })
  } else {
    lapply(X = pkgs, function(pkg) {
      tryCatch({
        res <- pak::pkg_deps(pkg = pkg)
        return(res)
      }, error = function(e) {
        return(NULL)
      }, warning = function(w) {
        return(res)
      })
    })
  }

  if (any(sapply(deps, is.null))) {
    warning(sprintf("Could not fetch dependencies for package: `%s`", pkgs[sapply(deps, is.null)]))
  }
  return(bind_rows(deps))
}

#' getDefaultPermittedPackages
#'
#' Gets permitted packages. An internet connection is required.
#'
#' @param nThreads (`numeric(1)`: 1)
#' Number of threads to use to fetch permitted packages
#'
#' @export
#'
#' @return (\link[dplyr]{tibble})
#' |  column |              data type |
#' | ------- | ---------------------- |
#' | package | \link[base]{character} |
#' | version | \link[base]{character} |
#'
#' @examples
#' \donttest{
#' # Set cache
#' withr::local_envvar(
#'   R_USER_CACHE_DIR = tempfile()
#' )
#'
#' if (interactive()) {
#'   getDefaultPermittedPackages()
#' }
#' }
getDefaultPermittedPackages <- function(nThreads = 1) {
  # Custom list
  customWhiteList <- dplyr::bind_rows(lapply(seq_len(nrow(PaRe::whiteList)), function(i) {
    pkgs <- utils::read.table(
      file = unlist(PaRe::whiteList[i, ]["link"]),
      sep = ",",
      header = TRUE
    ) %>%
      select(unlist(PaRe::whiteList[i, ]["package"]), unlist(PaRe::whiteList[i, ]["version"]))
  }))

  depList <- getParDeps(pkgs = customWhiteList$package, nThreads = nThreads)

  packages <- c(
    "base", "boot", "class", "cluster", "codetools", "compiler",
    "datasets", "foreign", "graphics", "grDevices", "grid", "KernSmooth",
    "lattice", "MASS", "Matrix", "methods", "mgcv", "nlme", "nnet",
    "parallel", "rpart", "spatial", "splines", "stats", "stats4",
    "survival", "tcltk", "tools", "utils"
  )

  basePackages <- data.frame(
    package = packages,
    version = rep("0.0.0", length(packages))
  )

  permittedPackages <- depList %>%
    dplyr::select("package", version) %>%
    dplyr::bind_rows(basePackages)

  permittedPackages <- permittedPackages %>%
    group_by(.data$package) %>%
    summarise(version = min(as.numeric_version(version)))

  return(permittedPackages)
}
