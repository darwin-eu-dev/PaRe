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

#' printMessage
#'
#' Prints messages dependening of the nrow of the number of rows of the
#' notPermitted and versionCheck data.frames
#'
#' @param notPermitted (`data.frame()`)
#' @param versionCheck (`data.frame()`)
#'
#' @return (`data.frame()`)
#' |  column |              data type |
#' | ------- | ---------------------- |
#' | package | `character()` |
#' | version | `character()` |
#' @noRd
printMessage <- function(notPermitted, versionCheck) {
  if (nrow(notPermitted) > 0) {
    message(
      glue::glue(
        "The following are not permitted: {cli::style_bold(paste0(notPermitted$package, collapse = ', '))}\n",
        "Please open an issue here: {cli::style_bold('https://github.com/mvankessel-EMC/DependencyReviewerWhitelists/issues')}"
      )
    )
    return(notPermitted)
  } else if (nrow(versionCheck) > 0) {
    message(glue::glue(
      "The following versions are not of the right version: {cli::col_yellow(paste0(versionCheck$package, collapse = ', '))}\n",
      "Please open an issue here: {cli::style_bold('https://github.com/mvankessel-EMC/DependencyReviewerWhitelists/issues')}"
    ))
    return(versionCheck)
  } else {
    message("All dependencies are approved.")
    return(NULL)
  }
}

#' getVersionDf
#'
#' Function to compare different versions.
#'
#' @noRd
#'
#' @param dependencies (`data.frame()`)
#' |  column |              data type |
#' | ------- | ---------------------- |
#' | package | `character()` |
#' | version | `character()` |
#' @param permittedPackages `data.frame()`
#' |  column |              data type |
#' | ------- | ---------------------- |
#' | package | `character()` |
#' | version | `character()` |
#'
#' @return (`data.frame()`)
#' |  column |              data type |
#' | ------- | ---------------------- |
#' | package | `character()` |
#' | version | `character()` |
getVersionDf <- function(dependencies, permittedPackages) {
  permitted <- dependencies %>%
    dplyr::filter(.data$package %in% permittedPackages$package)

  permitted$version[permitted$version == "*"] <- "0.0.0"

  permitted <- permitted %>%
    dplyr::arrange(.data$package)

  permittedPackages <- permittedPackages[
    permittedPackages$package %in% permitted$package,
  ] %>%
    dplyr::arrange(.data$package)

  df <- cbind(
    permittedPackages,
    allowed = permitted$version
  )

  return(df[
    !as.numeric_version(df$version) >= as.numeric_version(df$allowed),
  ])
}

#' checkDependencies
#'
#' Check package dependencies
#'
#' @export
#'
#' @param repo (`Repository`)\cr
#' Repository object.
#' @param dependencyType (`character()`)\cr
#' Types of dependencies to be included
#' @param nThreads (`numeric(1)`: 1)
#' Number of threads to use to fetch permitted packages
#'
#' @return (`data.frame()`)\cr
#' Data frame with all the packages that are now permitted.

#' |  column |              data type |
#' | ------- | ---------------------- |
#' | package | `character()` |
#' | version | `character()` |
#'
#' @examples
#' \donttest{
#' # Set cahce, usually not required.
#' withr::local_envvar(
#'   R_USER_CACHE_DIR = tempfile()
#' )
#'
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
#'   # Use checkDependencies on the Repository object.
#'   checkDependencies(repo)
#'   checkDependencies(repo, dependencyType = c("Imports", "Suggests"))
#' }
#' }
checkDependencies <- function(
    repo,
    dependencyType = c("Imports", "Depends"),
    nThreads = 1) {
  description <- repo$getDescription()

  dependencies <- description$get_deps() %>%
    dplyr::filter(.data$type %in% dependencyType) %>%
    dplyr::select("package", "version") %>%
    dplyr::filter(.data$package != "R")

  dependencies$version <- stringr::str_remove(
    string = dependencies$version,
    pattern = "[\\s>=<]+"
  )

  permittedPackages <- getDefaultPermittedPackages(nThreads)

  notPermitted <- dependencies %>%
    dplyr::filter(!.data$package %in% permittedPackages$package)

  permitted <- dependencies %>%
    dplyr::filter(.data$package %in% permittedPackages$package)

  permitted$version[permitted$version == "*"] <- "0.0.0"

  return(printMessage(
    notPermitted,
    getVersionDf(dependencies, permittedPackages)
  ))
}
