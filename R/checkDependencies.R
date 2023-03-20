# Copyright 2023 DARWIN EU®
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

#' getDiffVersions
#'
#' Helper function
#'
#' @param dependencies Dependencies
#' @param permittedPackages permittedPackages
#'
#' @return versions of permitted packages
getDiffVersions <- function(dependencies, permittedPackages) {
  permittedPackages %>%
    dplyr::filter(!is.na(version)) %>%
    dplyr::rename("version_rec" = "version") %>%
    dplyr::left_join(
      dependencies,
      by = c("package")
    ) %>%
    dplyr::filter("version_rec" != "version")
}

#' getNotPermitted
#'
#' Helper function
#'
#' @param dependencies Dependencies
#' @param permittedPackages Packages that are permitted as character vector
#'
#' @return Returns vector of not permitted packages
getNotPermitted <- function(dependencies, permittedPackages) {
  # check if dependencies are permitted
  not_permitted <- dependencies %>%
    dplyr::filter(.data$package != "R") %>%
    dplyr::anti_join(
      permittedPackages,
      by = "package"
    ) %>%
    dplyr::select(.data$package) %>%
    dplyr::arrange(.data$package) %>%
    dplyr::pull()
}

#' messagePermission
#'
#' Helper message function
#'
#' @param i iterator
#' @param not_permitted Not permitted
messagePermission <- function(i, not_permitted) {
  cli::cli_alert("  {.pkg {i}) {not_permitted[i]}}")
}

#' messagePackageVersion
#'
#' Helper message function
#'
#' @import cli
#'
#' @param i iterator
#' @param diffVersions different versions
messagePackageVersion <- function(i, diffVersions) {
  cli::cli_alert("  {.pkg {i}) {diffVersions[i]}}")
  cli::cli_alert("    {.pkg currently required: {diffVersions$version[i]}}")
  cli::cli_alert("    {.pkg should be: {diffVersions$version_rec[i]} }")
}

#' checkDependencies
#'
#' Check package dependencies
#'
#' @param packageName Name of package to profile. If NULL current package
#' @param dependencyType Imports, depends, and/ or suggests
#'
#' @return Returns value NULL
#'
#' @export
#' @examples
#' # Run only in interactive session
#' if (interactive()) {
#'   checkDependencies(packageName = "PaRe")
#'
#'   checkDependencies(packageName = "PaRe", c("Suggests"))
#' }
checkDependencies <- function(packageName = NULL,
                              dependencyType = c("Imports", "Depends")) {
  # find dependencies
  if (is.null(packageName)) {
    description <- desc::description$new(file = "./DESCRIPTION")
  } else {
    description <- desc::description$new(package = packageName)
  }

  dependencies <- description$get_deps() %>%
    dplyr::filter(.data$type %in% .env$dependencyType) %>%
    dplyr::select("package", "version")

  # dependencies that are permitted
  permittedPackages <- PaRe::getDefaultPermittedPackages()

  not_permitted <- getNotPermitted(dependencies, permittedPackages)

  n_not_permitted <- length(not_permitted)

  # message
  cli::cli_h2(
    "Checking if package{?s} in {dependencyType} have been approved"
  )

  if (n_not_permitted == 0) {
    cli::cli_alert_success(
      "{.strong All package{?s} in {dependencyType} are  already approved}"
    )
  } else {
    cli::cli_div(theme = list(.alert = list(color = "red")))
    cli::cli_alert_warning(
      "Found {n_not_permitted} package{?s} in {dependencyType} that are not
      approved"
    )
    cli::cli_end()

    sapply(
      X = 1:n_not_permitted,
      FUN = messagePermission,
      not_permitted = not_permitted
    )

    # Example
    example <-
      dplyr::bind_rows(
        lapply(
          X = not_permitted,
          FUN = function(pkg) {
            dplyr::tibble(pak::pkg_search(pkg)) %>%
              dplyr::filter(.data$package == pkg) %>%
              dplyr::select(
                .data$package, version, date, .data$downloads_last_month,
                license, url
              )
          }
        )
      )

    example$url <- stringr::str_replace_all(string = example$url, pattern = ",\\\n", replacement = " ")

    cli::cli_alert_warning(
      "{.emph Please create a new issue at https://github.com/mvankessel-EMC/PaReWhitelists/
    to request approval for packages with the following message:}
    "
    )

    cli::cli_alert(paste(knitr::kable(example), collapse = "\n"))
  }
  return(not_permitted)
}
