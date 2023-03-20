# Copyright 2023 DARWIN EU®
#
# This file is part of IncidencePrevalence
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

#' getDefaultPermittedpackages
#'
#' Gets permitted packages
#'
#' @return tibble of two columns (package, version) with all 'allowed'
#' packages.
#'
#' @import readr
#' @import tidyverse
#'
#' @export
getDefaultPermittedPackages <- function() {
  tmpFile <- list.files(
    path = tempdir(),
    pattern = "tmpPkgs*",
    full.names = TRUE)

  if (length(tmpFile) > 0) {
    message("Get from temp file")
    return(dplyr::tibble(read.csv(tmpFile)))
  } else {
    # Create tmp file
    tmpFile <- tempfile(
      pattern = "tmpPkgs",
      tmpdir = tempdir(),
      fileext = ".csv")

    permittedDependencies <- read.table(
      file = "https://raw.githubusercontent.com/mvankessel-EMC/DependencyReviewerWhitelists/main/dependencies.csv",
      sep = ",",
      header = TRUE) %>%
      tibble()

    # Get base packages
    basePackages <- data.frame(installed.packages(priority = "high")) %>%
      dplyr::select(Package, Built) %>%
      dplyr::rename(package = Package, version = Built) %>%
      dplyr::tibble()

    # Get Tidyverse packages
    tidyversePackages <- sapply(
      X = tidyverse::tidyverse_packages(include_self = TRUE),
      FUN = function(pkg) {
        as.character(packageVersion(pkg))
      }
    )

    tidyversePackages <- tibble(
      package = names(tidyversePackages),
      version = tidyversePackages)

    # Get HADES packages
    hadesPackages <- read.table(
      file = "https://raw.githubusercontent.com/OHDSI/Hades/main/extras/packages.csv",
      sep = ",",
      header = TRUE) %>% select(name) %>%
      mutate(version = rep("*", length(names))) %>%
      rename(package = name) %>%
      tibble()

    hadesPackages$package <- paste0("OHDSI/", hadesPackages$package)

    sourcePackages <- dplyr::bind_rows(
      tidyversePackages,
      hadesPackages,
      permittedDependencies
    )

    depList <- pak::pkg_deps(sourcePackages$package)

    permittedPackages <- dplyr::bind_rows(
      basePackages,
      depList %>%
        select(package, version))

    message("Writing temp file")
    write.csv(
      x = permittedPackages,
      file = tmpFile)
    return(permittedPackages)
  }
}
