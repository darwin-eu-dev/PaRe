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

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import utils
#' @import pak
#' @import glue
#' @import stringr
#' @import lintr
#' @import rmarkdown
#' @import DiagrammeR
#' @import DiagrammeRsvg
#' @import rsvg
#' @import R6
#' @import checkmate
#' @import parallel
#' @importFrom igraph graph_from_data_frame V write_graph all_simple_paths induced_subgraph graph_from_data_frame
#' @importFrom cli style_bold col_yellow col_red col_magenta col_green col_blue cli_alert_warning cli_alert_info cli_alert_danger cli_alert
#' @importFrom dplyr filter tibble arrange select bind_rows select tally summarise rename relocate mutate group_by
#' @importFrom git2r pull tree hash is_empty blame
#' @importFrom desc description
#' @importFrom magrittr %>%
#' @importFrom cyclocomp cyclocomp
## usethis namespace: end
NULL

#' whiteList
#'
#' data.frame containing links to csv-files which should be used to fetch
#' white-listed dependencies.
#'
#' By default three csv's are listed:
#' 1. darwin
#' 2. hades
#' 3. tidyverse
#'
#' The data.frame is locally fetched under:
#' `system.file(package = "PaRe", "whiteList.csv")`
#'
#' Manual insertions into this data.frame can be made, or the data.frame can
#' be overwritten entirely.
#'
#' The data.frame itself has the following structure:
#'
#' |  column |              data type | description |
#' | ------- | ---------------------- | ----------- |
#' |  source | \link[base]{character} | name of the source |
#' |    link | \link[base]{character} | link or path to the csv-file |
#' | package | \link[base]{character} | columnname of the package name column in the csv-file being linked to |
#' | version | \link[base]{character} | columnname of the version column in the csv-file being linked to |
#'
#' The csv-files that are being pointed to should have the following structure:
#'
#' @export
#' @examples
#' if (interactive()) {
#'   # Dropping tidyverse
#'   whiteList <- whiteList %>%
#'     dplyr::filter(source != "tidyverse")
#'
#'   # getDefaultPermittedPackages will now only use darwin and hades
#'   getDefaultPermittedPackages()
#' }
whiteList <- dplyr::tibble(read.csv(
  system.file(package = "PaRe", "whiteList.csv")
))

utils::globalVariables(".data")
