#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import readr
#' @import utils
#' @import pak
#' @import glue
#' @import stringr
#' @import dplyr
#' @import cli
#' @import knitr
#' @import rlang
#' @import tidygraph
#' @import lintr
#' @import rmarkdown
#' @import DiagrammeR
#' @import DiagrammeRsvg
#' @import rsvg
#' @import here
#' @import R6
#' @import git2r
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
#' `data.frame(source = c(), link = c(), package = c(), version = c())`
#' `source` is the name of the source
#' `link` is the link or path to the csv-file
#' `package` is the columnname of the package name column in the csv-file being
#' linked to.
#' `version` is the columnname of the version column in the csv-file being
#' linked to.
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
  system.file(package = "PaRe", "whiteList.csv")))
