#' getDefaultPermittedPackages
#'
#' Gets permitted packages
#'
#' @param base Boolean if base package should be included
#'
#' @return tibble of two columns (package, version) with all 'allowed'
#' packages.
#'
#' @export
#' @examples
#' # Run only in interactive session
#' if (interactive()) {
#'   getDefaultPermittedPackages()
#' }
#'
getDefaultPermittedPackages <- function(base = TRUE) {
  # Custom list
  customWhiteList <- dplyr::bind_rows(lapply(seq_len(nrow(whiteList)), function(i) {
    pkgs <- utils::read.table(
      file = unlist(whiteList[i, ]["link"]),
      sep = ",",
      header = TRUE) %>%
      select(unlist(whiteList[i, ]["package"]), unlist(whiteList[i, ]["version"]))
  }))

  basePackages <- NULL
  if (base) {
    # Get base packages
    basePackages <- data.frame(utils::installed.packages(
      lib.loc = .Library,
      priority = "high"
    )) %>%
      dplyr::select("Package", "Built") %>%
      dplyr::rename(package = "Package", version = "Built") %>%
      dplyr::tibble()
  }

  sourcePackages <- dplyr::bind_rows(
    customWhiteList,
    basePackages
  )

  depList <- pak::pkg_deps(sourcePackages$package)

  permittedPackages <- dplyr::bind_rows(
    basePackages,
    depList %>%
      dplyr::select("package", version))

  return(permittedPackages)
}
