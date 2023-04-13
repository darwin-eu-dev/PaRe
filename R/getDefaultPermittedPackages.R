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
  customWhiteList <- tryCatch({
    dplyr::bind_rows(lapply(seq_len(nrow(whiteList)), function(i) {
      pkgs <- utils::read.table(
        file = unlist(whiteList[i, ]["link"]),
        sep = ",",
        header = TRUE) %>%
        select(unlist(whiteList[i, ]["package"]), unlist(whiteList[i, ]["version"]))
    }))
  }, error = function(e) {
    cli::cli_alert_warning(cli::style_bold(cli::col_blue(
      "Could not connect to the internet, online hosted whitelists will be ignored.")))
    return(NULL)
  }, warning = function(w) {
    cli::cli_alert_warning(cli::style_bold(cli::col_blue(
      "Could not connect to the internet, online hosted whitelists will be ignored.")))
    return(NULL)
  })

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
    depList) %>%
      dplyr::select("package", version)

  permittedPackages <- permittedPackages %>%
    group_by(.data$package) %>%
    summarise(version = min(as.numeric_version(version)))

  return(permittedPackages)
}
