#' getGraphData
#'
#' @param path
#'     Path to package
#' @param excluded_packages
#'     Packages to exclude
#' @param package_types
#'     Types of packages to be included in the result. Default: c("imports", "depends")
#'     Available types are: "imports", "depends", "suggests", "enhances", "linkingto"
#'
#' @return net_data graph data
#'
#' @export
#' @examples
#' # Only run in interactive session
#' if (interactive()) {
#'   graphData <- getGraphData()
#' }
getGraphData <- function(path = "./", excluded_packages = c(""), package_types = c("imports", "depends")) {
  # Normalize path
  path <- normalizePath(path)

  # Get all dependencies using pak
  data <- pak::local_deps(path)

  # Filter data
  fData <- data %>%
    dplyr::filter(!.data$package %in% excluded_packages)

  sapply(
    X = 1:nrow(fData),
    FUN = function(row) {
      fData[["deps"]][[row]] <- fData[["deps"]][[row]] %>%
        dplyr::filter(!.data$package %in% excluded_packages)
    })

  # Reformat dependencies to long format
  pkg_deps <- dplyr::bind_rows(lapply(X = 1:nrow(fData), FUN = function(row) {
    deps <- fData[["deps"]][[row]][["package"]]
    pkg <- unlist(rep(fData[row, ]["package"], length(deps)))
    type <- tolower(fData[["deps"]][[row]][["type"]])
    dplyr::tibble(pkg = pkg, deps = deps, type = type)
  }))

  pkg_deps <- pkg_deps %>%
    dplyr::filter(.data$type %in% package_types)

  # Convert tibble to graph
  net_data <- tidygraph::as_tbl_graph(
    x = pkg_deps,
    directed = TRUE
  )
}
