#' getGraphData
#'
#' @param path
#'     Path to package
#' @param excludedPackages
#'     Packages to exclude
#' @param packageTypes
#'     Types of packages to be included in the result. Default: c("imports", "depends")
#'     Available types are: "imports", "depends", "suggests", "enhances", "linkingto"
#'
#' @return net_data graph data
#'
#' @export
getGraphData <- function(repo, packageTypes = c("Imports", "Suggests")) {
  deps <- repo$getDescription()$get_deps() %>%
    dplyr::filter(tolower(.data$type) %in% tolower(packageTypes)) %>%
    dplyr::pull(package)

  remoteRef <- repo$getDescription()$get_remotes()
  deps[deps %in% basename(remoteRef)] <- remoteRef[basename(remoteRef) %in% deps]

  # Get all dependencies using pak
  data <- pak::pkg_deps(deps)

  # Add current package
  data <- data %>%
    dplyr::add_row(
      ref = repo$getName(),
      package = repo$getName(),
      deps = list(dplyr::tibble(ref = deps, type = "Imports", package = deps)),
      .before = TRUE)

  # Reformat dependencies to long format
  pkgDeps <- dplyr::bind_rows(lapply(X = 1:nrow(data), FUN = function(row) {
    deps <- data[["deps"]][[row]][["package"]]
    pkg <- unlist(rep(data[row, ]["package"], length(deps)))
    type <- tolower(data[["deps"]][[row]][["type"]])
    dplyr::tibble(pkg = pkg, deps = deps, type = type, op = "", version = "")
  }))

  pkgDeps <- pkgDeps %>%
    dplyr::filter(tolower(.data$type) %in% tolower(packageTypes)) %>%
    dplyr::select(pkg, deps)

  # Convert tibble to graph
  netData <- tidygraph::as_tbl_graph(
    x = pkgDeps,
    directed = TRUE
  )
  return(netData)
}
