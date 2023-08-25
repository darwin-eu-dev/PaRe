#' getGraphData
#'
#' Get the dependency interactions as a graph representation.
#'
#' @export
#'
#' @param repo (\link[PaRe]{Repository})\cr
#' Repository object.
#' @param packageTypes (\link[base]{c}: `c("Imports")`) of (\link[base]{character})
#' Any of the following options may be included in a vector: \itemize{
#'   \item "imports"
#'   \item "depends"
#'   \item "suggests"
#'   \item "enhances"
#'   \item "linkingto"
#' }
#'
#' @return (\link[tidygraph]{as_tbl_graph})
#'
#' @examples
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
#'   # Run getGraphData on the Repository object.
#'   if (interactive()) {
#'     getGraphData(repo = repo, packageTypes = c("Imports"))
#'   }
#' }
getGraphData <- function(repo, packageTypes = c("Imports")) {
  deps <- repo$getDescription()$get_deps() |>
    data.table()

  deps <- deps[i = tolower(type) %in% tolower(packageTypes)]

  remoteRef <- repo$getDescription()$get_remotes()

  deps[
    i = package %in% basename(remoteRef),
    package := remoteRef[basename(remoteRef) %in% package]
  ]

  # Get all dependencies using pak
  data <- pak::pkg_deps(deps$package) |>
    data.table()

  # Add current package
  data <- rbind(
    data.table(
      ref = repo$getName(),
      package = repo$getName(),
      deps = list(data.table(ref = deps$package, type = deps$type, package = deps$package))
    ),
    data,
    fill = TRUE
  )

  data.table::data.table(
    pkg = rep(unname(unlist(data[row, ][, .(package)])), length(deps$package)),
    deps = data[["deps"]][[row]][, .(package)]
  )

  pkgDeps <- lapply(seq_len(nrow(data)), function(row) {
    data[["deps"]][[row]] <- data.table::data.table(data[["deps"]][[row]])
    suppressWarnings(data.table::data.table(
      pkg = rep(unname(unlist(data[row, ][, .(package)])), length(deps$package)),
      deps = unlist(unname(data[["deps"]][[row]][, .(package)]))
    ))
  }) |>
    data.table::rbindlist()

  # Convert tibble to graph
  netData <- igraph::graph_from_data_frame(
    d = pkgDeps,
    directed = TRUE
  )
  return(netData)
}
