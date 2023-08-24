#' getDefaultPermittedPackages
#'
#' Gets permitted packages. An internet connection is required.
#'
#' @export
#'
#' @return (\link[dplyr]{tibble})
#' |  column |              data type |
#' | ------- | ---------------------- |
#' | package | \link[base]{character} |
#' | version | \link[base]{character} |
#'
#' @examples
#' \donttest{
#' # Set cache
#' withr::local_envvar(
#'   R_USER_CACHE_DIR = tempfile()
#' )
#'
#' if (interactive()) {
#'   getDefaultPermittedPackages()
#' }
#' }
getDefaultPermittedPackages <- function() {
  # Custom list
  tryCatch({
    customWhiteList <- lapply(seq_len(nrow(PaRe::whiteList)), function(i) {
      pkgs <- data.table::fread(unlist(PaRe::whiteList[i, "link"]))

      cols <- PaRe::whiteList[i, c("package", "version")] |>
        unlist() |>
        unname()
      return(pkgs[j = ..cols])
    }) |>
      data.table::rbindlist()

    depList <- pak::pkg_deps(customWhiteList$package) |>
      data.table::data.table()

    packages <- c(
      "base", "boot", "class", "cluster", "codetools", "compiler",
      "datasets", "foreign", "graphics", "grDevices", "grid", "KernSmooth",
      "lattice", "MASS", "Matrix", "methods", "mgcv", "nlme", "nnet",
      "parallel", "rpart", "spatial", "splines", "stats", "stats4",
      "survival", "tcltk", "tools", "utils"
    )

    basePackages <- data.table::data.table(
      package = packages,
      version = rep("0.0.0", length(packages))
    )

    permittedPackages <- rbind(
      depList[j = .(package, version)],
      basePackages
    )

    permittedPackages[
      , version := .SD[as.numeric_version(version) == min(as.numeric_version(version))],
      by = package
    ]
  },
  error = function(e) {
    message(e)
    NULL
  },
  warning = function(w) {
    message(w)
    NULL
  })
}
