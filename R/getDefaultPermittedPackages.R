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
  tryCatch(
    {
      customWhiteList <- lapply(seq_len(nrow(PaRe::whiteList)), function(i) {
        pkgs <- utils::read.table(
          file = unlist(PaRe::whiteList[i, ]["link"]),
          sep = ",",
          header = TRUE
        ) %>%
          dplyr::select(unlist(PaRe::whiteList[i, ]["package"]), unlist(PaRe::whiteList[i, ]["version"]))
      }) %>%
        dplyr::bind_rows()

      depList <- lapply(customWhiteList$package, pak::pkg_deps) %>%
        dplyr::bind_rows() %>%
        dplyr::distinct()

      packages <- c(
        "base", "boot", "class", "cluster", "codetools", "compiler",
        "datasets", "foreign", "graphics", "grDevices", "grid", "KernSmooth",
        "lattice", "MASS", "Matrix", "methods", "mgcv", "nlme", "nnet",
        "parallel", "rpart", "spatial", "splines", "stats", "stats4",
        "survival", "tcltk", "tools", "utils"
      )

      basePackages <- data.frame(
        package = packages,
        version = rep("0.0.0", length(packages))
      )

      permittedPackages <- depList %>%
        dplyr::select("package", version) %>%
        dplyr::bind_rows(basePackages)

      permittedPackages <- permittedPackages %>%
        dplyr::group_by(.data$package) %>%
        dplyr::summarise(version = min(as.numeric_version(version)))

      permittedPackages
    },
    error = function(e) {
      message(e)
      NULL
    },
    warning = function(w) {
      message(w)
      NULL
    }
  )
}
