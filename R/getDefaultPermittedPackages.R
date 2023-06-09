#' getDefaultPermittedPackages
#'
#' Gets permitted packages. An internet connection is required.
#'
#' @export
#'
#' @param base (\link[base]{logical}: TRUE)
#' \describe{
#'   \item{TRUE}{Base packages will be included.}
#'   \item{FALSE}{Base packages will be ignored.}
#' }
#'
#' @return (\link[dplyr]{tibble})
#' |  column |              data type |
#' | ------- | ---------------------- |
#' | package | \link[base]{character} |
#' | version | \link[base]{character} |
#'
#' @examples
#' \donttest{
#'   # Set cache
#'   withr::local_envvar(
#'     R_USER_CACHE_DIR = tempfile()
#'   )
#'
#'   if (interactive()) {
#'     getDefaultPermittedPackages()
#'   }
#' }
getDefaultPermittedPackages <- function(base = TRUE) {
  # Custom list
  tryCatch(
    {
      customWhiteList <- dplyr::bind_rows(lapply(seq_len(nrow(PaRe::whiteList)), function(i) {
        pkgs <- utils::read.table(
          file = unlist(PaRe::whiteList[i, ]["link"]),
          sep = ",",
          header = TRUE
        ) %>%
          select(unlist(PaRe::whiteList[i, ]["package"]), unlist(PaRe::whiteList[i, ]["version"]))
      }))

      basePackages <- NULL
      if (base) {
        # Get base packages
        basePackages <- dplyr::bind_rows(lapply(list.files(.Library), function(pkg) {
          df <- packageDescription(pkg = pkg, fields = c("Package", "Version")) %>%
            unlist()

          dplyr::tibble(
            package = df[1],
            version = df[2],
            row.names = NULL
          )
        })) %>%
          dplyr::filter(.data$package != "translations")
      }

      sourcePackages <- dplyr::bind_rows(
        customWhiteList,
        basePackages
      )

      depList <- pak::pkg_deps(sourcePackages$package)

      permittedPackages <- dplyr::bind_rows(
        basePackages,
        depList
      ) %>%
        dplyr::select("package", version)

      permittedPackages <- permittedPackages %>%
        group_by(.data$package) %>%
        summarise(version = min(as.numeric_version(version)))

      permittedPackages
    },
    error = function(e) {
      print(e)
      message(
        "Could not connect to the internet, online hosted whitelists will be ignored."
      )
      NULL
    },
    warning = function(w) {
      message(
        "Could not connect to the internet, online hosted whitelists will be ignored."
      )
      NULL
    }
  )
}
