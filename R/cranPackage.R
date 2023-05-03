#' getCranPackage
#'
#' get CranPackage object from pkgPath. Returns NULL if there is no CRAN package.
#'
#' @param pkgPath package path
#'
#' @export
#'
#' @return CranPackage or NULL if it does not exist
getCranPackage <- function(pkgPath) {
  result <- NULL
  pkgName <- basename(pkgPath)
  tryCatch({
    result <- pkginfo::CranPackage$new(pkgName)
  },
  error = function(msg) {
    return(NULL)
  })
  return(result)
}
