#' gitBlameRepo
#'
#' Retrieves a data.frame of authors per file.
#'
#' @param repoPath <character> Path to repository.
#' @param regex <character> Reg-ex pattern to use to detect files.
#' @param verbose <logical> TRUE: Prints status messages.
#' @param threads <numeric> Number of threads to use. Useful for a large quantity of files.
#'
#' @return <data.frame>
#' @export
gitBlameRepo <- function(repo, verbose = FALSE) {
  files <- repo$getFiles()

  dplyr::bind_rows(lapply(
    X = files,
    FUN = blame,
    repo = path,
    verbose = verbose)) %>%
    dplyr::tibble()
}
