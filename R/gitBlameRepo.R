#' blame
#'
#' Workhorse blame function
#'
#' @param repo <character> Path to repo
#' @param file <character> Relative path to file in repo
#' @param verbose <logical> TRUE: Prints messages
#'
#' @return <data.frame>
blame <- function(repo, file, verbose) {
  dplyr::bind_rows(tryCatch({
    if (verbose) {
      message(glue::glue("working on file: {file}"))
    }
    b <- git2r::blame(repo = normalizePath(repo), path = file)

    lapply(b$hunks, function(hunk) {
      data.frame(
        author = hunk$orig_signature$name,
        file = hunk$orig_path
      )
    })
  }, error = function(e) {
    message(glue::glue("{file} not in git worktree."))
  }))
}

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
gitBlameRepo <- function(repoPath, regex = "\\.[Rr]$", verbose = FALSE, threads = 0) {
  path <- normalizePath(repoPath)
  files <- list.files(path, recursive = TRUE, pattern = regex)

  if (threads > 1) {
    cl <- parallel::makeCluster(spec = threads)
    on.exit(parallel::stopCluster(cl))

    dplyr::bind_rows(parallel::parLapply(
      cl = cl,
      X = files,
      fun = blame,
      repo = path,
      verbose = verbose)) %>%
      dplyr::tibble()
  } else {
    dplyr::bind_rows(lapply(
      X = files,
      FUN = blame,
      repo = path,
      verbose = verbose)) %>%
      dplyr::tibble()
  }
}
