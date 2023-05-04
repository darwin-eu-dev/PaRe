#' countLines
#'
#' Counts the lines of a list of files
#'
#' @param files List of files
#'
#' @return numeric
countLines <- function(files) {
  sum(unlist(lapply(files, function(file) {
    length(readLines(file, warn = FALSE))
  })))
}

#' countPackageLines
#'
#' Counts the lines of files ending with a specific extension.
#'
#' @param path Path to package
#' @param fileEx File extensions to search for, is case sensitive.
#' @param ignoreDirs Directories to ignore when searching for files.
#'
#' @export
#'
#' @return Tibble
#'
#' @examples
#' countPackageLines("./")
R6countPackageLines <- function(
    repo,
    fileEx = c("R", "cpp", "sql", "java"),
    ignoreDirs = c("tests", "extras")) {

  path <- repo$getPath()

  filesList <- lapply(fileEx, function(ex) {
    pths <- list.files(
      path,
      pattern = paste0("\\.", ex, "$"),
      recursive = TRUE)

    dirsToIgnore <- unlist(lapply(ignoreDirs, function(ig) {
      pths[startsWith(pths, ig)]
    }))

    if (length(pths) > 0) {
      file.path(path, pths[!pths %in% dirsToIgnore])
    }
  })

  names(filesList) <- fileEx

  dplyr::bind_rows(lapply(filesList, function(files) {
    countLines(files)
  })) %>% dplyr::mutate(package = basename(path))
}
