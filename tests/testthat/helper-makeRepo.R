makeRepo <- function() {
  repo <- tryCatch({
    tmpDir <- tempdir()
    pathToRepo <- file.path(tmpDir, "glue")

    git2r::clone(
      url = "https://github.com/tidyverse/glue",
      local_path = pathToRepo
    )

    PaRe::Repository$new(path = pathToRepo)
  }, error = function(e) {
    FALSE
  })
  return(repo)
}
