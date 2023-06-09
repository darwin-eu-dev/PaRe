library(PaRe)
library(git2r)
library(testthat)

tmpDir <- tempdir()
pathToRepo <- file.path(tmpDir, "glue")

clone(
  url = "https://github.com/tidyverse/glue",
  local_path = pathToRepo
)

repo <- Repository$new(path = pathToRepo)
