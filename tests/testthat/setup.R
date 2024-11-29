library(PaRe)
library(testthat)
library(git2r)

path <- file.path(tempdir(), "glue")

if (!is.null(curl::nslookup("captive.apple.com", error = FALSE))) {
  clone(
    url = "https://github.com/tidyverse/glue",
    local_path = path
  )
}

withr::defer({
  unlink(path, recursive = TRUE, force = TRUE)},
  teardown_env()
)
