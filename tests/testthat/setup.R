library(PaRe)
library(testthat)
library(git2r)

path <- file.path(tempdir(), "glue")
repoCloned <- NULL

if (!is.null(curl::nslookup("captive.apple.com", error = FALSE))) {
  tryCatch(
    {
      clone(
        url = "https://github.com/tidyverse/glue",
        local_path = path
      )
      repoCloned <- TRUE
    },
    error = function(e) {
      repoCloned <- FALSE
    }
  )
} else {
  repoCloned <- FALSE
}

withr::defer(
  {
    unlink(path, recursive = TRUE, force = TRUE)
  },
  teardown_env()
)
