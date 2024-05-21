library(PaRe)
library(git2r)
library(testthat)
library(withr)

withr::local_envvar(
  R_USER_CACHE_DIR = tempfile("cache-")
)

test_that("void", {
  expect_error(
    checkDependencies(),
    "argument \"repo\" is missing, with no default"
  )
})

test_that("minimal", {
  repo <- makeRepo()
  testthat::skip_if(!R6::is.R6(repo))

  expect_message(
    checkDependencies(repo),
    "All dependencies are approved."
  )
  #unlink(repo$getPath(), recursive = TRUE)
})
