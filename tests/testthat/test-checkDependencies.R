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
  skip_if_offline()
  skip_if_not(repoCloned)

  repo <- Repository$new(path)

  expect_message(
    suppressWarnings(checkDependencies(repo = repo)),
    "All dependencies are approved."
  )
})


test_that("parallel", {
  skip_if_offline()
  skip_if_not(repoCloned)

  repo <- Repository$new(path)

  expect_message(
    suppressWarnings(checkDependencies(repo = repo, nThreads = 2)),
    "All dependencies are approved."
  )
})
