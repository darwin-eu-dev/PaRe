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
  testthat::skip_on_os(os = "windows")
  expect_null(
    checkDependencies(repo = repo)
  )
})

#
# dependencies <- data.frame(
#   package = c("a", "b", "c"),
#   version = c("1.0.1", "1.9.2", "0.3.0")
# )
#
# permittedPackages <- data.frame(
#   package = c("a", "b"),
#   version = c("1.0.0", "1.9.5")
# )
#
# notPermitted <- data.frame(
#   package = c("c"),
#   version = c("*")
# )

# PaRe:::getVersionDf(dependencies, permittedPackages)

# test_that("", {
#   PaRe:::getVersionDf(dependencies, permittedPackages)
# })
#
# test_that("", {
#   PaRe:::printMessage(notPermitted, )
# })
