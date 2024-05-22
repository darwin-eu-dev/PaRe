library(PaRe)
library(testthat)

# if dlply calls > 0
#   lapply
#     if dlply call %in% defined functions
#
# 1: if1
# 2: !if1
# 2: if1 if2

test_that("minimal", {
  repo <- makeRepo()
  testthat::skip_if(!R6::is.R6(repo))
  file <- repo$getRFiles()[[2]]
  fun <- file$getFunctions()[[2]]
  defFuns <- PaRe::getDefinedFunctions(repo)

  expect_null(PaRe:::getDlplyCall(fun, defFuns))
  unlink(repo$getPath(), recursive = TRUE)
})
