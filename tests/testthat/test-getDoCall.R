library(PaRe)
library(testthat)

test_that("minimal", {
  repo <- makeRepo()
  testthat::skip_if(!R6::is.R6(repo))
  file <- repo$getRFiles()[[2]]
  fun <- file$getFunctions()[[2]]
  defFuns <- PaRe::getDefinedFunctions(repo)

  expect_null(PaRe:::getDoCall(fun, defFuns))
  unlink(repo$getPath())
})
