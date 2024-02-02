test_that("minimal", {
  repo <- makeRepo()
  testthat::skip_if(!R6::is.R6(repo))
  file <- repo$getRFiles()[[2]]
  fun <- file$getFunctions()[[2]]
  defFuns <- PaRe::getDefinedFunctions(repo)

  df <- bind_rows(PaRe:::getFunCall(fun, defFuns))
  expect_true(nrow(df) == 4)
  expect_true(ncol(df) == 2)
  unlink(repo$getPath(), recursive = TRUE)
})
