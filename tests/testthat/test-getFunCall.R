test_that("minimal", {
  repo <- makeRepo()
  testthat::skip_if_not(all(class(repo) == c("Repository", "R6")))
  files <- repo$getRFiles()
  glueIdx <- sapply(files, function(file) {
    file$getName() == "glue.R"
  })

  file <- files[glueIdx][[1]]
  funs <- file$getFunctions()
  defFuns <- PaRe::getDefinedFunctions(repo)

  glueIdx <- sapply(funs, function(fun) {
    fun$getName() == "glue"
  })

  fun <- funs[glueIdx][[1]]

  df <- bind_rows(PaRe:::getFunCall(fun, defFuns))
  expect_true(nrow(df) == 4)
  expect_true(ncol(df) == 2)
  unlink(repo$getPath(), recursive = TRUE)
})
