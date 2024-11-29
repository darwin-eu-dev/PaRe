test_that("minimal", {
  skip_if_offline()
  skip_if_not(repoCloned)

  repo <- Repository$new(path)

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
  expect_true(nrow(df) == 1)
  expect_true(ncol(df) == 2)
})
