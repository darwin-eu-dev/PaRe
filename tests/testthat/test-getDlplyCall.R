# if dlply calls > 0
#   lapply
#     if dlply call %in% defined functions
#
# 1: if1
# 2: !if1
# 2: if1 if2

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

  expect_null(PaRe:::getDlplyCall(fun, defFuns))
})
