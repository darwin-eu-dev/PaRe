# while (con1 || con2) {
#   if ()
#   if ()
# }
#
#  1: while !con1 !con2
#  2: while con1 if1
#  3: while con1 if2
#  4: while con1 if1 if2
#  5: while con2 if1
#  6: while con2 if2
#  7: while con2 if1 if2
#  8: while con1 con2 if1
#  9: while con1 con2 if2
# 10: while con1 con2 if1 if2

test_that("regular use", {
  repo <- makeRepo()
  testthat::skip_if(!R6::is.R6(repo))
  file <- repo$getRFiles()[[2]]
  fun <- file$getFunctions()[[2]]
  defFuns <- PaRe::getDefinedFunctions(repo)

  expect_true(length(PaRe:::getMultiLineFun(line = 1, lines = fun$getLines())) == 1)
  unlink(repo$getPath())
})

test_that("No closing bracket", {
  lines <- "s3_register <- function(generic, class, method = NULL"
  expect_true(length(PaRe:::getMultiLineFun(line = 1, lines = lines)) == 1)
})

test_that("Empty string", {
  lines <- ""
  expect_true(length(PaRe:::getMultiLineFun(line = 1, lines = lines)) == 1)
})

test_that("NA", {
  lines <- NA
  expect_null(PaRe:::getMultiLineFun(line = 1, lines = lines))
})

test_that("NULL", {
  lines <- NULL
  expect_error(PaRe:::getMultiLineFun(line = 1, lines = lines))
})
