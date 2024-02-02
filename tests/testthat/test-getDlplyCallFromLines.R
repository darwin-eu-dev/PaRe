library(PaRe)
library(testthat)

# Patterns:
#   [plyr::]?dlply
#   \\s"
#   dlply\\(
#   ,
#   [\\=]?\\w+"
#   \\w+"

test_that("minimal", {
  lines <- list(
    "plyr::dlply(.data = data, .variables = var, .fun = fun)",
    "dlply(.data = data, .variables = var, .fun = fun)",
    "plyr::dlply(.data = data, .variables = var, fun)",
    "dlply(.data = data, .variables = var, fun)",
    "plyr::dlply(data, var, fun)",
    "dlply(data, var, fun)",
    "plyr::dlply(",
    "  .data = data,",
    "  .variables = var,",
    "  .fun = fun)",
    "plyr::dlply(",
    "  .data = data,",
    "  .variables = var,",
    "  fun)",
    "plyr::dlply(",
    "  data,",
    "  var,",
    "  fun)"
  )

  expect_true(all(PaRe:::getDlplyCallFromLines(lines = lines) == "fun"))
})
