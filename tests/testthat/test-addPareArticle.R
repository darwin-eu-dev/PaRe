test_that("minimal", {
  if (checkSuggests()) {
    repo <- makeRepo()
    testthat::skip_if(!R6::is.R6(repo))

    suppressWarnings(addPareArticle(repo))

    path <- file.path(repo$getPath(), "vignettes", "articles", "PareReport.Rmd")

    expect_true(file.exists(path))
    unlink(repo$getPath(), recursive = TRUE)
  } else {
    skip()
  }
})
