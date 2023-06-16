test_that("minimal", {
  suppressWarnings(addPareArticle(repo))

  path <- file.path(repo$getPath(), "vignettes", "articles", "PareReport.Rmd")

  expect_true(file.exists(path))
})
