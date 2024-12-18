test_that("minimal", {
  skip_if_not(checkSuggests())
  skip_if_offline()
  skip_if_not(repoCloned)

  repo <- Repository$new(path)

  suppressWarnings(addPareArticle(repo))

  path <- file.path(repo$getPath(), "vignettes", "articles", "PareReport.Rmd")

  expect_true(file.exists(path))
})
