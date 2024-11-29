test_that("minimal", {
  lines <- c(
    "do.call(mean, list(c(1, 2, 3)))",
    "do.call('mean', list(c(1, 2, 3)))",
    "do.call(\"mean\", list(c(1, 2, 3)))"
  )

  expect_true(all(PaRe:::getDoCallFromLines(lines) == "mean"))
})
