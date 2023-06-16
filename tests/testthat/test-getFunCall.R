test_that("minimal", {
  df <- bind_rows(PaRe:::getFunCall(fun, defFuns))
  expect_true(nrow(df) == 4)
  expect_true(ncol(df) == 2)
})
