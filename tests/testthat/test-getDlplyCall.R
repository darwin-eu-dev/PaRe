# if dlply calls > 0
#   lapply
#     if dlply call %in% defined functions
#
# 1: if1
# 2: !if1
# 2: if1 if2

test_that("minimal", {
  expect_null(PaRe:::getDlplyCall(fun, defFuns))
})
