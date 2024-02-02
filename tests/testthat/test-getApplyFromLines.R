# Pattern:
# [\\w+]?[Aa]pply\\(
#
# lapply {
#   if function[ ]?\\( matches
#     if cluster matches
#     if = matches
# }
#
# 1: if1
# 2: !if1
# 3: if1 if2
# 4: if1 if3

test_that("minimal", {
  lines <- c(
    "apply(x, mean)",
    "sapply(x, mean)",
    "vapply(x, mean)",
    "vapply(X = x, FUN = mean)",
    "clusterApply(cl, x, mean)",
    "clusterApplyLB(cl, x, mean)",
    "parSapply(cl, x, mean)",
    "parLapplyLB(cl = cl, X = x, fun = mean)"
  )

  expect_true(all(PaRe:::getApplyFromLines(lines = lines) == "mean"))
})
