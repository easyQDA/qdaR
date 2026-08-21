# The demo export ships in qdaR and qdaPy and must be the same study.
# Written by qdaPy's scripts/make_demo_export.py into both packages at
# once; the shape and the headline coefficients are frozen here exactly
# as they are on the Python side (tests/test_demo_export.py).

test_that("demo fragments have the documented shape", {
  frag <- qda_read_fragments(qda_example("zotqda-fragments-demo.csv"))
  expect_equal(nrow(frag), 502L)
  expect_equal(length(unique(frag$code)), 9L)
  expect_equal(length(unique(frag$citekey)), 8L)
  expect_setequal(unique(frag$codedBy), c("ann", "bob"))
})

test_that("demo agreement matches the frozen values", {
  frag <- qda_read_fragments(qda_example("zotqda-fragments-demo.csv"))
  a <- qda_agreement(qda_units(frag))
  expect_equal(a$alpha, 0.7266, tolerance = 1e-3)
  expect_equal(a$ac1, 0.7299, tolerance = 1e-3)
})

test_that("demo history reads and carries its remove events", {
  hist <- qda_read_history(qda_example("zotqda-history-demo.csv"))
  expect_setequal(unique(hist$action), c("add", "remove"))
  expect_equal(sum(hist$action == "remove"), 6L)
})
