# The demo export ships in qdaR and qdaPy and must be the same study.
# It is a genuine zotQDA export: the plugin's own exporters, driven over a
# native model by tool/gen-demo-study.js in the zotQDA repository, write the
# two shipped copies at once. The shape and the headline coefficients are
# frozen here, and the two copies are held to byte identity -- the same
# discipline the frozen qdaZ references follow.
#
# The fragments file is "last state": one row per (annotation, code) with a
# single codedBy, so a segment two coders coded collapses to one and cannot
# carry intercoder agreement. Reliability is therefore computed from the
# HISTORY (one row per coding event, per coder), which is what the plugin
# itself does.

test_that("demo fragments have the documented shape", {
  frag <- qda_read_fragments(qda_example("easyqda-fragments-demo.csv"))
  # last-state export: fewer rows than the per-coder shape it replaced
  expect_equal(nrow(frag), 301L)
  expect_equal(length(unique(frag$code)), 9L)
  expect_equal(length(unique(frag$citekey)), 8L)
  expect_setequal(unique(frag$codedBy), c("Anna", "Robert"))
  expect_true(all(frag$positionKind == "text"))
})

test_that("demo history reads and carries its remove events", {
  hist <- qda_read_history(qda_example("easyqda-history-demo.csv"))
  expect_setequal(unique(hist$action), c("add", "remove"))
  expect_equal(sum(hist$action == "remove"), 6L)
})

test_that("demo agreement from the history matches the frozen values", {
  hist <- qda_read_history(qda_example("easyqda-history-demo.csv"))
  u <- qda_units(qda_codings(hist), coder = "user")
  a <- qda_agreement(u)
  # leaf level: the full code path
  expect_equal(a$alpha, 0.8002, tolerance = 1e-3)
  expect_equal(a$ac1, 0.8035, tolerance = 1e-3)
})

test_that("agreement rises when the code system is read at theme level", {
  hist <- qda_read_history(qda_example("easyqda-history-demo.csv"))
  codings <- qda_codings(hist)
  leaf <- qda_agreement(qda_units(codings, coder = "user"))

  themed <- codings
  # the code paths carry the study root ("<Studie>/<Thema>/<Code>") since the
  # one-study-one-root-code-system change -- the THEME is the second segment
  themed$code <- vapply(strsplit(themed$code, "/"), function(p) p[[2]], "")
  theme <- qda_agreement(qda_units(themed, coder = "user"))

  expect_equal(theme$alpha, 0.8615, tolerance = 1e-3)
  expect_equal(theme$ac1, 0.8640, tolerance = 1e-3)
  # the level effect: coders who split a theme differently still agree on it
  expect_gt(theme$alpha, leaf$alpha)
})

test_that("the shipped demo is byte-identical to the qdaPy copy", {
  for (name in c("easyqda-fragments-demo.csv", "easyqda-history-demo.csv")) {
    ours <- qda_example(name)
    theirs <- file.path("..", "..", "..", "qdaPy", "src", "qdapy", "data", name)
    skip_if_not(file.exists(theirs), "qdaPy sources not present")
    expect_identical(readBin(ours, "raw", file.info(ours)$size),
                     readBin(theirs, "raw", file.info(theirs)$size))
  }
})
