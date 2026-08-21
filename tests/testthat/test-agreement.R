test_that("units are built per segment and coder", {
  frag <- data.frame(
    annotationKey = c("s1", "s1", "s2", "s2", "s3"),
    codedBy = c("ann", "bob", "ann", "bob", "ann"),
    code = c("A", "A", "B", "A", "A"),
    stringsAsFactors = FALSE
  )
  u <- qda_units(frag)
  expect_equal(dim(u), c(3L, 2L))
  expect_equal(colnames(u), c("ann", "bob"))
  expect_equal(unname(u["s1", ]), c("A", "A"))
  expect_true(is.na(u["s3", "bob"]))  # bob never rated s3
  expect_equal(attr(u, "multi"), 0L)
})

test_that("a segment one coder coded twice is set aside and counted", {
  frag <- data.frame(
    annotationKey = c("s1", "s1", "s1", "s2", "s2"),
    codedBy = c("ann", "ann", "bob", "ann", "bob"),
    code = c("A", "B", "A", "B", "B"),
    stringsAsFactors = FALSE
  )
  u <- qda_units(frag)
  expect_true(is.na(u["s1", "ann"]))
  expect_equal(u[["s1", "bob"]], "A")
  expect_equal(attr(u, "multi"), 1L)
  # and it is reported, not swallowed
  expect_equal(qda_agreement(u)$multi_set_aside, 1L)
})

test_that("the binary view keeps multiply coded segments", {
  frag <- data.frame(
    annotationKey = c("s1", "s1", "s1", "s2", "s2"),
    codedBy = c("ann", "ann", "bob", "ann", "bob"),
    code = c("A", "B", "A", "B", "B"),
    stringsAsFactors = FALSE
  )
  b <- qda_units_binary(frag, "A")
  expect_equal(unname(b["s1", ]), c("yes", "yes"))
  expect_equal(unname(b["s2", ]), c("no", "no"))
  expect_equal(qda_percent_agreement(b), 1)
})

test_that("uncoded segments become their own category", {
  frag <- data.frame(
    annotationKey = c("s1", "s1"),
    codedBy = c("ann", "bob"),
    code = c("A", "A"),
    stringsAsFactors = FALSE
  )
  unc <- data.frame(annotationKey = "s2", stringsAsFactors = FALSE)
  u <- qda_units(frag, uncoded = unc)
  expect_equal(nrow(u), 2L)
  expect_equal(unname(u["s2", ]), c("(no code)", "(no code)"))
  # agreement about irrelevance is agreement
  expect_equal(qda_percent_agreement(u), 1)
  # without the uncoded export that segment is simply absent
  expect_equal(nrow(qda_units(frag)), 1L)
})

test_that("Cohen's kappa matches the hand computation", {
  u <- cbind(ann = c("A", "B", "A", "B"), bob = c("A", "B", "B", "B"))
  # po = 3/4, pe = .5*.25 + .5*.75 = .5
  expect_equal(qda_percent_agreement(u), 0.75)
  expect_equal(qda_kappa(u), 0.5)
  expect_error(qda_kappa(cbind(u, cat = c("A", "A", "A", "A"))), "two coders")
})

test_that("Fleiss' kappa matches the hand computation", {
  u <- cbind(ann = c("A", "B", "A"), bob = c("A", "B", "B"),
             cat = c("A", "B", "A"))
  # pa = 7/9, pe = (5/9)^2 + (4/9)^2 = 41/81  ->  22/40
  expect_equal(qda_fleiss(u), 0.55)
})

test_that("Krippendorff's alpha matches the hand computation", {
  u <- cbind(ann = c("A", "B", "A", "B"), bob = c("A", "B", "B", "B"))
  # d_obs = 2, d_exp = 30/7  ->  1 - 14/30
  expect_equal(qda_alpha(u), 8 / 15)
})

test_that("alpha tolerates a coder who skipped a unit", {
  u <- cbind(ann = c("A", "B", "A", NA), bob = c("A", "B", "B", "A"))
  expect_false(is.nan(qda_alpha(u)))
  # the incomplete unit contributes nothing
  expect_equal(qda_alpha(u), qda_alpha(u[1:3, ]))
})

test_that("AC1 holds up where kappa collapses on skewed marginals", {
  u <- cbind(ann = c("A", "A", "A", "B"), bob = c("A", "A", "A", "A"))
  expect_equal(qda_percent_agreement(u), 0.75)
  expect_equal(qda_kappa(u), 0)      # marginal-based chance eats it all
  expect_equal(qda_ac1(u), 0.68)     # (0.75 - 0.21875) / 0.78125
})

test_that("Brennan and Prediger uses uniform chance", {
  u <- cbind(ann = c("A", "A", "A", "B"), bob = c("A", "A", "A", "A"))
  expect_equal(qda_brennan(u), 0.5)          # (0.75 - 0.5) / 0.5
  expect_equal(qda_brennan(u, q = 4), 2 / 3) # (0.75 - 0.25) / 0.75
})

test_that("undefined measures say NaN instead of inventing a number", {
  same <- cbind(ann = c("A", "A"), bob = c("A", "A"))
  expect_true(is.nan(qda_kappa(same)))   # pe = 1
  expect_true(is.nan(qda_fleiss(same)))  # a single category
  expect_true(is.nan(qda_ac1(same)))
  empty <- matrix(NA_character_, 0, 2, dimnames = list(NULL, c("a", "b")))
  expect_true(is.nan(qda_percent_agreement(empty)))
})

test_that("agreement reports every measure side by side", {
  u <- cbind(ann = c("A", "B", "A", "B"), bob = c("A", "B", "B", "B"))
  a <- qda_agreement(u)
  expect_equal(nrow(a), 1L)
  expect_equal(a$units, 4L)
  expect_equal(a$coders, 2L)
  expect_equal(a$categories, 2L)
  expect_equal(a$cohen, 0.5)
  # Cohen is not defined for three coders and must not be faked
  u3 <- cbind(u, cat = c("A", "B", "A", "B"))
  expect_true(is.na(qda_agreement(u3)$cohen))
  expect_false(is.na(qda_agreement(u3)$fleiss))
})

test_that("paths flatten to a level", {
  expect_equal(qda_flatten_path("A/b/c", 2), "A/b")
  expect_equal(qda_flatten_path("A/b/c", 9), "A/b/c")
  expect_equal(qda_flatten_path("A/b/c", NULL), "A/b/c")
  expect_equal(qda_flatten_path(c("A/b", "C"), 1), c("A", "C"))
})

test_that("agreement improves towards the top of the code system", {
  u <- cbind(ann = c("A/x", "A/y", "B/x"), bob = c("A/y", "A/y", "B/x"))
  lv <- qda_level_agreement(u)
  expect_equal(nrow(lv), 2L)
  expect_equal(lv$level, 1:2)
  expect_equal(lv$percent[1], 1)        # both call it A, A, B
  expect_equal(lv$percent[2], 2 / 3)    # they split on the subcode
  expect_equal(lv$categories[1], 2L)
  expect_equal(lv$categories[2], 3L)
})

test_that("the level curve honours max_level", {
  u <- cbind(ann = c("A/x/1", "A/y/1"), bob = c("A/x/2", "A/y/1"))
  expect_equal(nrow(qda_level_agreement(u)), 3L)
  expect_equal(nrow(qda_level_agreement(u, max_level = 2)), 2L)
})

test_that("the confusion table names the pairs that cost the agreement", {
  u <- cbind(ann = c("A", "B", "A", "B"), bob = c("A", "B", "B", "B"))
  cf <- qda_confusion(u)
  expect_equal(names(cf)[1:2], c("ann", "bob"))
  expect_equal(sum(cf$n), 4L)
  dis <- qda_confusion(u, only_disagreements = TRUE)
  expect_equal(nrow(dis), 1L)
  expect_equal(dis$ann, "A")
  expect_equal(dis$bob, "B")
  expect_equal(dis$n, 1L)
})

test_that("plots are ggplot objects", {
  u <- cbind(ann = c("A/x", "A/y", "B/x"), bob = c("A/y", "A/y", "B/x"))
  expect_s3_class(qda_plot_level_agreement(u), "ggplot")
})

test_that("units can be keyed on the code identity instead of the path", {
  frag <- data.frame(
    annotationKey = c("s1", "s1"),
    codedBy = c("ann", "bob"),
    code = c("Belastung", "Stress"),   # same code, renamed between exports
    codeId = c("cX1", "cX1"),
    stringsAsFactors = FALSE
  )
  expect_equal(qda_percent_agreement(qda_units(frag)), 0)
  expect_equal(qda_percent_agreement(qda_units(frag, value = "codeId")), 1)
})
