frag <- function() qda_read_fragments(qda_example("zotqda-fragments.csv"))

test_that("code counts are ordered and drop empty codes", {
  d <- qda_code_counts(frag())
  expect_true(all(nzchar(d$code)))
  expect_true(!is.unsorted(rev(d$n)))
})

test_that("the code distance is a proper distance", {
  d <- qda_code_distance(frag(), min_n = 1)
  expect_s3_class(d, "dist")
  expect_true(all(as.matrix(d) >= 0))
  expect_true(all(diag(as.matrix(d)) == 0))
})

# three codes over four segments, with a deliberate overlap
toy <- function() {
  data.frame(
    zotqdaFormat = "fragments/1",
    code = c("A", "A", "B", "B", "C", "A"),
    codeId = c("c1", "c1", "c2", "c2", "c3", "c1"),
    annotationKey = c("s1", "s2", "s1", "s3", "s4", "s3"),
    citekey = c("x", "x", "y", "y", "y", "x"),
    stringsAsFactors = FALSE
  )
}

test_that("clustering reports how well the dendrogram represents the distances", {
  cl <- qda_cluster(toy(), min_n = 1)
  expect_s3_class(cl$hclust, "hclust")
  expect_true(is.finite(cl$cophenetic))
  expect_lte(cl$cophenetic, 1)
  expect_gte(cl$cophenetic, -1)
})

test_that("with two codes the cophenetic correlation is NA, not a fake number", {
  cl <- qda_cluster(frag(), min_n = 1)
  expect_true(is.na(cl$cophenetic))
})

test_that("codes used on the same segments come out closer", {
  d <- as.matrix(qda_code_distance(toy(), min_n = 1))
  # A and B share segments s1 and s3; C shares none
  expect_lt(d["A", "B"], d["A", "C"])
  expect_equal(unname(d["A", "C"]), 1)
})

test_that("MDS returns one point per code", {
  m <- qda_mds(frag(), min_n = 1)
  expect_true(all(c("dim1", "code") %in% names(m$points)))
  expect_equal(nrow(m$points), length(unique(frag()$code)))
})

test_that("too few codes is an error rather than a meaningless result", {
  one <- frag()[1, , drop = FALSE]
  expect_error(qda_code_distance(one, min_n = 1), "fewer than two codes")
})

test_that("chi-squared warns instead of pretending when counts are small", {
  f <- frag()
  expect_warning(res <- qda_chisq(f, group = "citekey"), "expected counts below 5")
  expect_false(res$expected_ok)
  expect_true(is.finite(res$cramers_v))
  expect_gte(res$cramers_v, 0)
  expect_lte(res$cramers_v, 1)
})

test_that("a degenerate table is refused", {
  f <- frag()
  f$code <- "nur einer"
  expect_error(qda_chisq(f, group = "citekey"), "at least two codes")
})

test_that("a table that is too big for the exact test still returns a result", {
  # six codes over five documents with a few hundred codings: the expected
  # counts fall below five, and fisher.test() aborts with a workspace error if
  # asked for the exact p-value.  The table dimensions do not predict that --
  # this fixture is 6x5, well under the old prod(dim) > 100 threshold.
  set.seed(4711)
  codes <- c("A", "A/x", "A/y", "B", "B/x", "C")
  frag <- data.frame(
    annotationKey = sprintf("s%03d", seq_len(260)),
    citekey = sample(paste0("doc", 1:5), 260, TRUE,
                     prob = c(.35, .25, .18, .12, .10)),
    code = sample(codes, 260, TRUE, prob = c(.28, .2, .16, .14, .12, .10)),
    stringsAsFactors = FALSE
  )
  res <- suppressWarnings(qda_chisq(frag))
  expect_false(res$expected_ok)
  expect_true(is.finite(res$test$p.value))
  expect_match(res$test$method, "simulated")
  expect_true(is.finite(res$cramers_v))
})

test_that("the simulated p-value is reproducible and does not disturb the RNG", {
  set.seed(4711)
  codes <- c("A", "B", "C")
  frag <- data.frame(
    annotationKey = sprintf("s%03d", seq_len(40)),
    citekey = sample(paste0("doc", 1:3), 40, TRUE),
    code = sample(codes, 40, TRUE),
    stringsAsFactors = FALSE
  )
  a <- suppressWarnings(qda_chisq(frag, resamples = 200))
  b <- suppressWarnings(qda_chisq(frag, resamples = 200))
  expect_equal(a$test$p.value, b$test$p.value)

  # a function that silently reseeds the global generator breaks every
  # simulation that happens to run after it
  set.seed(99)
  before <- runif(1)
  set.seed(99)
  invisible(suppressWarnings(qda_chisq(frag, resamples = 200)))
  expect_equal(runif(1), before)
})

test_that("a two-by-two table still gets the exact test", {
  frag <- data.frame(
    annotationKey = sprintf("s%02d", seq_len(12)),
    citekey = c(rep("doc1", 6), rep("doc2", 6)),
    code = c("A", "A", "A", "A", "B", "A", "B", "B", "B", "B", "A", "B"),
    stringsAsFactors = FALSE
  )
  res <- suppressWarnings(qda_chisq(frag))
  expect_false(res$expected_ok)
  expect_match(res$test$method, "Fisher")
})

test_that("the chi-squared statistic is reported even when Fisher is used", {
  frag <- data.frame(
    annotationKey = sprintf("s%02d", seq_len(12)),
    citekey = c(rep("doc1", 6), rep("doc2", 6)),
    code = c("A", "A", "A", "A", "B", "A", "B", "B", "B", "B", "A", "B"),
    stringsAsFactors = FALSE
  )
  res <- suppressWarnings(qda_chisq(frag))
  expect_false(res$expected_ok)
  expect_null(res$test$statistic)          # the Fisher object carries none
  expect_true(is.finite(res$statistic))    # but Cramer's V must be traceable
  expect_equal(res$cramers_v,
               sqrt(res$statistic / (res$n * (min(dim(res$table)) - 1))))
  expect_true(all(dim(res$expected) == dim(res$table)))
})

test_that("the inertia share is relative to the whole table, not to what was kept", {
  set.seed(4711)
  frag <- data.frame(
    annotationKey = sprintf("s%03d", seq_len(240)),
    citekey = sample(paste0("doc", 1:5), 240, TRUE),
    code = sample(c("A", "B", "C", "D", "E", "F"), 240, TRUE),
    stringsAsFactors = FALSE
  )
  res <- qda_ca(frag)
  # a 6x5 table has four dimensions; keeping two must not report 100 percent
  expect_lt(sum(res$inertia_share), 1)
  expect_equal(sum(res$inertia_share), sum(res$inertia) / res$total_inertia)
  # total inertia is the chi-squared statistic over n
  chi <- suppressWarnings(qda_chisq(frag))
  expect_equal(res$total_inertia, chi$statistic / chi$n, tolerance = 1e-8)
  # and keeping every dimension does add up to one
  full <- qda_ca(frag, n_dims = 4)
  expect_equal(sum(full$inertia_share), 1, tolerance = 1e-8)
})
