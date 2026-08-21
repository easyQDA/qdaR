# The same fixture and the same expected values ship with the Python twin
# (qdaPy), where they are checked against its own implementation. Here they act
# as a snapshot: if any of these numbers moves, either a bug was fixed -- in
# which case the reference must be regenerated on both sides -- or something
# drifted. Both are worth stopping for.
#
# Two of these numbers exist because the cross-language comparison found real
# faults: the exact-test branch aborted on a table this size, and the inertia
# share was normalised to the dimensions that happened to be kept, so it always
# added up to 100 percent.

ref <- jsonlite::fromJSON(testthat::test_path("stats-reference.json"),
                          simplifyVector = TRUE)
frag <- utils::read.csv(testthat::test_path("stats-fixture.csv"),
                        stringsAsFactors = FALSE)

test_that("the fixture is the one the reference was made from", {
  expect_equal(nrow(frag), ref$n_rows)
  expect_setequal(unique(frag$code), ref$codes)
})

test_that("the chi-squared result is unchanged", {
  res <- suppressWarnings(qda_chisq(frag, group = "citekey"))
  expect_equal(res$statistic, ref$chisq$statistic, tolerance = 1e-12)
  expect_equal(res$cramers_v, ref$chisq$cramers_v, tolerance = 1e-12)
  expect_equal(res$n, ref$chisq$n)
  expect_equal(res$expected_ok, ref$chisq$expected_ok)
  # this fixture must keep exercising the simulated branch -- it is the branch
  # that used to abort
  expect_false(res$expected_ok)
  expect_match(res$test$method, "simulated")
})

test_that("the Jaccard distances are unchanged", {
  d <- as.matrix(qda_code_distance(frag, min_n = 3))
  expect_setequal(rownames(d), ref$codes)
  expect_equal(unname(d[ref$codes, ref$codes]),
               unname(ref$distance), tolerance = 1e-12)
})

test_that("the clustering is unchanged", {
  expect_equal(qda_cluster(frag, min_n = 3)$cophenetic, ref$cophenetic,
               tolerance = 1e-12)
})

test_that("the code map is unchanged up to sign", {
  # scaling coordinates are defined up to reflection, so the distances between
  # the points are compared rather than the coordinates themselves
  pts <- qda_mds(frag, min_n = 3)$points
  m <- as.matrix(pts[match(ref$codes, pts$code), c("dim1", "dim2")])
  rownames(m) <- ref$codes
  got <- as.matrix(stats::dist(m))
  expect_equal(unname(got[ref$codes, ref$codes]), unname(ref$mds_distance),
               tolerance = 1e-12)
})

test_that("the correspondence analysis is unchanged", {
  ca <- qda_ca(frag)
  expect_equal(as.numeric(ca$inertia), ref$ca$inertia, tolerance = 1e-12)
  expect_equal(ca$total_inertia, ref$ca$total_inertia, tolerance = 1e-12)
  expect_equal(as.numeric(ca$inertia_share), ref$ca$inertia_share,
               tolerance = 1e-12)
  # the share must stay honest: two of four dimensions are not the whole table
  expect_lt(sum(ca$inertia_share), 1)
})

test_that("the agreement measures are unchanged", {
  got <- qda_agreement(qda_units(frag))
  for (measure in c("percent", "cohen", "brennan", "fleiss", "alpha", "ac1")) {
    want <- ref$agreement[[measure]]
    expect_equal(got[[measure]], if (is.null(want)) NA_real_ else want,
                 tolerance = 1e-12, info = measure)
  }
})
