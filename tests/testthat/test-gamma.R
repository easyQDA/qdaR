U <- function(s, e, v) list(start = s, end = e, value = v)
BEST <- rep(list(list(U(0, 10, "A"), U(20, 30, "B"), U(40, 50, "C"))), 3)
MIDDLE <- list(
  list(U(0, 10, "A"), U(20, 30, "A"), U(40, 50, "C")),
  list(U(0, 10, "A"), U(20, 30, "B"), U(40, 50, "C")),
  list(U(22, 32, "B"), U(40, 50, "C"))
)
WORST <- list(list(U(0, 10, "A")), list(U(20, 30, "B")), list(U(40, 50, "C")))

test_that("the positional dissimilarity matches equation three", {
  # ((|0-2| + |10-12|) / ((10-0) + (12-2)))^2 = (4/20)^2 = 0.04
  expect_equal(qda_gamma_dissimilarity(U(0, 10, "A"), U(2, 12, "A")), 0.04)
  expect_equal(qda_gamma_dissimilarity(U(0, 10, "A"), U(0, 10, "A")), 0)
})

test_that("position and category are added, not traded off", {
  expect_equal(qda_gamma_dissimilarity(U(0, 10, "A"), U(0, 10, "B")), 1)
  expect_equal(qda_gamma_dissimilarity(U(0, 10, "A"), U(0, 10, "B"),
                                       dist_cat = function(a, b) 0.5), 0.5)
})

test_that("an unaligned unit costs Delta_empty", {
  expect_equal(qda_gamma_dissimilarity(U(0, 10, "A"), NULL), 1)
  expect_equal(qda_gamma_dissimilarity(NULL, NULL), 1)
  # so a unitary alignment holding a single real unit costs exactly that
  expect_equal(gamma_unitary_disorder(list(U(0, 10, "A"), NULL, NULL)), 1)
  expect_equal(gamma_unitary_disorder(rep(list(U(0, 10, "A")), 3)), 0)
})

test_that("the three configurations of Figure 11 behave as published", {
  best <- qda_gamma(BEST, samples = 20, seed = 42)
  middle <- qda_gamma(MIDDLE, samples = 20, seed = 42)
  worst <- qda_gamma(WORST, samples = 20, seed = 42)
  expect_equal(best$gamma, 1)
  expect_equal(best$observed, 0)
  expect_lt(worst$gamma, 0)            # worse than annotating at random
  expect_lt(worst$gamma, middle$gamma)
  expect_lt(middle$gamma, best$gamma)
})

test_that("the worst case disorder comes out as three, by hand", {
  # three lone units: each unitary alignment costs Delta_empty, the mean
  # number of units per annotator is 1, so the disorder is 3
  expect_equal(qda_gamma(WORST, samples = 5)$observed, 3)
})

test_that("gamma reports the alignment it found", {
  r <- qda_gamma(BEST, samples = 10)
  expect_equal(length(r$alignment), 3L)
  expect_true(all(lengths(r$alignment) == 3L))
})

test_that("the result is reproducible and the seed matters", {
  a <- qda_gamma(MIDDLE, samples = 20, seed = 42)
  expect_equal(a$gamma, qda_gamma(MIDDLE, samples = 20, seed = 42)$gamma)
  expect_false(isTRUE(all.equal(a$gamma,
                                qda_gamma(MIDDLE, samples = 20, seed = 7)$gamma)))
})

test_that("one annotator is not an agreement question", {
  expect_null(qda_gamma(list(list(U(0, 10, "A")))))
})

test_that("refusing beats approximating", {
  r <- qda_gamma(MIDDLE, samples = 5, max_nodes = 1)
  expect_true(r$exhausted)
  expect_true(is.nan(r$gamma))
  expect_match(r$reason, "exact")
})

test_that("the pruning theorem discards candidates", {
  dense <- list(
    list(U(0, 10, "A"), U(12, 22, "A"), U(24, 34, "A")),
    list(U(0, 10, "A"), U(12, 22, "A"), U(60, 70, "B"))
  )
  expect_lt(qda_gamma(dense, samples = 5)$candidates, (3 + 1) * (3 + 1) - 1)
})

test_that("a best alignment can pair units that do not overlap", {
  # exactly what the unitizing alpha cannot express
  r <- qda_gamma_best_alignment(list(list(U(0, 10, "A")), list(U(11, 21, "A"))))
  expect_equal(length(r$alignment), 1L)
  expect_equal(length(r$alignment[[1]]), 2L)
})

test_that("gamma matches the plugin on every frozen fixture", {
  ref <- jsonlite::fromJSON(testthat::test_path("qdaz-gamma-reference.json"),
                            simplifyVector = FALSE)
  bad <- character(0)
  for (i in seq_along(ref$cases)) {
    coders <- lapply(ref$cases[[i]]$fixture$coders, function(segs)
      lapply(segs, function(s) list(start = s$start, end = s$end, value = s$value)))
    r <- qda_gamma(coders, samples = 12, seed = 42)
    e <- ref$cases[[i]]$expected
    for (k in c("gamma", "observed", "expected")) {
      if (abs(as.numeric(r[[k]]) - as.numeric(e[[k]])) >= 1e-12) {
        bad <- c(bad, sprintf("case %d, %s: qdaR %s vs qdaZ %s", i, k,
                              r[[k]], e[[k]]))
      }
    }
    if (r$candidates != e$candidates) {
      bad <- c(bad, sprintf("case %d, candidates", i))
    }
  }
  expect_equal(bad, character(0))
})

test_that("the reference is the file the Python package uses", {
  ours <- testthat::test_path("qdaz-gamma-reference.json")
  theirs <- file.path("..", "..", "..", "qdaPy", "tests",
                      "qdaz-gamma-reference.json")
  skip_if_not(file.exists(theirs), "qdaPy sources not present")
  expect_equal(readBin(ours, "raw", file.size(ours)),
               readBin(theirs, "raw", file.size(theirs)))
})
