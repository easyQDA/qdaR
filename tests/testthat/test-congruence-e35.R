# The new measures, held to the plugin's numbers. Same principle as
# test-congruence.R: fixtures generated at random, computed by the plugin's
# JavaScript, frozen here. The reference file is byte-identical to the one in
# qdaPy, so all three implementations answer to one set of numbers rather
# than to each other.

ref <- jsonlite::fromJSON(testthat::test_path("qdaz-e35-reference.json"),
                          simplifyVector = FALSE)

num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
same <- function(a, b) {
  a <- num(a); b <- num(b)
  (is.na(a) && is.na(b)) || (!is.na(a) && !is.na(b) && abs(a - b) < 1e-12)
}
as_df <- function(segs) do.call(rbind, lapply(segs, function(s)
  data.frame(start = s$start, end = s$end,
             value = if (is.null(s$value)) NA_character_ else as.character(s$value),
             stringsAsFactors = FALSE)))

test_that("the frozen reference covers the awkward cases", {
  expect_gte(length(ref$unitizing), 25)
  widths <- unique(vapply(ref$unitizing, function(f) length(f$fixture$coders),
                          integer(1)))
  expect_true(all(c(2L, 3L) %in% widths))
  notations <- vapply(ref$saturation,
                      function(f) if (is.null(f$expected$notation)) ""
                                  else f$expected$notation, character(1))
  expect_true("" %in% notations)          # saturation that is never reached
  expect_gt(length(unique(notations)), 2)
  ks <- vapply(ref$wilson, function(f) f$fixture$k, numeric(1))
  expect_true(0 %in% ks)                  # a proportion at the edge
})

test_that("unitizing matches the plugin on every fixture", {
  bad <- character(0)
  for (i in seq_along(ref$unitizing)) {
    f <- ref$unitizing[[i]]$fixture; e <- ref$unitizing[[i]]$expected
    coders <- lapply(f$coders, as_df)
    a <- qda_unitizing_alpha(coders)
    ident <- qda_unitizing_alpha(coders, function(x, y) 0)
    got <- list(alpha = if (is.list(a)) a$alpha else NA,
                Do = if (is.list(a)) a$Do else NA,
                De = if (is.list(a)) a$De else NA,
                identAlpha = if (is.list(ident)) ident$alpha else NA,
                wd = qda_window_diff(coders[[1]], coders[[2]], f$length),
                pk = qda_pk(coders[[1]], coders[[2]], f$length))
    for (k in names(got)) {
      if (!same(got[[k]], e[[k]])) {
        bad <- c(bad, sprintf("unitizing %d, %s: qdaR %s vs qdaZ %s",
                              i, k, got[[k]], e[[k]]))
      }
    }
    inter <- if (is.list(a)) a$intersections else NA
    if (!same(inter, e$inter)) bad <- c(bad, sprintf("unitizing %d, inter", i))
  }
  expect_equal(bad, character(0))
})

test_that("the saturation metric matches the plugin on every fixture", {
  bad <- character(0)
  for (i in seq_along(ref$saturation)) {
    f <- ref$saturation[[i]]$fixture; e <- ref$saturation[[i]]$expected
    r <- qda_saturation_ratio(unlist(f$counts), base_size = f$base,
                              run_length = f$run, threshold = f$threshold)
    rn <- if (is.null(r$notation)) "" else r$notation
    en <- if (is.null(e$notation)) "" else e$notation
    if (!identical(rn, en)) {
      bad <- c(bad, sprintf("saturation %d: %s vs %s", i, rn, en))
    }
    if (!same(r$base_codes, e$base)) bad <- c(bad, sprintf("saturation %d base", i))
    for (j in seq_along(e$ratios)) {
      if (!same(r$runs$ratio[j], e$ratios[[j]])) {
        bad <- c(bad, sprintf("saturation %d ratio %d", i, j))
      }
    }
  }
  expect_equal(bad, character(0))
})

test_that("the Wilson interval matches the plugin on every fixture", {
  bad <- character(0)
  for (i in seq_along(ref$wilson)) {
    f <- ref$wilson[[i]]$fixture; e <- ref$wilson[[i]]$expected
    w <- qda_wilson(f$k, f$n)
    for (k in c("estimate", "lo", "hi")) {
      if (!same(w[[k]], e[[k]])) bad <- c(bad, sprintf("wilson %d %s", i, k))
    }
  }
  expect_equal(bad, character(0))
})

test_that("the reference is the file the Python package uses", {
  ours <- testthat::test_path("qdaz-e35-reference.json")
  theirs <- file.path("..", "..", "..", "qdaPy", "tests", "qdaz-e35-reference.json")
  skip_if_not(file.exists(theirs), "qdaPy sources not present")
  expect_equal(readBin(ours, "raw", file.size(ours)),
               readBin(theirs, "raw", file.size(theirs)))
})
