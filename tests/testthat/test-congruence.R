# qdaR and the qdaZ plugin compute the same reliability coefficients in two
# languages.  Two implementations that agree on a hand-picked example prove
# very little; these fixtures were generated at random -- varying coders,
# units, categories, deliberately skewed marginals and missing ratings --
# run through the plugin's JavaScript, and the results frozen here.  If a
# figure ever moves in one implementation and not the other, this fails.

ref <- jsonlite::fromJSON(
  testthat::test_path("qdaz-reference.json"), simplifyDataFrame = FALSE)

# jsonlite hands back a matrix when every row has the same length and a list
# of rows when a skipped rating made one of them ragged
as_matrix <- function(units) {
  m <- if (is.matrix(units)) {
    matrix(as.character(units), nrow = nrow(units))
  } else {
    rows <- lapply(units, function(r) vapply(r, function(v) {
      if (is.null(v)) NA_character_ else as.character(v)
    }, character(1)))
    do.call(rbind, rows)
  }
  colnames(m) <- paste0("coder", seq_len(ncol(m)))
  m
}

row_width <- function(units) {
  if (is.matrix(units)) ncol(units) else length(units[[1]])
}

# a coefficient the plugin left undefined arrives as JSON null
num <- function(x) if (is.null(x) || !length(x) || is.na(x)) NA_real_ else as.numeric(x)

same <- function(a, b) {
  a <- num(a)
  b <- num(b)
  if (is.na(a) && is.na(b)) return(TRUE)
  !is.na(a) && !is.na(b) && abs(a - b) < 1e-12
}

test_that("the frozen reference covers the awkward cases", {
  expect_gt(length(ref), 50)
  shapes <- vapply(ref, function(f) as.integer(row_width(f$units)), integer(1))
  expect_true(all(c(2L, 3L, 4L) %in% shapes))          # two and more coders
  has_na <- vapply(ref, function(f) any(is.na(as_matrix(f$units))), logical(1))
  expect_true(any(has_na))                              # skipped ratings
  # and the values really do span the range where the coefficients diverge
  fleiss <- vapply(ref, function(f) num(f$expected$fleiss), numeric(1))
  expect_lt(min(fleiss, na.rm = TRUE), 0.05)
  expect_gt(max(fleiss, na.rm = TRUE), 0.3)
  expect_gt(sum(has_na), 20L)
})

test_that("every coefficient matches the qdaZ plugin", {
  bad <- character(0)
  for (i in seq_along(ref)) {
    m <- as_matrix(ref[[i]]$units)
    e <- ref[[i]]$expected
    two <- ncol(m) == 2L
    got <- list(
      percent = qda_percent_agreement(m),
      cohen   = if (two) qda_kappa(m) else NA_real_,
      brennan = if (two) qda_brennan(m) else NA_real_,
      fleiss  = qda_fleiss(m),
      alpha   = qda_alpha(m),
      ac1     = qda_ac1(m)
    )
    for (k in names(got)) {
      if (!same(got[[k]], e[[k]])) {
        bad <- c(bad, sprintf("fixture %d, %s: qdaR %s vs qdaZ %s",
                              i, k, got[[k]], e[[k]]))
      }
    }
  }
  expect_equal(bad, character(0))
})

test_that("undefined cases are undefined in both implementations", {
  # a coefficient the plugin reports as NaN must not become a number here
  nan_seen <- 0L
  for (i in seq_along(ref)) {
    e <- ref[[i]]$expected
    for (k in names(e)) if (is.na(num(e[[k]]))) nan_seen <- nan_seen + 1L
  }
  # three-and-four-coder fixtures leave Cohen and Brennan undefined
  expect_gt(nan_seen, 0L)
})
