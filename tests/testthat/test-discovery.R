test_that("the direct IW estimate is exact algebra from the two end points", {
  # Lowe et al. (a16): A and b follow from T_1 and T_N alone, and putting
  # them back into the IW curve must return T_N exactly
  N <- 8; T1 <- 8; TN <- 21
  A <- (N - 1) * T1 * TN / (N * T1 - TN)
  b <- (TN - N * T1) / ((1 - N) * TN)
  expect_equal(A * b * N / (1 + b * (N - 1)), TN)
  expect_equal(A * b * 1 / (1 + b * 0), T1)
})

test_that("a flattening curve gives a high index, a linear one a low index", {
  flattening <- qda_saturation_index(c(8, 13, 16, 18, 19, 20, 20, 21), "IW")
  linear <- qda_saturation_index(c(5, 10, 15, 20, 25, 30, 35, 40), "IW")
  expect_gt(flattening$index, 70)
  expect_lt(linear$index, 20)
  # a curve still rising in a straight line implies a huge unseen remainder
  expect_gt(linear$A, 100)
})

test_that("all three models fit, and none is required to agree with the others", {
  y <- c(8, 13, 16, 18, 19, 20, 20, 21)
  fits <- lapply(c("IS", "IW", "SW"), function(m) qda_saturation_index(y, m))
  for (f in fits) {
    expect_true(is.finite(f$A))
    expect_true(is.finite(f$b) && f$b > 0 && f$b < 1)
    expect_lt(f$rmse, 1)                      # each describes the data
    expect_equal(length(f$fitted), length(y))
  }
  # they differ: that is the point of reporting which one was used
  expect_gt(diff(range(vapply(fits, function(f) f$A, numeric(1)))), 1)
})

test_that("the index is the share of the estimated total, floored as published", {
  f <- qda_saturation_index(c(8, 13, 16, 18, 19, 20, 20, 21), "IW")
  expect_equal(f$index, 100 * 21 / floor(f$A))
})

test_that("the SW model survives numbers that would overflow a gamma", {
  # gamma(1 - b + n) overflows past n of about 170; the logs must not
  y <- cumsum(c(20, rep(1, 250)))
  f <- qda_saturation_index(y, "SW")
  expect_true(is.finite(f$A))
  expect_true(all(is.finite(f$fitted)))
})

test_that("the model predicts how much more material a target needs", {
  f <- qda_saturation_index(c(8, 13, 16, 18, 19, 20, 20, 21), "IW")
  at90 <- qda_documents_for(f, 90)
  at95 <- qda_documents_for(f, 95)
  expect_lte(at90, at95)
  expect_gte(at95, 8)                # never fewer than what is already read
})

test_that("a target the model never reaches says NA rather than a guess", {
  f <- qda_saturation_index(c(5, 10, 15, 20, 25, 30, 35, 40), "IW")
  expect_true(is.na(qda_documents_for(f, 99, max_n = 20)))
})

test_that("too little material is refused with a reason", {
  r <- qda_saturation_index(c(1, 2))
  expect_true(is.na(r$index))
  expect_match(r$reason, "fewer than three")
  expect_match(qda_saturation_index(c(0, 0, 0, 0))$reason, "no themes")
})
