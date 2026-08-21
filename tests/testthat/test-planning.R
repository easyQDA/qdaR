# Donner and Rotondi (2010), Table 2. Their tables were computed with the
# chi-squared critical value rounded to 2.71; with the exact quantile eight
# of the forty-eight cells come out one smaller, which is the mathematically
# correct answer. Both are checked.
PAPER_TABLE2 <- list(
  list(k0 = 0.50, kL = 0.40, n = c(559, 373, 301, 255, 264, 146, 112, 95,
                                   228, 120, 89, 76)),
  list(k0 = 0.60, kL = 0.40, n = c(140, 94, 76, 64, 66, 37, 28, 24,
                                   57, 30, 23, 19)),
  list(k0 = 0.70, kL = 0.60, n = c(463, 311, 247, 207, 205, 124, 99, 87,
                                   174, 102, 81, 73)),
  list(k0 = 0.80, kL = 0.60, n = c(116, 78, 62, 52, 52, 31, 25, 22,
                                   44, 26, 21, 19))
)
cells <- function(k0, kL, critical = NULL) {
  out <- integer(0)
  for (p in c(0.1, 0.3, 0.5)) {
    for (raters in c(2, 3, 4, 5)) {
      out <- c(out, qda_plan_kappa(k0, kL, p, raters, critical = critical))
    }
  }
  out
}

test_that("the published table is reproduced cell for cell with its own critical value", {
  for (row in PAPER_TABLE2) {
    expect_equal(cells(row$k0, row$kL, critical = 2.71), row$n,
                 info = paste(row$k0, row$kL))
  }
})

test_that("the exact quantile is never larger than the rounded one", {
  for (row in PAPER_TABLE2) {
    exact <- cells(row$k0, row$kL)
    expect_true(all(exact <= row$n))
    expect_true(all(row$n - exact <= 1), info = "and never off by more than one")
  }
})

test_that("more coders and a more balanced code both reduce the material needed", {
  expect_gt(qda_plan_kappa(0.8, 0.6, 0.1, raters = 2),
            qda_plan_kappa(0.8, 0.6, 0.1, raters = 4))
  expect_gt(qda_plan_kappa(0.8, 0.6, 0.1, raters = 2),
            qda_plan_kappa(0.8, 0.6, 0.3, raters = 2))
})

test_that("an unreachable bound is infinite rather than a large number", {
  expect_equal(qda_plan_kappa(0.6, 0.6, 0.3), Inf)
  expect_equal(qda_plan_kappa(0.6, 0.7, 0.3), Inf)
})

test_that("the lower bound and the sample size are inverses of each other", {
  n <- qda_plan_kappa(0.8, 0.6, 0.3, raters = 3)
  expect_gte(qda_kappa_lower(n, 0.8, 0.3, raters = 3), 0.6)
  # one segment short and the bound falls below the target
  expect_lt(qda_kappa_lower(n - 1, 0.8, 0.3, raters = 3), 0.6)
})

test_that("more material buys a higher lower bound", {
  small <- qda_kappa_lower(30, 0.8, 0.3)
  large <- qda_kappa_lower(300, 0.8, 0.3)
  expect_lt(small, large)
  expect_lt(large, 0.8)          # never above the anticipated kappa itself
})

# Fugard and Potts (2015), Table 1: 80 % power.
PAPER_TABLE1 <- list(
  "0.05" = c(32, 59, 85, 110, 134, 249, 471, 687),
  "0.10" = c(16, 29, 42, 54, 66, 124, 234, 343),
  "0.25" = c(6, 11, 16, 21, 26, 49, 93, 136),
  "0.50" = c(3, 5, 8, 10, 12, 24, 45, 66),
  "0.95" = c(1, 2, 3, 4, 6, 11, 22, 33)
)

test_that("Fugard and Potts' table is reproduced exactly", {
  wanted <- c(1, 2, 3, 4, 5, 10, 20, 30)
  for (p in names(PAPER_TABLE1)) {
    got <- vapply(wanted, function(k) qda_plan_themes(as.numeric(p), k),
                  numeric(1))
    expect_equal(got, PAPER_TABLE1[[p]], info = p)
  }
})

test_that("power and sample size are two views of one calculation", {
  n <- qda_plan_themes(0.05, instances = 1, power = 0.8)
  expect_gte(qda_theme_power(n, 0.05, 1), 0.8)
  expect_lt(qda_theme_power(n - 1, 0.05, 1), 0.8)
})

test_that("a rarer theme or more wanted instances needs more documents", {
  expect_gt(qda_plan_themes(0.05), qda_plan_themes(0.20))
  expect_gt(qda_plan_themes(0.20, instances = 5),
            qda_plan_themes(0.20, instances = 1))
})

test_that("a theme present in everyone needs one document", {
  expect_equal(qda_plan_themes(1, instances = 1), 1)
  expect_equal(qda_theme_power(0, 0.5, 1), 0)
})
