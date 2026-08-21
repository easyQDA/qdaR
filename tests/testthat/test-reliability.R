test_that("the paradox indices explain a collapsed kappa", {
  # Feinstein & Cicchetti's case, worked by hand:
  # a=3, b=0, c=1, d=0; po = 3/4; Cohen's pe = .75 -> kappa = 0
  u <- cbind(ann = c("A", "A", "A", "B"), bob = c("A", "A", "A", "A"))
  expect_equal(qda_kappa(u), 0)
  d <- qda_paradox(u)
  expect_equal(d$prevalence_index, 0.75)
  expect_equal(d$bias_index, 0.25)
  expect_equal(d$pabak, 0.5)
  expect_equal(d$percent, 0.75)
  expect_equal(unname(d$table), c(3, 0, 1, 0))
})

test_that("the paradox indices refuse cases they do not describe", {
  expect_null(qda_paradox(cbind(a = "A", b = "A", c = "A")))
  expect_null(qda_paradox(cbind(a = c("A", "B"), b = c("B", "C"))))
})

test_that("balanced marginals leave kappa intact", {
  bal <- cbind(ann = c("A", "A", "B", "B", "A"),
               bob = c("A", "A", "B", "B", "B"))
  expect_lt(qda_paradox(bal)$prevalence_index, 0.25)
  expect_gt(qda_kappa(bal), 0.5)
})

test_that("the Wilson interval stays inside the unit interval", {
  w <- qda_wilson(2, 40)
  expect_equal(w$estimate, 0.05)
  expect_gt(w$lo, 0.013); expect_lt(w$lo, 0.015)
  expect_gt(w$hi, 0.164); expect_lt(w$hi, 0.166)
  for (case in list(c(0, 10), c(10, 10), c(1, 3))) {
    v <- qda_wilson(case[1], case[2])
    expect_gte(v$lo, 0); expect_lte(v$hi, 1); expect_lte(v$lo, v$hi)
  }
  # the interval must contain the estimate even at the edges
  for (case in list(c(0, 10), c(10, 10), c(1, 1), c(7, 7))) {
    v <- qda_wilson(case[1], case[2])
    expect_lte(v$lo, v$estimate)
    expect_lte(v$estimate, v$hi)
  }
  expect_true(is.na(qda_wilson(1, 0)$estimate))
  expect_true(is.na(qda_wilson(5, 3)$estimate))
})

test_that("the random generator reproduces the plugin's, bit for bit", {
  # the whole point of reimplementing mulberry32 instead of using R's RNG:
  # an interval reported by qdaR must be the interval the plugin reports
  r <- mulberry32(42)
  expect_equal(round(r(6), 12),
               c(0.601103751920, 0.448290558998, 0.852465793490,
                 0.669734041439, 0.174813898746, 0.526592542185))
  expect_equal(round(mulberry32(7)(1), 12), round(mulberry32(7)(1), 12))
  expect_false(isTRUE(all.equal(mulberry32(1)(1), mulberry32(2)(1))))
})

test_that("the bootstrap interval brackets the estimate and is reproducible", {
  set.seed(1)
  u <- cbind(ann = rep(c("A", "B"), 30),
             bob = rep(c("A", "B", "B", "A"), 15))
  ci <- qda_bootstrap_ci(u, qda_kappa, resamples = 300)
  expect_gte(ci$estimate, ci$lo)
  expect_lte(ci$estimate, ci$hi)
  expect_equal(ci, qda_bootstrap_ci(u, qda_kappa, resamples = 300))
  expect_false(isTRUE(all.equal(
    ci$lo, qda_bootstrap_ci(u, qda_kappa, resamples = 300, seed = 7)$lo)))
  # too little to say -> nothing, rather than an interval computed from noise
  expect_null(qda_bootstrap_ci(cbind(a = "A", b = "A"), qda_kappa,
                               resamples = 50))
})

test_that("agreement is reported per code with its prevalence", {
  frag <- data.frame(
    annotationKey = rep(paste0("s", 1:6), each = 2),
    codedBy = rep(c("ann", "bob"), 6),
    code = c("A", "A", "A", "B", "B", "B", "A", "A", "B", "B", "A", "A"),
    stringsAsFactors = FALSE
  )
  out <- qda_agreement_by_code(frag, min_n = 1)
  expect_setequal(out$code, c("A", "B"))
  expect_true(all(out$percent >= 0 & out$percent <= 1))
  expect_true(all(out$lo <= out$prevalence & out$prevalence <= out$hi))
  # ordered by how often the code was used, so the important ones come first
  expect_equal(out$n, sort(out$n, decreasing = TRUE))
  # min_n does drop rare codes
  expect_equal(nrow(qda_agreement_by_code(frag, min_n = 99)), 0L)
})
