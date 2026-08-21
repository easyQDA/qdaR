test_that("new codes are counted per document in coding order", {
  h <- data.frame(
    ts = sprintf("2026-01-%02dT09:00:00Z", 1:6), user = "ann", action = "add",
    code = c("A", "B", "A", "C", "A", "B"),
    citekey = c("d1", "d1", "d2", "d2", "d3", "d3"),
    stringsAsFactors = FALSE
  )
  n <- qda_new_codes(h)
  expect_equal(n$document, c("d1", "d2", "d3"))
  expect_equal(n$new_codes, c(2L, 1L, 0L))   # A,B then C then nothing new
  expect_equal(n$cumulative, c(2L, 3L, 3L))
})

test_that("the saturation ratio follows Guest, Namey and Chen", {
  # base = first 4 documents, 4+3+2+1 = 10 codes; runs of 2 overlapping by one
  #   5-6: 1 new -> 0.10, above the 5 % threshold
  #   6-7: 0 new -> 0.00, at or below -> saturation, named for document 5
  r <- qda_saturation_ratio(c(4, 3, 2, 1, 1, 0, 0, 0))
  expect_equal(r$base_codes, 10)
  expect_equal(r$notation, "5+2")
  expect_equal(r$saturated_at, 5L)
  expect_equal(r$runs$ratio[1], 0.1)
  expect_false(r$runs$ok[1])
  expect_true(r$runs$ok[2])
})

test_that("a stricter threshold never declares saturation earlier", {
  strict <- qda_saturation_ratio(c(4, 3, 2, 1, 1, 1, 0, 0), threshold = 0)
  lax <- qda_saturation_ratio(c(4, 3, 2, 1, 1, 1, 0, 0), threshold = 0.2)
  expect_gte(strict$saturated_at, lax$saturated_at)
})

test_that("material that keeps producing codes never saturates", {
  r <- qda_saturation_ratio(c(4, 4, 4, 4, 4, 4, 4, 4))
  expect_null(r$notation)
  expect_null(r$saturated_at)
})

test_that("a question the data cannot answer says why", {
  expect_null(qda_saturation_ratio(c(1, 2))$notation)
  expect_match(qda_saturation_ratio(c(1, 2))$reason, "too few")
  expect_match(qda_saturation_ratio(rep(0, 8))$reason, "base")
})

test_that("the run length travels with the number", {
  # "5+2" and "5+3" are different claims, so the notation carries both
  expect_equal(qda_saturation_ratio(c(4, 3, 2, 1, 0, 0, 0),
                                    run_length = 3)$notation, "4+3")
})

test_that("a coder who changed everything has a distance of one", {
  h <- data.frame(
    ts = sprintf("2026-01-%02dT09:00:00Z", 1:16), user = "ann", action = "add",
    code = c(rep("A", 8), rep("B", 8)), citekey = "d1", stringsAsFactors = FALSE
  )
  d <- qda_code_drift(h, windows = 2)
  expect_equal(d$distance, c(0, 1))
  expect_equal(d$n, c(8L, 8L))
})

test_that("a steady coder shows little drift, and coders stay apart", {
  h <- data.frame(
    ts = sprintf("2026-01-%02dT09:00:00Z", 1:12),
    user = rep(c("ann", "bob"), each = 6), action = "add",
    code = rep(c("A", "B"), 6), citekey = "d1", stringsAsFactors = FALSE
  )
  d <- qda_code_drift(h, windows = 3)
  expect_setequal(d$coder, c("ann", "bob"))
  expect_lt(max(d$distance), 0.35)
})

test_that("an empty log gives an empty table, not an error", {
  h <- data.frame(ts = character(), user = character(), action = character(),
                  code = character(), citekey = character(),
                  stringsAsFactors = FALSE)
  expect_equal(nrow(qda_code_drift(h)), 0L)
  expect_equal(nrow(qda_new_codes(h)), 0L)
})
