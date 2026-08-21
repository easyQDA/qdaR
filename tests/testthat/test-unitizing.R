test_that("the unitizing alpha reproduces Krippendorff's worked example", {
  # Content Analysis, 3rd ed., replacement of section 12.4, Figure 12.11.
  # The figure prints the five intersections that make up the observed
  # disagreement:
  #   (15-15(1-0)) + (23-5(1-1)) + 2*3 + (10-5(1-1)) + (5-5(1-0))
  #     = 0 + 23 + 6 + 10 + 0 = 39, over N_o = 5  ->  uD_o = 7.8
  # D_e is not checked: it depends on the individual unit lengths, which the
  # figure does not print.
  i <- data.frame(start = c(0, 20, 60, 70), end = c(15, 38, 65, 75),
                  value = c(1, 1, 2, 1))
  j <- data.frame(start = c(0, 33, 50, 60, 70), end = c(15, 43, 53, 65, 80),
                  value = c(1, 3, 4, 2, 5))
  r <- qda_unitizing_alpha(list(i, j))
  expect_equal(r$intersections, 5L)
  expect_equal(r$Do, 7.8)
  expect_equal(r$units, 9L)
})

test_that("the unitizing alpha has the three anchors any alpha has", {
  same <- data.frame(start = 0, end = 10, value = "A")
  expect_equal(qda_unitizing_alpha(list(same, same))$alpha, 1)
  # same stretch, different code: identification perfect, coding not
  other <- data.frame(start = 0, end = 10, value = "B")
  expect_lt(qda_unitizing_alpha(list(same, other))$alpha, 1)
  expect_equal(qda_unitizing_alpha(list(same, other),
                                   function(a, b) 0)$alpha, 1)
  # segments that miss each other entirely are worse than chance
  away <- data.frame(start = 50, end = 60, value = "A")
  expect_lt(qda_unitizing_alpha(list(same, away))$alpha, 0)
  # one coder is not a reliability test
  expect_true(is.na(qda_unitizing_alpha(list(same))))
})

test_that("ignoring the categories can only improve agreement", {
  i <- data.frame(start = c(0, 20), end = c(15, 38), value = c(1, 1))
  j <- data.frame(start = c(0, 33), end = c(15, 43), value = c(1, 3))
  nominal <- qda_unitizing_alpha(list(i, j))
  ident <- qda_unitizing_alpha(list(i, j), function(a, b) 0)
  expect_lt(ident$Do, nominal$Do)
  expect_gt(ident$alpha, nominal$alpha)
})

test_that("WindowDiff and Pk are zero for identical segmentations", {
  ref <- data.frame(start = c(0, 20, 40), end = c(20, 40, 60))
  expect_equal(qda_window_diff(ref, ref, 60), 0)
  expect_equal(qda_pk(ref, ref, 60), 0)
})

test_that("a missing boundary costs more than a shifted one", {
  ref <- data.frame(start = c(0, 20, 40), end = c(20, 40, 60))
  near <- data.frame(start = c(0, 22, 40), end = c(22, 40, 60))
  far <- data.frame(start = c(0, 40), end = c(40, 60))
  expect_gt(qda_window_diff(ref, near, 60), 0)
  expect_gt(qda_window_diff(ref, far, 60), qda_window_diff(ref, near, 60))
  expect_gt(qda_pk(ref, far, 60), qda_pk(ref, near, 60))
})

test_that("a continuum too short for a window has no answer", {
  ref <- data.frame(start = c(0, 20), end = c(20, 40))
  expect_true(is.na(qda_window_diff(ref, ref, 1)))
  expect_true(is.na(qda_pk(ref, ref, 1)))
})

test_that("segments come out of the position columns, per coder", {
  frag <- data.frame(
    codedBy = c("ann", "ann", "bob"), code = c("A", "B", "A"),
    positionKind = c("text", "text", "text"),
    positionStart = c(0, 30, 5), positionEnd = c(20, 50, 22),
    stringsAsFactors = FALSE
  )
  segs <- qda_segments(frag)
  expect_equal(names(segs), c("ann", "bob"))
  expect_equal(nrow(segs$ann), 2L)
  expect_equal(segs$ann$start, c(0, 30))
})

test_that("PDF segments are dropped with a warning, not approximated", {
  frag <- data.frame(
    codedBy = c("ann", "bob"), code = "A",
    positionKind = c("text", "pdf"),
    positionStart = c(0, NA), positionEnd = c(20, NA),
    stringsAsFactors = FALSE
  )
  expect_warning(segs <- qda_segments(frag), "PDF")
  expect_equal(names(segs), "ann")
})

test_that("an export predating the position columns says so", {
  frag <- data.frame(codedBy = "ann", code = "A", stringsAsFactors = FALSE)
  expect_error(qda_segments(frag), "predates the position columns")
})
