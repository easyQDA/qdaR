#' Segments from a fragments export
#'
#' Turns the position columns into the segments the unitizing measures work
#' on.  Only `positionKind == "text"` gives a continuum to measure boundaries
#' on; PDF rectangles do not, and are dropped with a warning rather than
#' quietly approximated.
#'
#' The position columns arrived with a later version of the plugin.  An older
#' export simply does not have them, and this function says so instead of
#' returning an empty result that looks like disagreement.
#'
#' @inheritParams qda_units
#' @return A list of data frames, one per coder, each with `start`, `end` and
#'   `value`.
#' @examples
#' frag <- data.frame(
#'   codedBy = c("ann", "bob"), code = c("A", "A"),
#'   positionKind = c("text", "text"),
#'   positionStart = c(0, 5), positionEnd = c(20, 22)
#' )
#' qda_segments(frag)
#' @export
qda_segments <- function(fragments, coder = "codedBy", value = "code") {
  needed <- c("positionKind", "positionStart", "positionEnd")
  missing <- setdiff(needed, names(fragments))
  if (length(missing)) {
    stop("this export predates the position columns (missing: ",
         paste(missing, collapse = ", "),
         "); re-export from a current zotQDA to measure unitizing",
         call. = FALSE)
  }
  keep <- fragments$positionKind == "text" &
    is.finite(fragments$positionStart) & is.finite(fragments$positionEnd) &
    fragments$positionEnd > fragments$positionStart
  if (any(fragments$positionKind == "pdf", na.rm = TRUE)) {
    warning("PDF segments have no linear continuum and were dropped; ",
            "unitizing measures apply to text sources", call. = FALSE)
  }
  d <- fragments[keep, , drop = FALSE]
  if (!nrow(d)) return(list())
  split_by <- as.character(d[[coder]])
  out <- lapply(split(seq_len(nrow(d)), split_by), function(i) {
    s <- data.frame(start = as.numeric(d$positionStart[i]),
                    end = as.numeric(d$positionEnd[i]),
                    value = as.character(d[[value]][i]),
                    stringsAsFactors = FALSE)
    s[order(s$start), , drop = FALSE]
  })
  out[order(names(out))]
}

# Internal, and split the same way in all three implementations (qdaZ's
# stats.js and qdaPy's unitizing.py) so the three can still be read side by
# side against the paper: one function per numbered equation.

seg_overlap <- function(a, b) max(0, min(a$end, b$end) - max(a$start, b$start))
seg_union <- function(a, b) max(a$end, b$end) - min(a$start, b$start)

# Equation (16) for ONE pair of coders. Every unit of A against every unit
# of B it touches; a unit the other coder did not mark sits inside their gap
# and is charged twice its length, in both directions. Gaps are never
# compared with each other.
pair_disorder <- function(A, B, delta2) {
  total <- 0
  n <- 0L
  for (ai in seq_len(nrow(A))) {
    u <- A[ai, ]
    matched <- FALSE
    for (bi in seq_len(nrow(B))) {
      v <- B[bi, ]
      if (seg_overlap(u, v) <= 0) next
      matched <- TRUE
      total <- total + seg_union(u, v) -
        seg_overlap(u, v) * (1 - delta2(u$value, v$value))
      n <- n + 1L
    }
    if (!matched) { total <- total + 2 * (u$end - u$start); n <- n + 1L }
  }
  for (bi in seq_len(nrow(B))) {
    v <- B[bi, ]
    touches <- any(vapply(seq_len(nrow(A)),
                          function(ai) seg_overlap(A[ai, ], v) > 0, logical(1)))
    if (!touches) { total <- total + 2 * (v$end - v$start); n <- n + 1L }
  }
  list(total = total, n = n)
}

# Equation (17): the pairwise disorder summed over all pairs of coders.
observed_disorder <- function(coders, delta2) {
  total <- 0
  n <- 0L
  for (i in seq_len(length(coders) - 1L)) {
    for (j in seq(i + 1L, length(coders))) {
      part <- pair_disorder(coders[[i]], coders[[j]], delta2)
      total <- total + part$total
      n <- n + part$n
    }
  }
  list(total = total, n = n)
}

# Equation (18): every unit against every other, itself excluded, ignoring
# where on the continuum they sit. This is the chance baseline.
expected_disorder <- function(all_units, delta2) {
  n <- nrow(all_units)
  len <- all_units$end - all_units$start
  num <- 0
  den <- 0
  for (a in seq_len(n)) {
    for (b in seq_len(n)) {
      if (a == b) next
      num <- num + len[a]^2 + len[b]^2 +
        len[a] * len[b] * delta2(all_units$value[a], all_units$value[b])
      den <- den + len[a] + len[b]
    }
  }
  if (!den) NaN else num / den
}

#' Krippendorff's alpha for unitizing
#'
#' Every other coefficient in this package assumes the segments already line
#' up and only asks whether the categories agree.  That assumption does a lot
#' of work.  This one asks the prior question: did the coders mark the same
#' stretches of text at all?
#'
#' Krippendorff (1995) \doi{10.2307/271061}, in the form given in the
#' replacement of section 12.4 of *Content Analysis* (3rd ed.), equations
#' 16 to 19.  Gaps are not compared with each other -- two coders agreeing
#' that a stretch is irrelevant is not evidence of reliable unitizing.
#'
#' Established QDA software settles this with a single overlap threshold,
#' yes or no.  What that discards is precisely the information about how the
#' boundaries differ.
#'
#' @param segments A list of coders' segments, from [qda_segments()].
#' @param metric Squared difference between two values; the default is
#'   nominal (0 when equal, 1 otherwise).  Pass `function(a, b) 0` to measure
#'   identification alone and ignore the codes.
#'
#' @section Comparing the two:
#' Ignoring the categories lowers the observed disagreement, but it lowers
#' the *expected* disagreement too, because randomly paired units no longer
#' differ by category either.  Which effect wins depends on whether the
#' coders actually disagreed about categories: where they did, alpha rises;
#' where they agreed throughout, alpha can fall.  Compare the two `Do`
#' values, not the two alphas.
#' @return A list with `alpha`, the observed and expected disagreement `Do`
#'   and `De`, the number of `intersections` behind `Do` and the number of
#'   `units`; or `NA` when fewer than two coders contributed.
#' @examples
#' ann <- data.frame(start = c(0, 40), end = c(20, 60), value = c("A", "B"))
#' bob <- data.frame(start = c(0, 40), end = c(20, 60), value = c("A", "B"))
#' qda_unitizing_alpha(list(ann, bob))$alpha        # 1
#' @export
qda_unitizing_alpha <- function(segments, metric = NULL) {
  delta2 <- if (is.function(metric)) metric else
    function(a, b) if (identical(as.character(a), as.character(b))) 0 else 1
  coders <- Filter(function(s) is.data.frame(s) && nrow(s) > 0, segments)
  if (length(coders) < 2L) return(NA)

  obs <- observed_disorder(coders, delta2)
  if (!obs$n) return(NA)
  Do <- obs$total / obs$n

  all_units <- do.call(rbind, coders)
  n <- nrow(all_units)
  if (n < 2L) return(NA)
  De <- expected_disorder(all_units, delta2)
  if (!is.finite(De) || De == 0) return(NA)
  list(alpha = 1 - Do / De, Do = Do, De = De,
       intersections = obs$n, units = n)
}

# Boundary positions as a 0/1 vector over the continuum.
boundary_vector <- function(segments, length_) {
  v <- integer(max(0, length_))
  if (!is.data.frame(segments) || !nrow(segments)) return(v)
  for (p in c(segments$start, segments$end)) {
    if (p > 0 && p < length(v)) v[p + 1L] <- 1L
  }
  v
}

boundaries_in <- function(vec, i, k) {
  hi <- min(i + k, length(vec) - 1L)
  if (hi < i + 1L) return(0L)
  sum(vec[seq(i + 2L, hi + 1L)])
}

window_size <- function(vec, L) {
  segments <- sum(vec) + 1
  # floor(x + 0.5), not round(): R and Python round halves to even while
  # JavaScript rounds them up, and a window width off by one gives a
  # different error rate. All three must pick the same width.
  max(1, floor(L / segments / 2 + 0.5))
}

#' WindowDiff and Pk: how far apart are two segmentations?
#'
#' Two error rates from text segmentation, reported together because they
#' disagree in an informative way.  Both slide a window across the continuum;
#' `qda_window_diff()` compares how many boundaries each segmentation puts in
#' it, `qda_pk()` only whether there is one at all.  A spurious extra
#' boundary therefore costs something in the first and nothing in the second.
#'
#' Neither is chance-corrected -- for that, use [qda_unitizing_alpha()].
#' What they offer instead is comparability with the segmentation
#' literature, and a number that behaves sensibly for near misses: a boundary
#' two characters off is nearly right, and both measures say so.
#'
#' Pevzner and Hearst (2002) \doi{10.1162/089120102317341756};
#' Beeferman, Berger and Lafferty (1999) \doi{10.1023/A:1007506220214}.
#'
#' @param reference,hypothesis Segment data frames, as from [qda_segments()].
#' @param length_ Length of the continuum in characters.
#' @param k Window width; the default is half the average reference segment.
#' @return A number between 0 and 1; 0 means the boundaries coincide.
#' @examples
#' ref <- data.frame(start = c(0, 20, 40), end = c(20, 40, 60))
#' hyp <- data.frame(start = c(0, 22, 40), end = c(22, 40, 60))
#' qda_window_diff(ref, ref, 60)   # 0
#' qda_window_diff(ref, hyp, 60)
#' qda_pk(ref, hyp, 60)
#' @export
qda_window_diff <- function(reference, hypothesis, length_, k = NULL) {
  L <- as.integer(length_)
  if (is.na(L) || L < 2L) return(NA_real_)
  ref <- boundary_vector(reference, L)
  hyp <- boundary_vector(hypothesis, L)
  width <- if (is.null(k)) window_size(ref, L) else as.integer(k)
  if (width < 1L || width >= L) return(NA_real_)
  idx <- seq(0L, L - width - 1L)
  wrong <- sum(vapply(idx, function(i)
    boundaries_in(ref, i, width) != boundaries_in(hyp, i, width), logical(1)))
  wrong / length(idx)
}

#' @rdname qda_window_diff
#' @export
qda_pk <- function(reference, hypothesis, length_, k = NULL) {
  L <- as.integer(length_)
  if (is.na(L) || L < 2L) return(NA_real_)
  ref <- boundary_vector(reference, L)
  hyp <- boundary_vector(hypothesis, L)
  width <- if (is.null(k)) window_size(ref, L) else as.integer(k)
  if (width < 1L || width >= L) return(NA_real_)
  idx <- seq(0L, L - width - 1L)
  wrong <- sum(vapply(idx, function(i)
    (boundaries_in(ref, i, width) == 0L) != (boundaries_in(hyp, i, width) == 0L),
    logical(1)))
  wrong / length(idx)
}
