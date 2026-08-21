# Delta_empty. A constant of the method, not a parameter (Mathet et al.,
# section 4.4.1): the cost above which two units count as critically
# different, and the cost of leaving a unit unaligned.
GAMMA_EMPTY <- 1

# Equation (3): positional dissimilarity. The numerator adds the two boundary
# differences, the denominator the two lengths, so the measure is scale-free;
# squaring makes it grow faster as the positions drift apart.
gamma_pos <- function(u, v) {
  span <- (u$end - u$start) + (v$end - v$start)
  if (span <= 0) return(GAMMA_EMPTY)
  r <- (abs(u$start - v$start) + abs(u$end - v$end)) / span
  r * r * GAMMA_EMPTY
}

# Equation (4): categorical dissimilarity from a category distance in [0, 1].
# The default is the nominal case, which is what a code system gives you
# unless someone has declared that two codes partly overlap in meaning.
gamma_cat <- function(u, v, dist_cat) {
  d <- if (is.function(dist_cat)) dist_cat(u$value, v$value) else
    if (identical(as.character(u$value), as.character(v$value))) 0 else 1
  max(0, min(1, d)) * GAMMA_EMPTY
}

#' Dissimilarity between two units, as gamma defines it
#'
#' Equation (5) of Mathet, Widlocher and Metivier (2015)
#' \doi{10.1162/coli_a_00227}, with both weights at one: position and
#' category are added, so a unit in the right place with the wrong code costs
#' the same as one with the right code in a badly wrong place.
#'
#' @param u,v Units, each a list with `start`, `end` and `value`.  `NULL`
#'   stands for the empty unit, which costs `Delta_empty` against anything.
#' @param dist_cat Optional category distance in `[0, 1]`; nominal by default.
#' @param alpha,beta Weights for position and category.
#' @return A number.
#' @examples
#' u <- list(start = 0, end = 10, value = "A")
#' qda_gamma_dissimilarity(u, list(start = 2, end = 12, value = "A"))  # 0.04
#' qda_gamma_dissimilarity(u, list(start = 0, end = 10, value = "B"))  # 1
#' @export
qda_gamma_dissimilarity <- function(u, v, dist_cat = NULL, alpha = 1, beta = 1) {
  if (is.null(u) || is.null(v)) return(GAMMA_EMPTY)
  alpha * gamma_pos(u, v) + beta * gamma_cat(u, v, dist_cat)
}

# Equation (6): the disorder of a unitary alignment is the AVERAGE
# dissimilarity over all C(n,2) annotator pairs, so it does not grow with the
# number of annotators. A pair involving an empty unit costs Delta_empty, and
# so does a pair of two empties -- which is why a unitary alignment holding
# one real unit costs exactly Delta_empty.
gamma_unitary_disorder <- function(tuple, dist_cat = NULL, alpha = 1, beta = 1) {
  n <- length(tuple)
  if (n < 2) return(0)
  total <- 0
  pairs <- 0
  for (i in seq_len(n - 1)) {
    for (j in seq(i + 1, n)) {
      total <- total + qda_gamma_dissimilarity(tuple[[i]], tuple[[j]],
                                               dist_cat, alpha, beta)
      pairs <- pairs + 1
    }
  }
  total / pairs
}

# Candidate unitary alignments: every combination of at most one unit per
# annotator, at least one real. Equation (9) discards any whose disorder
# exceeds n * Delta_empty, because such a combination can always be replaced
# by separate single-unit alignments.
gamma_candidates <- function(by_coder, dist_cat, alpha, beta) {
  n <- length(by_coder)
  limit <- n * GAMMA_EMPTY + 1e-12
  out <- list()
  build <- function(coder, tuple, members) {
    if (coder > n) {
      if (!length(members)) return(invisible(NULL))
      d <- gamma_unitary_disorder(tuple, dist_cat, alpha, beta)
      if (d <= limit) {
        out[[length(out) + 1L]] <<- list(members = members, disorder = d)
      }
      return(invisible(NULL))
    }
    build(coder + 1L, c(tuple, list(NULL)), members)
    units <- by_coder[[coder]]
    for (k in seq_along(units)) {
      build(coder + 1L, c(tuple, list(units[[k]])),
            c(members, paste0(coder, ":", k)))
    }
  }
  build(1L, list(), character(0))
  out
}

#' The best alignment, and the disorder of an annotation set
#'
#' Gamma does not fix the alignment before measuring: it searches for the
#' pairing of units across annotators that minimises the combined positional
#' and categorical disorder, and reports agreement with respect to that.
#' Finding it is a set-partitioning problem, NP-hard for three or more
#' annotators.
#'
#' This is an exact branch and bound using the paper's pruning theorem
#' (equation 9) plus an admissible bound.  When the search would exceed
#' `max_nodes` it **refuses**: a gamma produced by a heuristic is not gamma,
#' and reporting one would be worse than reporting nothing.
#'
#' @param by_coder A list, one entry per annotator, of lists of units.
#' @param dist_cat Optional category distance.
#' @param alpha,beta Weights for position and category.
#' @param max_nodes Search budget.
#' @return A list with `disorder`, the `alignment` as unit identifiers, the
#'   number of `candidates` after pruning, and `exhausted`.
#' @examples
#' u <- function(s, e, v) list(start = s, end = e, value = v)
#' qda_gamma_best_alignment(list(list(u(0, 10, "A")), list(u(0, 10, "A"))))$disorder
#' @export
qda_gamma_best_alignment <- function(by_coder, dist_cat = NULL, alpha = 1,
                                     beta = 1, max_nodes = 200000) {
  n <- length(by_coder)
  ids <- unlist(lapply(seq_len(n), function(c)
    if (length(by_coder[[c]])) paste0(c, ":", seq_along(by_coder[[c]])) else
      character(0)))
  if (!length(ids)) return(NULL)
  idx <- stats::setNames(seq_along(ids), ids)

  cands <- gamma_candidates(by_coder, dist_cat, alpha, beta)
  by_unit <- vector("list", length(ids))
  for (ci in seq_along(cands)) {
    for (m in cands[[ci]]$members) {
      i <- idx[[m]]
      by_unit[[i]] <- c(by_unit[[i]], ci)
    }
  }
  if (any(vapply(by_unit, length, integer(1)) == 0L)) return(NULL)
  cheapest <- vapply(by_unit, function(v)
    min(vapply(v, function(ci) cands[[ci]]$disorder, numeric(1))), numeric(1))

  covered <- rep(FALSE, length(ids))
  best <- Inf
  best_set <- NULL
  nodes <- 0L
  exhausted <- FALSE

  search <- function(chosen, cost) {
    if (exhausted) return(invisible(NULL))
    nodes <<- nodes + 1L
    if (nodes > max_nodes) { exhausted <<- TRUE; return(invisible(NULL)) }
    first <- which(!covered)[1]
    if (is.na(first)) {
      if (cost < best) { best <<- cost; best_set <<- chosen }
      return(invisible(NULL))
    }
    if (cost + sum(cheapest[!covered]) / n >= best) return(invisible(NULL))
    options <- by_unit[[first]]
    options <- options[order(vapply(options,
                                    function(ci) cands[[ci]]$disorder, numeric(1)))]
    for (ci in options) {
      members <- cands[[ci]]$members
      if (any(covered[idx[members]])) next
      covered[idx[members]] <<- TRUE
      search(c(chosen, ci), cost + cands[[ci]]$disorder)
      covered[idx[members]] <<- FALSE
      if (exhausted) return(invisible(NULL))
    }
    invisible(NULL)
  }
  search(integer(0), 0)
  if (exhausted || is.null(best_set)) {
    return(list(exhausted = TRUE, nodes = nodes, candidates = length(cands)))
  }
  mean_units <- length(ids) / n
  list(disorder = best / mean_units, raw_disorder = best,
       alignment = lapply(best_set, function(ci) cands[[ci]]$members),
       mean_units = mean_units, nodes = nodes,
       candidates = length(cands), exhausted = FALSE)
}

# The circular shift of section 5.2.1: split each annotator's units at a
# random position and swap the parts. Every unit keeps its length and its
# category; only the alignment between annotators is destroyed.
gamma_shift <- function(units, length_, offset) {
  out <- lapply(units, function(u) {
    start <- (u$start + offset) %% length_
    list(start = start, end = start + (u$end - u$start), value = u$value)
  })
  out[order(vapply(out, function(u) u$start, numeric(1)))]
}

#' Agreement measured with respect to the best alignment
#'
#' Every other coefficient in this package fixes the alignment first and then
#' measures agreement on it.  Gamma (Mathet, Widlocher & Metivier 2015,
#' \doi{10.1162/coli_a_00227}) refuses that separation: unitizing and
#' categorisation are judged together, and the measure reports the alignment
#' it found alongside the number.
#'
#' The practical difference from [qda_unitizing_alpha()] is that gamma can
#' pair two units that do not overlap at all, when the surrounding
#' configuration says they refer to the same phenomenon.  Alpha cannot
#' express that.
#'
#' Chance correction is by sampling: the annotations are randomly shifted
#' around the continuum, which preserves every unit's length and category and
#' destroys only the alignment.  The generator is the plugin's, seeded, so all
#' three implementations report the same expected value.
#'
#' @inheritParams qda_gamma_best_alignment
#' @param samples Number of random continua for the expected disorder.
#' @param seed Seed for the shifts.
#' @return A list with `gamma`, the `observed` and `expected` disorder, the
#'   `alignment`, `samples`, and `recommended_samples` -- the number the
#'   observed variability suggests for two per cent precision (the paper's
#'   sampling rule).  `gamma` is `NaN` with a `reason` when the search was
#'   cut short.
#' @examples
#' u <- function(s, e, v) list(start = s, end = e, value = v)
#' same <- list(u(0, 10, "A"), u(20, 30, "B"))
#' qda_gamma(list(same, same), samples = 10)$gamma      # 1
#' @export
qda_gamma <- function(by_coder, dist_cat = NULL, alpha = 1, beta = 1,
                      samples = 30, seed = 42, max_nodes = 200000) {
  coders <- lapply(by_coder, function(list_) {
    keep <- Filter(function(u) is.finite(u$start) && is.finite(u$end) &&
                     u$end > u$start, list_)
    keep[order(vapply(keep, function(u) u$start, numeric(1)))]
  })
  if (length(coders) < 2 || any(vapply(coders, length, integer(1)) == 0L)) {
    return(NULL)
  }
  observed <- qda_gamma_best_alignment(coders, dist_cat, alpha, beta, max_nodes)
  if (is.null(observed) || isTRUE(observed$exhausted)) {
    return(list(gamma = NaN, exhausted = TRUE,
                candidates = if (is.null(observed)) 0 else observed$candidates,
                reason = paste("the search for the best alignment exceeded",
                               "max_nodes; gamma is exact or it is nothing")))
  }
  span <- max(vapply(unlist(coders, recursive = FALSE),
                     function(u) u$end, numeric(1)))
  rng <- mulberry32(seed)
  values <- numeric(0)
  for (s in seq_len(samples)) {
    offsets <- floor(rng(length(coders)) * span)
    shifted <- lapply(seq_along(coders), function(i)
      gamma_shift(coders[[i]], span, offsets[i]))
    r <- qda_gamma_best_alignment(shifted, dist_cat, alpha, beta, max_nodes)
    if (!is.null(r) && !isTRUE(r$exhausted)) values <- c(values, r$disorder)
  }
  if (!length(values) || mean(values) <= 0) {
    return(list(gamma = NaN, observed = observed$disorder, expected = NaN,
                reason = "the expected disorder came out as zero"))
  }
  m <- mean(values)
  sd <- if (length(values) > 1) stats::sd(values) else 0
  list(gamma = 1 - observed$disorder / m,
       observed = observed$disorder, expected = m, expected_sd = sd,
       samples = length(values),
       recommended_samples = if (m > 0)
         ceiling(((sd / m) * stats::qnorm(0.975) / 0.02)^2) else 0,
       alignment = observed$alignment, candidates = observed$candidates,
       exhausted = FALSE, reason = "")
}
