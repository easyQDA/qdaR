#' Build the unit-by-coder matrix
#'
#' Intercoder measures need one row per unit of analysis and one column per
#' coder.  The fragments export is longer than that -- one row per annotation
#' and code -- so it has to be reshaped, and two decisions have to be made
#' explicitly rather than by accident.
#'
#' **Segments nobody coded are a category, not a gap.** Agreement about what
#' is *not* relevant is agreement.  Pass the `uncoded` export and those
#' segments enter as their own category; leave it out and the figures only
#' describe the segments at least one coder marked, which is a different and
#' usually more flattering question.
#'
#' **A segment two coders coded twice is set aside.** Where a coder gave one
#' segment several codes there is no single value to compare, so the cell
#' becomes missing and is counted in the `multi` attribute.  The honest way
#' to include those segments is the per-code binary view,
#' [qda_units_binary()].  Reporting an overall figure that quietly dropped a
#' tenth of the material is not.
#'
#' @param fragments A fragments data frame from [qda_read_fragments()].
#' @param uncoded Optionally the matching uncoded export, so segments no
#'   coder coded become their own category.
#' @param unit Column identifying the unit of analysis.
#' @param coder Column identifying the coder.
#' @param value Column holding the category; `"code"` is the readable path,
#'   `"codeId"` the identity that survives renaming.
#' @param no_code Label for a segment a coder left uncoded.
#' @param level Flatten paths to this many levels first; see
#'   [qda_level_agreement()].
#'
#' @return A character matrix, units in rows, coders in columns, `NA` where a
#'   coder did not rate a unit, with the attribute `multi` giving the number
#'   of cells set aside because of multiple coding.
#' @examples
#' frag <- data.frame(
#'   annotationKey = c("s1", "s1", "s2", "s2"),
#'   codedBy = c("ann", "bob", "ann", "bob"),
#'   code = c("A", "A", "B", "A")
#' )
#' qda_units(frag)
#' @export
qda_units <- function(fragments, uncoded = NULL, unit = "annotationKey",
                      coder = "codedBy", value = "code",
                      no_code = "(no code)", level = NULL) {
  stopifnot(is.data.frame(fragments))
  need <- c(unit, coder, value)
  miss <- setdiff(need, names(fragments))
  if (length(miss)) {
    stop("fragments is missing: ", paste(miss, collapse = ", "), call. = FALSE)
  }

  d <- fragments[nzchar(fragments[[value]]), c(unit, coder, value)]
  names(d) <- c("unit", "coder", "value")
  if (!is.null(level)) d$value <- qda_flatten_path(d$value, level)

  coders <- sort(unique(as.character(d$coder)))
  units <- unique(as.character(d$unit))

  if (!is.null(uncoded)) {
    stopifnot(is.data.frame(uncoded))
    units <- unique(c(units, as.character(uncoded[[unit]])))
  }
  if (!length(units) || !length(coders)) {
    m <- matrix(NA_character_, 0, length(coders),
                dimnames = list(NULL, coders))
    attr(m, "multi") <- 0L
    return(m)
  }

  m <- matrix(NA_character_, length(units), length(coders),
              dimnames = list(units, coders))

  # every coder who coded anywhere is taken to have seen every segment, so
  # "coded nothing here" is a judgement rather than a missing observation
  if (!is.null(uncoded)) m[] <- no_code

  key <- paste(d$unit, d$coder, sep = "\r")
  split_vals <- split(as.character(d$value), key)
  multi <- 0L
  for (k in names(split_vals)) {
    parts <- strsplit(k, "\r", fixed = TRUE)[[1]]
    vals <- unique(split_vals[[k]])
    if (length(vals) > 1L) {
      multi <- multi + 1L
      m[parts[1], parts[2]] <- NA_character_
    } else {
      m[parts[1], parts[2]] <- vals
    }
  }
  attr(m, "multi") <- multi
  m
}

#' The per-code binary view
#'
#' Turns one code into a yes/no judgement per unit, which is how a
#' multiply-coded body of material can still be assessed: every code is asked
#' about separately, and a segment carrying three codes contributes to all
#' three questions instead of being dropped.
#'
#' @inheritParams qda_units
#' @param code The code to ask about.
#' @return A character matrix as in [qda_units()], with values `"yes"` and
#'   `"no"`.
#' @examples
#' frag <- data.frame(
#'   annotationKey = c("s1", "s1", "s2", "s2"),
#'   codedBy = c("ann", "bob", "ann", "bob"),
#'   code = c("A", "A", "B", "A")
#' )
#' qda_units_binary(frag, "A")
#' @export
qda_units_binary <- function(fragments, code, unit = "annotationKey",
                             coder = "codedBy", value = "code",
                             uncoded = NULL) {
  stopifnot(is.data.frame(fragments), length(code) == 1L)
  d <- fragments[, c(unit, coder, value)]
  names(d) <- c("unit", "coder", "value")
  coders <- sort(unique(as.character(d$coder)[nzchar(d$value)]))
  units <- unique(as.character(d$unit))
  if (!is.null(uncoded)) units <- unique(c(units, as.character(uncoded[[unit]])))
  m <- matrix("no", length(units), length(coders),
              dimnames = list(units, coders))
  hit <- d[d$value == code, , drop = FALSE]
  for (i in seq_len(nrow(hit))) {
    u <- as.character(hit$unit[i]); c_ <- as.character(hit$coder[i])
    if (u %in% units && c_ %in% coders) m[u, c_] <- "yes"
  }
  attr(m, "multi") <- 0L
  m
}

#' Shorten a code path to a number of levels
#'
#' `"Belastung/beruflich/akut"` at level 2 becomes `"Belastung/beruflich"`.
#'
#' @param path Code paths.
#' @param level Number of levels to keep; `NULL` or `0` keeps everything.
#' @return A character vector.
#' @examples
#' qda_flatten_path("Belastung/beruflich/akut", 2)
#' @export
qda_flatten_path <- function(path, level = NULL) {
  p <- as.character(path)
  p[is.na(p)] <- ""
  if (is.null(level) || level < 1) return(p)
  vapply(strsplit(p, "/", fixed = TRUE), function(parts) {
    paste(parts[seq_len(min(length(parts), level))], collapse = "/")
  }, character(1))
}

# --- the measures -----------------------------------------------------

pairable <- function(units) {
  apply(units, 1, function(r) sum(!is.na(r)) >= 2L)
}

#' Observed agreement between coders
#'
#' The share of agreeing coder pairs, over all units and all pairs where
#' both coders rated.  Easy to read and, on its own, easy to over-read: with
#' one dominant category a high value says little.
#'
#' @param units A unit-by-coder matrix from [qda_units()].
#' @return A number between 0 and 1, or `NaN` when nothing is comparable.
#' @examples
#' u <- cbind(ann = c("A", "B", "A"), bob = c("A", "B", "B"))
#' qda_percent_agreement(u)
#' @export
qda_percent_agreement <- function(units) {
  agree <- 0; total <- 0
  for (i in seq_len(nrow(units))) {
    r <- units[i, ]
    r <- r[!is.na(r)]
    if (length(r) < 2L) next
    for (a in seq_along(r)) for (b in seq_len(length(r))[-seq_len(a)]) {
      total <- total + 1
      if (identical(r[[a]], r[[b]])) agree <- agree + 1
    }
  }
  if (!total) return(NaN)
  agree / total
}

#' Cohen's kappa
#'
#' Chance-corrected agreement for exactly two coders, on the units both
#' rated.  Chance is estimated from the coders' own marginals, which is what
#' makes kappa fall when one category dominates -- the paradox that keeps
#' being mistaken for a defect of the coding.
#'
#' Cohen (1960) \doi{10.1177/001316446002000104}.
#'
#' @inheritParams qda_percent_agreement
#' @return A number, or `NaN` when kappa is undefined.
#' @examples
#' u <- cbind(ann = c("A", "B", "A", "B"), bob = c("A", "B", "B", "B"))
#' qda_kappa(u)
#' @export
qda_kappa <- function(units) {
  if (ncol(units) != 2L) {
    stop("Cohen's kappa is for two coders; this matrix has ", ncol(units),
         " -- use qda_fleiss() or qda_alpha()", call. = FALSE)
  }
  keep <- !is.na(units[, 1]) & !is.na(units[, 2])
  a <- units[keep, 1]; b <- units[keep, 2]
  n <- length(a)
  if (!n) return(NaN)
  cats <- sort(unique(c(a, b)))
  po <- sum(a == b) / n
  pe <- sum(vapply(cats, function(c_) mean(a == c_) * mean(b == c_), numeric(1)))
  if (pe >= 1) return(NaN)
  (po - pe) / (1 - pe)
}

#' Brennan and Prediger's kappa
#'
#' Like Cohen's, but chance is the uniform `1/q` over the categories the
#' scheme offers rather than the coders' marginals.  This is the figure
#' MAXQDA reports, so it is the one to use when a result has to line up with
#' a MAXQDA output.
#'
#' Brennan and Prediger (1981) \doi{10.1177/001316448104100307}.
#'
#' @inheritParams qda_percent_agreement
#' @param q Number of categories the scheme offers.  Defaults to the
#'   categories present anywhere in `units` -- pass the size of the code
#'   system when coders could have chosen codes they never used, because that
#'   is the number the coefficient is actually about.
#' @return A number, or `NaN` when nothing is comparable.
#' @examples
#' u <- cbind(ann = c("A", "B", "A", "B"), bob = c("A", "B", "B", "B"))
#' qda_brennan(u)
#' @export
qda_brennan <- function(units, q = NULL) {
  if (ncol(units) != 2L) {
    stop("Brennan and Prediger's kappa is defined here for two coders",
         call. = FALSE)
  }
  keep <- !is.na(units[, 1]) & !is.na(units[, 2])
  a <- units[keep, 1]; b <- units[keep, 2]
  n <- length(a)
  if (!n) return(NaN)
  seen <- length(unique(as.vector(units[!is.na(units)])))
  k <- max(2L, if (is.null(q)) seen else as.integer(q))
  po <- sum(a == b) / n
  (po - 1 / k) / (1 - 1 / k)
}

#' Fleiss' kappa
#'
#' Chance-corrected agreement for any number of coders.  Units rated by
#' fewer than two coders carry no agreement information and are skipped.
#'
#' Because it works on one category per unit, this is the figure that makes
#' the case for coding a segment once: a scheme where segments routinely
#' carry several codes has no single value to compare, and the overall figure
#' is then computed on whatever remains unambiguous.  [qda_units()] counts
#' what it set aside, and the count is worth reporting next to the kappa.
#'
#' Fleiss (1971) \doi{10.1037/h0031619}.
#'
#' @inheritParams qda_percent_agreement
#' @return A number, or `NaN` when kappa is undefined.
#' @examples
#' u <- cbind(ann = c("A", "B", "A"), bob = c("A", "B", "B"),
#'            cat = c("A", "B", "A"))
#' qda_fleiss(u)
#' @export
qda_fleiss <- function(units) {
  cats <- sort(unique(as.vector(units[!is.na(units)])))
  if (length(cats) < 2L) return(NaN)
  pa_sum <- 0; n_units <- 0L
  cat_total <- stats::setNames(numeric(length(cats)), cats)
  rating_total <- 0
  for (i in seq_len(nrow(units))) {
    vals <- units[i, ]; vals <- vals[!is.na(vals)]
    r <- length(vals)
    if (r < 2L) next
    n_units <- n_units + 1L
    rating_total <- rating_total + r
    counts <- table(vals)
    pa_sum <- pa_sum + sum(counts * (counts - 1)) / (r * (r - 1))
    cat_total[names(counts)] <- cat_total[names(counts)] + as.numeric(counts)
  }
  if (!n_units || !rating_total) return(NaN)
  pa <- pa_sum / n_units
  pe <- sum((cat_total / rating_total)^2)
  if (pe >= 1) return(NaN)
  (pa - pe) / (1 - pe)
}

#' Krippendorff's alpha
#'
#' Chance-corrected agreement for any number of coders that tolerates
#' missing values, computed from the coincidence matrix.  Nominal data only
#' here, which is what codes are.
#'
#' Hayes and Krippendorff (2007) \doi{10.1080/19312450709336664}.
#'
#' @inheritParams qda_percent_agreement
#' @return A number, or `NaN` when alpha is undefined.
#' @examples
#' u <- cbind(ann = c("A", "B", "A", NA), bob = c("A", "B", "B", "A"))
#' qda_alpha(u)
#' @export
qda_alpha <- function(units) {
  o <- list(); nc <- list(); n <- 0
  bump <- function(lst, key, by) {
    lst[[key]] <- (if (is.null(lst[[key]])) 0 else lst[[key]]) + by
    lst
  }
  for (i in seq_len(nrow(units))) {
    vals <- units[i, ]; vals <- as.character(vals[!is.na(vals)])
    m <- length(vals)
    if (m < 2L) next
    w <- 1 / (m - 1)
    for (a in seq_len(m)) for (b in seq_len(m)) {
      if (a == b) next
      o <- bump(o, paste(vals[a], vals[b], sep = "\r"), w)
      nc <- bump(nc, vals[a], w)
      n <- n + w
    }
  }
  if (n <= 1) return(NaN)
  d_obs <- 0
  for (key in names(o)) {
    ab <- strsplit(key, "\r", fixed = TRUE)[[1]]
    if (ab[1] != ab[2]) d_obs <- d_obs + o[[key]]
  }
  cats <- names(nc)
  d_exp <- 0
  for (a in cats) for (b in cats) if (a != b) d_exp <- d_exp + nc[[a]] * nc[[b]]
  d_exp <- d_exp / (n - 1)
  if (d_exp == 0) return(NaN)
  1 - d_obs / d_exp
}

#' Gwet's AC1
#'
#' Chance-corrected agreement that stays stable when one category dominates,
#' the situation in which kappa collapses although the coders plainly agree.
#' Worth reporting beside kappa rather than instead of it: where the two
#' diverge, the marginals are the story.
#'
#' Gwet (2008) \doi{10.1348/000711006X126600}.
#'
#' @inheritParams qda_percent_agreement
#' @return A number, or `NaN` when AC1 is undefined.
#' @examples
#' u <- cbind(ann = c("A", "A", "A", "B"), bob = c("A", "A", "A", "A"))
#' qda_ac1(u)
#' @export
qda_ac1 <- function(units) {
  cats <- sort(unique(as.vector(units[!is.na(units)])))
  q <- length(cats)
  if (q < 2L) return(NaN)
  pa_sum <- 0; n_units <- 0L
  pi_sum <- stats::setNames(numeric(q), cats)
  for (i in seq_len(nrow(units))) {
    vals <- units[i, ]; vals <- vals[!is.na(vals)]
    r <- length(vals)
    if (r < 2L) next
    n_units <- n_units + 1L
    counts <- table(vals)
    pa_sum <- pa_sum + sum(counts * (counts - 1)) / (r * (r - 1))
    share <- stats::setNames(numeric(q), cats)
    share[names(counts)] <- as.numeric(counts) / r
    pi_sum <- pi_sum + share
  }
  if (!n_units) return(NaN)
  pa <- pa_sum / n_units
  pi <- pi_sum / n_units
  pe <- sum(pi * (1 - pi)) / (q - 1)
  if (pe >= 1) return(NaN)
  (pa - pe) / (1 - pe)
}

#' All agreement measures at once
#'
#' Reports the measures side by side, because no single coefficient settles
#' the question: they disagree exactly where the marginals are skewed, and
#' seeing them disagree is the finding.
#'
#' @inheritParams qda_percent_agreement
#' @return A one-row data frame with the number of comparable units, the
#'   number of categories, and the measures.  Cohen's kappa is `NA` for more
#'   than two coders.
#' @examples
#' u <- cbind(ann = c("A", "B", "A", "B"), bob = c("A", "B", "B", "B"))
#' qda_agreement(u)
#' @export
qda_agreement <- function(units) {
  n <- sum(pairable(units))
  cats <- unique(as.vector(units[!is.na(units)]))
  data.frame(
    units = n,
    coders = ncol(units),
    categories = length(cats),
    multi_set_aside = as.integer(attr(units, "multi") %||% 0L),
    percent = qda_percent_agreement(units),
    cohen = if (ncol(units) == 2L) qda_kappa(units) else NA_real_,
    brennan = if (ncol(units) == 2L) qda_brennan(units) else NA_real_,
    fleiss = qda_fleiss(units),
    alpha = qda_alpha(units),
    ac1 = qda_ac1(units),
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Agreement by level of the code system
#'
#' A hierarchical code system can be read at several resolutions, and coders
#' who disagree about `Belastung/beruflich` against `Belastung/privat` still
#' agree that the segment is about `Belastung`.  Flattening paths level by
#' level and recomputing shows where in the hierarchy the agreement is lost
#' -- which is a statement about the code system, not about the coders.
#'
#' @param units A unit-by-coder matrix from [qda_units()], values being code
#'   paths.
#' @param max_level Deepest level to report; defaults to the deepest path.
#' @return A data frame with one row per level, holding the categories and
#'   comparable units at that level and all measures.
#' @examples
#' u <- cbind(ann = c("A/x", "A/y", "B/x"), bob = c("A/y", "A/y", "B/x"))
#' qda_level_agreement(u)
#' @export
qda_level_agreement <- function(units, max_level = NULL) {
  vals <- as.vector(units[!is.na(units)])
  depth <- if (is.null(max_level)) {
    max(1L, max(lengths(strsplit(vals, "/", fixed = TRUE)), 0L))
  } else as.integer(max_level)
  rows <- lapply(seq_len(depth), function(level) {
    flat <- units
    flat[!is.na(flat)] <- qda_flatten_path(flat[!is.na(flat)], level)
    attr(flat, "multi") <- attr(units, "multi")
    out <- qda_agreement(flat)
    cbind(level = level, out)
  })
  do.call(rbind, rows)
}

#' Plot agreement across the levels of the code system
#'
#' @inheritParams qda_level_agreement
#' @param measures Which measures to draw.
#' @return A 'ggplot2' object.
#' @examples
#' u <- cbind(ann = c("A/x", "A/y", "B/x"), bob = c("A/y", "A/y", "B/x"))
#' qda_plot_level_agreement(u)
#' @export
qda_plot_level_agreement <- function(units, max_level = NULL,
                                     measures = c("percent", "fleiss", "alpha")) {
  d <- qda_level_agreement(units, max_level = max_level)
  measures <- intersect(measures, names(d))
  long <- do.call(rbind, lapply(measures, function(m) {
    data.frame(level = d$level, measure = m, value = d[[m]],
               stringsAsFactors = FALSE)
  }))
  ggplot2::ggplot(long, ggplot2::aes(x = long$level, y = long$value,
                                     colour = long$measure)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(breaks = unique(long$level)) +
    ggplot2::labs(x = "levels of the code system kept", y = NULL,
                  colour = NULL) +
    ggplot2::theme_minimal()
}

#' Where two coders disagreed
#'
#' The confusion table is what turns a disappointing kappa into something
#' actionable: usually a handful of category pairs account for most of it,
#' and those pairs are the ones whose definitions need work.
#'
#' @inheritParams qda_percent_agreement
#' @param only_disagreements Drop the diagonal.
#' @return A data frame with the two coders' categories and the count, most
#'   frequent first.
#' @examples
#' u <- cbind(ann = c("A", "B", "A", "B"), bob = c("A", "B", "B", "B"))
#' qda_confusion(u)
#' @export
qda_confusion <- function(units, only_disagreements = FALSE) {
  if (ncol(units) != 2L) {
    stop("the confusion table is for two coders", call. = FALSE)
  }
  keep <- !is.na(units[, 1]) & !is.na(units[, 2])
  tab <- table(a = units[keep, 1], b = units[keep, 2])
  d <- as.data.frame(tab, stringsAsFactors = FALSE)
  names(d)[names(d) == "Freq"] <- "n"
  names(d)[1:2] <- colnames(units)
  d <- d[d$n > 0, , drop = FALSE]
  if (only_disagreements) d <- d[d[[1]] != d[[2]], , drop = FALSE]
  d <- d[order(-d$n), , drop = FALSE]
  rownames(d) <- NULL
  d
}
