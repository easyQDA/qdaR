#' Bootstrap confidence interval for an agreement coefficient
#'
#' A coefficient without an interval invites over-reading, which is the
#' complaint Sim and Wright (2005) \doi{10.1093/ptj/85.3.257} and Zapf et al.
#' (2016) \doi{10.1186/s12874-016-0200-9} both make.  Qualitative studies
#' work with few units, so the interval is usually wide -- and that is the
#' point.
#'
#' Units are resampled, not ratings: the unit of analysis is the segment, and
#' resampling ratings would treat two judgements of one segment as
#' independent observations.
#'
#' @param units A unit-by-coder matrix from [qda_units()].
#' @param fn The coefficient to bootstrap, e.g. [qda_fleiss()].
#' @param resamples Number of bootstrap samples.
#' @param seed Seed, so a published interval can be reproduced.  The plugin
#'   and the Python twin use the same generator and the same default, so all
#'   three report the same interval for the same data.
#' @param level Confidence level.
#'
#' @return A list with `estimate`, `lo`, `hi` and `used` (how many resamples
#'   produced a finite value), or `NULL` when fewer than 20 did -- a wide
#'   interval is informative, an interval computed from nothing is not.
#' @examples
#' u <- cbind(ann = rep(c("A", "B"), 20), bob = rep(c("A", "B", "B", "A"), 10))
#' qda_bootstrap_ci(u, qda_kappa, resamples = 200)
#' @export
qda_bootstrap_ci <- function(units, fn = qda_fleiss, resamples = 1000,
                             seed = 42, level = 0.95) {
  stopifnot(is.function(fn))
  n <- nrow(units)
  if (!n) return(NULL)
  rng <- mulberry32(seed)
  values <- numeric(0)
  for (b in seq_len(resamples)) {
    idx <- floor(rng(n) * n) + 1L
    idx[idx > n] <- n
    sample_units <- units[idx, , drop = FALSE]
    attr(sample_units, "multi") <- attr(units, "multi")
    v <- suppressWarnings(tryCatch(fn(sample_units), error = function(e) NA_real_))
    if (is.finite(v)) values <- c(values, v)
  }
  if (length(values) < 20) return(NULL)
  values <- sort(values)
  alpha <- 1 - level
  at <- function(p) {
    values[min(length(values), max(1L, floor(p * length(values)) + 1L))]
  }
  list(estimate = suppressWarnings(fn(units)),
       lo = at(alpha / 2), hi = at(1 - alpha / 2),
       used = length(values), resamples = resamples, seed = seed)
}

# mulberry32, the generator the plugin uses. Reimplemented rather than using
# R's own RNG so that an interval reported by qdaR, qdaPy and the plugin is
# the SAME interval; three different generators would give three different
# intervals for the same data, and nobody could tell which to believe.
#
# The arithmetic has to be done in 32-bit unsigned words. R has no such type
# and doubles lose precision past 2^53, so the multiplication is split into
# 16-bit halves -- the standard trick for reproducing JavaScript's Math.imul.
u32 <- function(x) x %% 4294967296

imul32 <- function(a, b) {
  a <- u32(a); b <- u32(b)
  a_hi <- a %/% 65536; a_lo <- a %% 65536
  b_hi <- b %/% 65536; b_lo <- b %% 65536
  # the hi*hi term overflows past bit 32 and is discarded, as in Math.imul
  u32(a_lo * b_lo + u32((a_hi * b_lo + a_lo * b_hi) * 65536))
}

# bitwOr and bitwXor take signed 32-bit integers, so anything above 2^31
# has to be split as well.
or32 <- function(a, b) {
  a <- u32(a); b <- u32(b)
  bitwOr(a %/% 65536, b %/% 65536) * 65536 + bitwOr(a %% 65536, b %% 65536)
}

xor32 <- function(a, b) {
  # bitwXor works on signed 32-bit integers; fold the sign bit by hand
  a <- u32(a); b <- u32(b)
  bitwXor(a %/% 65536, b %/% 65536) * 65536 + bitwXor(a %% 65536, b %% 65536)
}

mulberry32 <- function(seed) {
  state <- u32(seed)
  function(k = 1L) {
    out <- numeric(k)
    for (i in seq_len(k)) {
      state <<- u32(state + 0x6D2B79F5)
      t <- state
      t <- imul32(xor32(t, t %/% 32768), or32(1, t))
      t <- xor32(u32(t + imul32(xor32(t, t %/% 128), or32(61, t))), t)
      out[i] <- xor32(t, t %/% 16384) / 4294967296
    }
    out
  }
}

#' Agreement per code
#'
#' A single pooled coefficient hides which codes the coders actually argued
#' about.  This asks the question once per code, as a yes/no judgement, which
#' is also the only honest way to treat material where segments legitimately
#' carry several codes.
#'
#' @inheritParams qda_units
#' @param min_n Skip codes used fewer than this many times; with two or three
#'   uses every coefficient is noise.
#' @return A data frame with one row per code: `n` uses, `units` compared,
#'   `percent`, `cohen`, `ac1`, and `prevalence` with its Wilson interval.
#' @examples
#' frag <- data.frame(
#'   annotationKey = rep(paste0("s", 1:6), each = 2),
#'   codedBy = rep(c("ann", "bob"), 6),
#'   code = c("A", "A", "A", "B", "B", "B", "A", "A", "B", "B", "A", "A")
#' )
#' qda_agreement_by_code(frag, min_n = 1)
#' @export
qda_agreement_by_code <- function(fragments, min_n = 3,
                                  unit = "annotationKey",
                                  coder = "codedBy", value = "code") {
  stopifnot(is.data.frame(fragments))
  codes <- table(fragments[[value]][nzchar(fragments[[value]])])
  codes <- names(codes)[codes >= min_n]
  if (!length(codes)) {
    return(data.frame(code = character(), n = integer(), units = integer(),
                      percent = numeric(), cohen = numeric(), ac1 = numeric(),
                      prevalence = numeric(), lo = numeric(), hi = numeric(),
                      stringsAsFactors = FALSE))
  }
  rows <- lapply(codes, function(code) {
    b <- qda_units_binary(fragments, code, unit = unit, coder = coder,
                          value = value)
    yes <- sum(b == "yes", na.rm = TRUE)
    ci <- qda_wilson(yes, length(b))
    data.frame(
      code = code,
      n = sum(fragments[[value]] == code),
      units = nrow(b),
      percent = qda_percent_agreement(b),
      cohen = if (ncol(b) == 2L) qda_kappa(b) else NA_real_,
      ac1 = qda_ac1(b),
      prevalence = ci$estimate, lo = ci$lo, hi = ci$hi,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[order(-out$n), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Why a kappa is disappointing
#'
#' Feinstein and Cicchetti (1990) \doi{10.1016/0895-4356(90)90158-L} named the
#' two reasons a kappa can collapse while observed agreement is high: a skewed
#' marginal distribution, and a systematic difference between the coders.
#' These indices measure exactly those two, and PABAK is kappa recomputed with
#' chance fixed at one half, which removes the prevalence effect.
#'
#' Reporting kappa alone tells a reader that agreement is poor.  These three
#' numbers tell them why, which is the difference between a result and
#' something they can act on.
#'
#' @param units A unit-by-coder matrix with exactly two coders and two
#'   categories; anything else returns `NULL` rather than a number that does
#'   not mean what it appears to.
#' @return A list with `prevalence_index`, `bias_index`, `pabak`, `percent`,
#'   the two `categories`, `n` and the two-by-two `table`; or `NULL`.
#' @examples
#' u <- cbind(ann = c("A", "A", "A", "B"), bob = c("A", "A", "A", "A"))
#' qda_kappa(u)             # 0, which looks like failure
#' qda_paradox(u)$pabak     # 0.5, and the prevalence index says why
#' @export
qda_paradox <- function(units) {
  keep <- !is.na(units[, 1]) & !is.na(units[, 2])
  if (ncol(units) != 2L || !any(keep)) return(NULL)
  a_col <- as.character(units[keep, 1])
  b_col <- as.character(units[keep, 2])
  cats <- sort(unique(c(a_col, b_col)))
  if (length(cats) != 2L) return(NULL)
  n <- length(a_col)
  a <- sum(a_col == cats[1] & b_col == cats[1])
  b <- sum(a_col == cats[1] & b_col == cats[2])
  cc <- sum(a_col == cats[2] & b_col == cats[1])
  d <- sum(a_col == cats[2] & b_col == cats[2])
  po <- (a + d) / n
  list(categories = cats, n = n,
       prevalence_index = abs(a - d) / n,
       bias_index = abs(b - cc) / n,
       pabak = 2 * po - 1,
       percent = po,
       table = c(a = a, b = b, c = cc, d = d))
}

#' A proportion with an interval that behaves at the edges
#'
#' Wilson (1927) \doi{10.1080/01621459.1927.10502953} rather than the textbook
#' normal approximation, which Brown, Cai and DasGupta (2001)
#' \doi{10.1214/ss/1009213286} show to be erratic for small samples and
#' degenerate at nought or one.  Code prevalences live exactly there: a code
#' used in two of forty segments must not get an interval reaching below zero.
#'
#' @param successes Count.
#' @param total Sample size.
#' @param level Confidence level.
#' @return A list with `estimate`, `lo`, `hi` and `n`.
#' @examples
#' qda_wilson(2, 40)
#' qda_wilson(0, 10)   # upper bound only, and it stays inside [0, 1]
#' @export
qda_wilson <- function(successes, total, level = 0.95) {
  k <- as.numeric(successes)
  n <- as.numeric(total)
  if (!is.finite(k) || !is.finite(n) || n <= 0 || k < 0 || k > n) {
    return(list(estimate = NA_real_, lo = NA_real_, hi = NA_real_,
                n = if (is.finite(n)) n else 0))
  }
  z <- stats::qnorm(1 - (1 - level) / 2)
  p <- k / n
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half <- (z / denom) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  # the Wilson interval always contains the point estimate; at p = 0 or 1
  # floating point can put a bound a machine epsilon on the wrong side, so
  # the containment is enforced rather than left to luck
  list(estimate = p,
       lo = min(p, max(0, centre - half)),
       hi = max(p, min(1, centre + half)),
       n = n)
}
