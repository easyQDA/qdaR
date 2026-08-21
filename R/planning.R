# Donner & Rotondi's common-correlation model for a binary outcome and n
# raters. The reparameterisation (their equations 2-3, following Altaye et
# al.) collapses to three probabilities: all raters say no, all say yes, and
# anything in between.
kappa_cell_probs <- function(kappa, prevalence, raters) {
  p <- prevalence
  n <- raters
  p_none <- (1 - p)^n * (1 - kappa) + kappa * (1 - p)
  p_all <- p^n * (1 - kappa) + kappa * p
  c(none = p_none, mixed = 1 - p_none - p_all, all = p_all)
}

# The Pearson statistic of their equation (4) with all disagreeing cells
# pooled, per subject. Pooling leaves one degree of freedom, and because the
# expected counts are N times these probabilities, the statistic is linear in
# N -- so the sample size follows in closed form instead of by iteration.
kappa_chisq_per_subject <- function(kappa0, kappa_lower, prevalence, raters) {
  observed <- kappa_cell_probs(kappa0, prevalence, raters)
  under_null <- kappa_cell_probs(kappa_lower, prevalence, raters)
  keep <- under_null > 0
  sum((observed[keep] - under_null[keep])^2 / under_null[keep])
}

#' How many segments must be double-coded?
#'
#' Interobserver studies are routinely run at whatever size was convenient and
#' then reported with a confidence interval far too wide to support the claim
#' made from it.  Donner and Rotondi (2010) \doi{10.2202/1557-4679.1275} give
#' the sample size that makes the *lower* bound of a one-sided interval for
#' kappa reach a value you name in advance -- which is the quantity a reader
#' actually cares about, since nobody argues that agreement was too good.
#'
#' You supply three numbers: the kappa you expect (`kappa0`, from a pilot or
#' the literature), the smallest kappa you would still be willing to defend
#' (`kappa_lower`), and the prevalence of the code (`prevalence`).  Prevalence
#' matters more than people expect: a code applied to a tenth of the segments
#' needs several times the material of one applied to a third.
#'
#' @param kappa0 The kappa you anticipate.
#' @param kappa_lower The minimum you want the interval's lower bound to reach.
#' @param prevalence Share of segments carrying the code, between 0 and 1.
#'   The requirement is symmetric about 0.5, so a conservative planner takes
#'   the value further from it.
#' @param raters Number of coders; two or more.
#' @param alpha One minus the confidence level of the one-sided interval.
#' @param critical The chi-squared critical value.  The default is the exact
#'   quantile.  The published tables were computed with it rounded to 2.71,
#'   which makes eight of their forty-eight cells one larger; pass
#'   `critical = 2.71` to reproduce them cell for cell.
#'
#' @return The number of segments, rounded up.  `Inf` when `kappa_lower` is
#'   not below `kappa0` -- no sample size makes an interval reach a bound at
#'   or above the point estimate it is centred on.
#' @examples
#' # Donner and Rotondi's own Table 2: kappa0 = 0.8, lower bound 0.6,
#' # prevalence 0.1, two raters
#' qda_plan_kappa(0.8, 0.6, 0.1, raters = 2)     # 116
#' qda_plan_kappa(0.8, 0.6, 0.1, raters = 4)     # 62 -- more coders, less material
#' @export
qda_plan_kappa <- function(kappa0, kappa_lower, prevalence, raters = 2,
                           alpha = 0.05, critical = NULL) {
  stopifnot(prevalence > 0, prevalence < 1, raters >= 2,
            kappa0 > -1, kappa0 < 1, kappa_lower > -1, kappa_lower < 1)
  if (kappa_lower >= kappa0) return(Inf)
  crit <- if (is.null(critical)) stats::qchisq(1 - 2 * alpha, df = 1) else critical
  per <- kappa_chisq_per_subject(kappa0, kappa_lower, prevalence, raters)
  if (!is.finite(per) || per <= 0) return(Inf)
  ceiling(crit / per)
}

#' What lower bound can this much material reach?
#'
#' The question the other way round, which is the one you face when the number
#' of segments is already fixed by the budget: given `n` segments, how far
#' down does the one-sided interval for kappa reach?  Donner and Rotondi's
#' Table 3 answers it; this reproduces the calculation.
#'
#' @inheritParams qda_plan_kappa
#' @param n Number of segments available.
#' @return The expected lower bound, or `NA` when even a kappa of nought
#'   cannot be excluded with this much material.
#' @examples
#' qda_kappa_lower(100, kappa0 = 0.7, prevalence = 0.3, raters = 4)
#' @export
qda_kappa_lower <- function(n, kappa0, prevalence, raters = 2, alpha = 0.05,
                            critical = NULL) {
  stopifnot(n > 0, prevalence > 0, prevalence < 1, raters >= 2)
  crit <- if (is.null(critical)) stats::qchisq(1 - 2 * alpha, df = 1) else critical
  target <- function(kl) n * kappa_chisq_per_subject(kappa0, kl, prevalence,
                                                     raters) - crit
  lo <- -0.999
  if (target(lo) < 0) return(NA_real_)
  hi <- kappa0 - 1e-9
  if (target(hi) > 0) return(kappa0)
  stats::uniroot(target, c(lo, hi), tol = 1e-9)$root
}

#' How many documents to be reasonably sure of meeting a theme?
#'
#' Fugard and Potts (2015) \doi{10.1080/13645579.2015.1005453} ask the
#' planning question thematic analysis usually answers with a rule of thumb:
#' if a theme is present in a known share of the population, how many
#' interviews does it take to be, say, 80 percent sure of meeting it at least
#' `instances` times?  The waiting time is negative binomial, which is the
#' same as requiring the binomial tail `P(X >= instances)` to reach the
#' desired power.
#'
#' @section What it assumes, and who disputes it:
#' Themes are treated as present or absent, independent of one another, and
#' certain to surface once present.  Braun and Clarke (2016)
#' \doi{10.1080/13645579.2016.1195588} reject the premise for reflexive
#' thematic analysis, where themes are constructed rather than discovered and
#' a population prevalence is not a meaningful quantity.  The number is a
#' planning aid for work that accepts those assumptions, not a sample size
#' requirement for qualitative research at large.
#'
#' @param prevalence Share of the population in which the theme is present.
#' @param instances How many separate occurrences you want to see.
#' @param power Desired probability of seeing them.
#' @param max_n Upper bound for the search.
#' @return The number of documents, or `NA` when `max_n` is not enough.
#' @examples
#' # Fugard and Potts' Table 1: a theme in 5 % of the population, one
#' # instance wanted, 80 % power
#' qda_plan_themes(0.05, instances = 1)     # 32
#' qda_plan_themes(0.10, instances = 2)     # 29
#' @export
qda_plan_themes <- function(prevalence, instances = 1, power = 0.8,
                            max_n = 10000) {
  stopifnot(prevalence > 0, prevalence <= 1, instances >= 1,
            power > 0, power < 1)
  for (n in seq(instances, max_n)) {
    if (stats::pbinom(instances - 1, n, prevalence, lower.tail = FALSE) >= power) {
      return(n)
    }
  }
  NA_integer_
}

#' The same question as power
#'
#' Given the documents you have, how likely are you to meet a theme of this
#' prevalence the desired number of times?
#'
#' @inheritParams qda_plan_themes
#' @param n Number of documents.
#' @return A probability.
#' @examples
#' qda_theme_power(32, 0.05)          # about 0.8, the flip side of the table
#' @export
qda_theme_power <- function(n, prevalence, instances = 1) {
  stopifnot(n >= 0, prevalence >= 0, prevalence <= 1, instances >= 1)
  stats::pbinom(instances - 1, n, prevalence, lower.tail = FALSE)
}
