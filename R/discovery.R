# Lowe et al.'s theme-accumulation models. T_n is the expected number of
# distinct themes found after n observations; A is the number that exists to
# be found, b the average share of them present in one observation.
lowe_curve <- function(model, n, A, b, d = 0) {
  n <- as.numeric(n)
  switch(model,
    # independent samples: observations share nothing systematically (a6)
    IS = A * (1 - (1 - b)^n),
    # information weighted: overlap grows with what is already known (a10)
    IW = A * b * n / (1 + b * (n - 1)),
    # sample size weighted: overlap grows with the number of observations
    # (a12). The gamma ratio overflows past n of about 170, so it is taken
    # in logs -- the quantity itself stays small, only the pieces are huge.
    SW = A * (1 - exp(lgamma(1 - b + n) - lgamma(1 - b) - lgamma(1 + n))),
    stop("unknown model: ", model, call. = FALSE)
  )
}

#' How saturated is this material, and how much more would it take?
#'
#' A saturation curve that is still climbing tells you nothing about how far
#' from the top it is.  Lowe, Norris, Farris and Babbage (2018)
#' \doi{10.1177/1525822X17749386} fit the accumulation of themes to a growth
#' model, which estimates the number of themes that exist to be found (`A`)
#' and thereby turns "still climbing" into a percentage.
#'
#' Their saturation index is the share of the estimable themes you already
#' have, `100 * T_N / floor(A)`.  Because it comes from a fitted `A`, it also
#' answers the question a project actually asks halfway through: how many more
#' documents for another ten points.
#'
#' @section Which model:
#' `IS` assumes observations are independent, `IW` that overlap grows with
#' what is already known, `SW` that it grows with the number of observations.
#' They differ mainly in how fast the curve flattens, and Lowe et al. found no
#' single winner -- fit all three and look at which describes your data,
#' rather than picking one in advance.
#'
#' @param cumulative Distinct themes after each document, in coding order --
#'   the `cumulative` column of [qda_new_codes()], or a plain vector.
#' @param model `"IS"`, `"IW"` or `"SW"`.
#' @return A list with the fitted `A` and `b`, the `index` (per cent), the
#'   `fitted` curve, the residual `rmse`, and `model`.
#' @examples
#' # a curve that is clearly flattening
#' qda_saturation_index(c(8, 13, 16, 18, 19, 20, 20, 21))$index
#' @export
qda_saturation_index <- function(cumulative, model = c("IW", "IS", "SW")) {
  model <- match.arg(model)
  y <- if (is.data.frame(cumulative)) cumulative$cumulative else cumulative
  y <- as.numeric(y)
  n <- seq_along(y)
  if (length(y) < 3 || max(y) <= 0) {
    return(list(model = model, A = NA_real_, b = NA_real_, index = NA_real_,
                fitted = rep(NA_real_, length(y)), rmse = NA_real_,
                reason = "fewer than three documents, or no themes"))
  }

  # Lowe et al.'s direct estimate for IW (a16) is exact algebra from the
  # first and last point, and makes a good starting value for the others
  N <- length(y)
  T1 <- y[1]
  TN <- y[N]
  denom <- N * T1 - TN
  A0 <- if (denom != 0) (N - 1) * T1 * TN / denom else TN * 2
  b0 <- if (A0 > 0) min(0.99, max(0.01, T1 / A0)) else 0.5
  if (!is.finite(A0) || A0 < TN) A0 <- TN * 1.2

  loss <- function(par) {
    A <- par[1]; b <- par[2]
    if (!is.finite(A) || !is.finite(b) || A <= 0 || b <= 0 || b >= 1) return(1e12)
    pred <- suppressWarnings(lowe_curve(model, n, A, b))
    if (any(!is.finite(pred))) return(1e12)
    sum((pred - y)^2)
  }
  fit <- stats::optim(c(A0, b0), loss, method = "Nelder-Mead",
                      control = list(maxit = 2000, reltol = 1e-12))
  A <- fit$par[1]; b <- fit$par[2]
  pred <- lowe_curve(model, n, A, b)
  list(model = model, A = A, b = b,
       # the index uses floor(A), as the paper specifies: a fitted A is not
       # an integer, and the number of themes that exist certainly is
       index = 100 * TN / max(1, floor(A)),
       fitted = pred,
       rmse = sqrt(mean((pred - y)^2)),
       reason = "")
}

#' How many more documents for a given saturation?
#'
#' The practical follow-up to [qda_saturation_index()]: the fitted curve is
#' solved for the number of documents at which the index would reach `target`.
#'
#' @param fit A fit from [qda_saturation_index()].
#' @param target Desired saturation, in per cent.
#' @param max_n Largest number of documents to consider.
#' @return The number of documents, or `NA` when the model does not reach the
#'   target within `max_n` -- which is itself worth reporting.
#' @examples
#' fit <- qda_saturation_index(c(8, 13, 16, 18, 19, 20, 20, 21))
#' qda_documents_for(fit, 95)
#' @export
qda_documents_for <- function(fit, target = 95, max_n = 1000) {
  if (!is.list(fit) || !is.finite(fit$A) || !is.finite(fit$b)) return(NA_integer_)
  want <- target / 100 * floor(fit$A)
  for (n in seq_len(max_n)) {
    value <- lowe_curve(fit$model, n, fit$A, fit$b)
    if (is.finite(value) && value >= want) return(n)
  }
  NA_integer_
}
