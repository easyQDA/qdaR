#' Association between codes and a grouping variable
#'
#' The plugins report descriptive agreement and co-occurrence but no
#' inferential tests -- deliberately, because a test invites a claim the
#' design often does not support.  Where the design *does* support it, this
#' function performs the usual chi-squared test of independence, reports
#' Cramer's V as an effect size, and says whether the approximation was
#' appropriate at all.  When expected counts fall below five it reports an
#' exact test instead: Fisher's for a two-by-two table, and a Monte Carlo
#' p-value with the margins fixed for anything larger.
#'
#' Note the unit of this test: one coded fragment.  Fragments from the same
#' document are not independent observations, so a significant result across
#' documents is weaker evidence than the p-value suggests.
#'
#' @param fragments A fragments data frame.
#' @param group A column of `fragments` to test the codes against, e.g.
#'   `"citekey"`, or a vector of the same length.
#' @param codes Restrict to these codes; `NULL` uses all.
#' @param resamples Number of Monte Carlo replicates for the simulated
#'   p-value.
#' @param seed Seed for those replicates, so a reported p-value can be
#'   reproduced exactly.
#'
#' @return A list with the contingency `table`, the `test` object, the
#'   chi-squared `statistic` and `expected` counts it was computed from, the
#'   effect size `cramers_v`, and `expected_ok` telling you whether the
#'   chi-squared approximation was appropriate.  The statistic is reported
#'   even when the exact test is used, because Cramer's V is derived from it
#'   and an effect size nobody can recompute is not worth reporting.
#' @examples
#' frag <- qda_read_fragments(qda_example("easyqda-fragments.csv"))
#' res <- qda_chisq(frag, group = "citekey")
#' res$cramers_v
#' @export
qda_chisq <- function(fragments, group = "citekey", codes = NULL,
                      resamples = 2000, seed = 42) {
  stopifnot(is.data.frame(fragments))
  g <- if (length(group) == 1L && group %in% names(fragments)) {
    fragments[[group]]
  } else {
    group
  }
  stopifnot(length(g) == nrow(fragments))
  keep <- nzchar(fragments$code)
  if (!is.null(codes)) keep <- keep & fragments$code %in% codes
  tab <- table(code = fragments$code[keep], group = g[keep])
  if (any(dim(tab) < 2L)) {
    stop("need at least two codes and two groups to test independence",
         call. = FALSE)
  }
  suppressWarnings(chi <- stats::chisq.test(tab))
  expected_ok <- all(chi$expected >= 5)
  test <- if (expected_ok) {
    chi
  } else if (all(dim(tab) == 2L)) {
    stats::fisher.test(tab)
  } else {
    # the exact test for an r-by-c table is only feasible for small counts:
    # fisher.test() aborts with a workspace error on tables that occur
    # routinely here (six codes over five documents is enough), and the table
    # dimensions do not predict it -- the counts do.  So anything beyond
    # two-by-two goes to the simulated p-value, seeded to stay reproducible.
    if (!is.null(seed)) {
      old <- if (exists(".Random.seed", .GlobalEnv)) {
        get(".Random.seed", .GlobalEnv)
      } else NULL
      set.seed(seed)
      on.exit({
        if (is.null(old)) {
          suppressWarnings(rm(".Random.seed", envir = .GlobalEnv))
        } else {
          assign(".Random.seed", old, envir = .GlobalEnv)
        }
      }, add = TRUE)
    }
    stats::fisher.test(tab, simulate.p.value = TRUE, B = resamples)
  }
  n <- sum(tab)
  v <- sqrt(as.numeric(chi$statistic) / (n * (min(dim(tab)) - 1)))
  if (!expected_ok) {
    warning("expected counts below 5 -- reporting ", test$method,
            "; Cramer's V is still computed from the chi-squared statistic",
            call. = FALSE)
  }
  list(table = tab, test = test, statistic = as.numeric(chi$statistic),
       expected = chi$expected, cramers_v = v, expected_ok = expected_ok,
       n = n)
}

#' Jaccard distances between codes
#'
#' Two codes are close when they are assigned to the same segments.  This is
#' the distance the multidimensional scaling and the clustering below work
#' on, and the same coefficient the plugin uses to propose code matches.
#'
#' @inheritParams qda_chisq
#' @param unit Column identifying the segment; defaults to `"annotationKey"`.
#' @param min_n Ignore codes used fewer than `min_n` times.  Rarely used
#'   codes make every coefficient unstable.
#' @return An object of class `dist`.
#' @examples
#' frag <- qda_read_fragments(qda_example("easyqda-fragments.csv"))
#' qda_code_distance(frag, min_n = 1)
#' @export
qda_code_distance <- function(fragments, unit = "annotationKey", min_n = 3) {
  stopifnot(is.data.frame(fragments), unit %in% names(fragments))
  keep <- nzchar(fragments$code)
  tab <- table(fragments[[unit]][keep], fragments$code[keep])
  tab <- tab > 0
  n <- colSums(tab)
  tab <- tab[, n >= min_n, drop = FALSE]
  if (ncol(tab) < 2L) {
    stop("fewer than two codes reach min_n = ", min_n, call. = FALSE)
  }
  codes <- colnames(tab)
  m <- matrix(0, length(codes), length(codes),
              dimnames = list(codes, codes))
  for (i in seq_along(codes)) {
    for (j in seq_along(codes)) {
      a <- tab[, i]; b <- tab[, j]
      inter <- sum(a & b); union <- sum(a | b)
      m[i, j] <- if (union == 0) 1 else 1 - inter / union
    }
  }
  stats::as.dist(m)
}

#' Multidimensional scaling of codes
#'
#' Places codes in two dimensions so that codes applied to the same segments
#' end up close together.  A map of this kind says nothing about
#' significance; it is a way of looking at a distance matrix.
#'
#' @inheritParams qda_code_distance
#' @return A list with the `points` data frame and the `goodness` of fit
#'   reported by [stats::cmdscale()].
#' @examples
#' frag <- qda_read_fragments(qda_example("easyqda-fragments.csv"))
#' qda_mds(frag, min_n = 1)$points
#' @export
qda_mds <- function(fragments, unit = "annotationKey", min_n = 3) {
  d <- qda_code_distance(fragments, unit = unit, min_n = min_n)
  k <- min(2L, attr(d, "Size") - 1L)
  fit <- stats::cmdscale(d, k = k, eig = TRUE)
  pts <- as.data.frame(fit$points)
  names(pts) <- paste0("dim", seq_len(ncol(pts)))
  pts$code <- rownames(pts)
  rownames(pts) <- NULL
  list(points = pts, goodness = fit$GOF)
}

#' Plot the code map
#'
#' @inheritParams qda_mds
#' @return A 'ggplot2' object.
#' @examples
#' frag <- qda_read_fragments(qda_example("easyqda-fragments.csv"))
#' qda_plot_mds(frag, min_n = 1)
#' @export
qda_plot_mds <- function(fragments, unit = "annotationKey", min_n = 3) {
  pts <- qda_mds(fragments, unit = unit, min_n = min_n)$points
  if (!"dim2" %in% names(pts)) pts$dim2 <- 0
  ggplot2::ggplot(pts, ggplot2::aes(x = pts$dim1, y = pts$dim2)) +
    ggplot2::geom_point() +
    ggplot2::geom_text(ggplot2::aes(label = pts$code), vjust = -0.7, size = 3) +
    ggplot2::labs(x = "dimension 1", y = "dimension 2") +
    ggplot2::theme_minimal()
}

#' Hierarchical clustering of codes
#'
#' Clusters codes by the segments they share.  The cophenetic correlation is
#' reported alongside, because a dendrogram always looks convincing even when
#' it represents the distances poorly -- values well below about 0.7 mean the
#' picture should not be over-read.
#'
#' @inheritParams qda_code_distance
#' @param method Linkage passed to [stats::hclust()].
#' @return A list with the `hclust` object, the `distance` and the
#'   `cophenetic` correlation.  With fewer than three codes the correlation
#'   is undefined and reported as `NA` rather than as a number that means
#'   nothing.
#' @examples
#' frag <- qda_read_fragments(qda_example("easyqda-fragments.csv"))
#' qda_cluster(frag, min_n = 1)$cophenetic
#' @export
qda_cluster <- function(fragments, unit = "annotationKey", min_n = 3,
                        method = "average") {
  d <- qda_code_distance(fragments, unit = unit, min_n = min_n)
  h <- stats::hclust(d, method = method)
  # with two objects there is a single distance, so the correlation has no
  # variance to work with -- NA is the honest answer
  coph <- if (attr(d, "Size") < 3L) NA_real_ else {
    as.numeric(stats::cor(d, stats::cophenetic(h)))
  }
  list(hclust = h, distance = d, cophenetic = coph)
}

#' Correspondence analysis of the code by document table
#'
#' Shows which codes and which documents attract each other.  Unlike the
#' plugin's descriptive matrix, this decomposes the table and reports how
#' much of its inertia the first dimensions explain -- the honest answer to
#' "how much of the picture am I actually seeing".
#'
#' @inheritParams qda_plot_code_matrix
#' @param n_dims Number of dimensions to keep.
#' @return A list with the `correspondence` object from [MASS::corresp()],
#'   the row and column `scores`, the `inertia` of the kept dimensions, the
#'   `total_inertia` of the whole table and `inertia_share`, the fraction of
#'   that total each kept dimension carries.  The share is deliberately
#'   relative to the *total*: shares that are normalised to the dimensions you
#'   happened to keep always add up to 100 percent and so answer a question
#'   nobody asked.
#' @examples
#' frag <- qda_read_fragments(qda_example("easyqda-fragments.csv"))
#' if (nrow(unique(frag["code"])) > 1) {
#'   ca <- try(qda_ca(frag), silent = TRUE)
#' }
#' @export
qda_ca <- function(fragments, doc_col = "citekey", n_dims = 2) {
  tab <- qda_code_matrix(fragments, doc_col = doc_col, long = FALSE)
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  if (any(dim(tab) < 2L)) {
    stop("correspondence analysis needs at least a 2x2 table", call. = FALSE)
  }
  full <- min(dim(tab)) - 1L
  nf <- min(as.integer(n_dims), full)
  ca <- MASS::corresp(tab, nf = nf)
  all_dims <- MASS::corresp(tab, nf = full)
  total <- sum(all_dims$cor^2)
  inertia <- ca$cor^2
  list(correspondence = ca,
       row_scores = as.data.frame(ca$rscore),
       col_scores = as.data.frame(ca$cscore),
       inertia = inertia,
       total_inertia = total,
       inertia_share = inertia / total)
}
