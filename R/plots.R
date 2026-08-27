#' Code frequencies
#'
#' The plugin's overview chart, drawn with 'ggplot2': how often each code was
#' assigned.
#'
#' @param fragments A fragments data frame from [qda_read_fragments()].
#' @param top Show only the `top` most frequent codes; `NULL` shows all.
#' @param fill Bar colour.  When the export carries a `color` column and
#'   `fill` is `NULL`, the code colours from the code system are used.
#' @return A 'ggplot2' object.
#' @examples
#' frag <- qda_read_fragments(qda_example("easyqda-fragments.csv"))
#' qda_plot_frequencies(frag)
#' @export
qda_plot_frequencies <- function(fragments, top = 25, fill = "#4c78a8") {
  d <- qda_code_counts(fragments, top = top)
  ggplot2::ggplot(d, ggplot2::aes(x = stats::reorder(d$code, d$n), y = d$n)) +
    ggplot2::geom_col(fill = fill) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "codings") +
    ggplot2::theme_minimal()
}

#' Counts per code
#'
#' @inheritParams qda_plot_frequencies
#' @return A data frame with `code` and `n`, most frequent first.
#' @examples
#' qda_code_counts(qda_read_fragments(qda_example("easyqda-fragments.csv")))
#' @export
qda_code_counts <- function(fragments, top = NULL) {
  stopifnot(is.data.frame(fragments), "code" %in% names(fragments))
  x <- fragments$code
  x <- x[nzchar(x)]
  tab <- sort(table(x), decreasing = TRUE)
  d <- data.frame(code = names(tab), n = as.integer(tab),
                  stringsAsFactors = FALSE)
  if (!is.null(top) && nrow(d) > top) d <- d[seq_len(top), , drop = FALSE]
  d
}

#' Code by document matrix
#'
#' The plugin's code-by-document heat map.  Documents are identified by
#' `citekey` when present, otherwise by title.
#'
#' @inheritParams qda_plot_frequencies
#' @param doc_col Column identifying the document.
#' @return A 'ggplot2' object.
#' @examples
#' frag <- qda_read_fragments(qda_example("easyqda-fragments.csv"))
#' qda_plot_code_matrix(frag)
#' @export
qda_plot_code_matrix <- function(fragments, doc_col = "citekey") {
  d <- qda_code_matrix(fragments, doc_col = doc_col, long = TRUE)
  ggplot2::ggplot(d, ggplot2::aes(x = d$document, y = d$code, fill = d$n)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = "#f0f4f8", high = "#2b5d8a") +
    ggplot2::labs(x = NULL, y = NULL, fill = "codings") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Code by document counts
#'
#' @inheritParams qda_plot_code_matrix
#' @param long Return a long data frame (`TRUE`) or a matrix (`FALSE`).
#' @return A data frame or a matrix of counts.
#' @examples
#' frag <- qda_read_fragments(qda_example("easyqda-fragments.csv"))
#' qda_code_matrix(frag, long = FALSE)
#' @export
qda_code_matrix <- function(fragments, doc_col = "citekey", long = TRUE) {
  stopifnot(is.data.frame(fragments))
  if (!doc_col %in% names(fragments)) doc_col <- "title"
  keep <- nzchar(fragments$code)
  tab <- table(code = fragments$code[keep], document = fragments[[doc_col]][keep])
  if (!long) return(unclass(tab))
  d <- as.data.frame(tab, stringsAsFactors = FALSE)
  names(d)[names(d) == "Freq"] <- "n"
  d
}

#' Coding progress over time
#'
#' The plugin's process view: how the number of codings grew, per coder.
#'
#' @param history A history data frame from [qda_read_history()].
#' @return A 'ggplot2' object.
#' @examples
#' h <- qda_read_history(qda_example("easyqda-history.csv"))
#' qda_plot_timeline(h)
#' @export
qda_plot_timeline <- function(history) {
  d <- qda_timeline(history)
  ggplot2::ggplot(d, ggplot2::aes(x = d$time, y = d$cumulative,
                                  colour = d$user, group = d$user)) +
    ggplot2::geom_step() +
    ggplot2::labs(x = NULL, y = "codings (cumulative)", colour = "coder") +
    ggplot2::theme_minimal()
}

#' Cumulative codings per coder
#'
#' @inheritParams qda_plot_timeline
#' @return A data frame with `time`, `user` and `cumulative`.
#' @examples
#' qda_timeline(qda_read_history(qda_example("easyqda-history.csv")))
#' @export
qda_timeline <- function(history) {
  stopifnot(is.data.frame(history), all(c("ts", "user", "action") %in% names(history)))
  d <- history[history$action == "add", , drop = FALSE]
  if (!nrow(d)) {
    return(data.frame(time = as.POSIXct(character()), user = character(),
                      cumulative = integer(), stringsAsFactors = FALSE))
  }
  d$time <- as.POSIXct(d$ts, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  d <- d[order(d$time), , drop = FALSE]
  out <- do.call(rbind, lapply(split(d, d$user), function(g) {
    data.frame(time = g$time, user = g$user, cumulative = seq_len(nrow(g)),
               stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}

#' Saturation curve
#'
#' How many *new* codes each successive coding introduced -- the curve
#' flattens when a code system stops growing.
#'
#' @inheritParams qda_plot_timeline
#' @return A 'ggplot2' object.
#' @examples
#' qda_plot_saturation(qda_read_history(qda_example("easyqda-history.csv")))
#' @export
qda_plot_saturation <- function(history) {
  d <- qda_saturation(history)
  ggplot2::ggplot(d, ggplot2::aes(x = d$step, y = d$codes)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "codings", y = "distinct codes so far") +
    ggplot2::theme_minimal()
}

#' Distinct codes over the course of coding
#'
#' @inheritParams qda_plot_timeline
#' @return A data frame with `step` and `codes`.
#' @examples
#' qda_saturation(qda_read_history(qda_example("easyqda-history.csv")))
#' @export
qda_saturation <- function(history) {
  stopifnot(is.data.frame(history))
  d <- history[history$action == "add", , drop = FALSE]
  d <- d[order(d$ts), , drop = FALSE]
  seen <- character(0)
  codes <- integer(nrow(d))
  for (i in seq_len(nrow(d))) {
    seen <- union(seen, d$code[i])
    codes[i] <- length(seen)
  }
  data.frame(step = seq_len(nrow(d)), codes = codes)
}
