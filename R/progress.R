#' New codes per document, in coding order
#'
#' The input the saturation measures work on: how many codes appeared for the
#' first time in each document, with documents ordered by when they were
#' first coded.
#'
#' @param history A history data frame from [qda_read_history()].
#' @param doc_col Column identifying the document.
#' @return A data frame with `position`, `document`, `new_codes` and
#'   `cumulative`.
#' @examples
#' h <- data.frame(
#'   ts = sprintf("2026-01-%02dT09:00:00Z", 1:6),
#'   user = "ann", action = "add",
#'   code = c("A", "B", "A", "C", "A", "B"),
#'   citekey = c("d1", "d1", "d2", "d2", "d3", "d3")
#' )
#' qda_new_codes(h)
#' @export
qda_new_codes <- function(history, doc_col = "citekey") {
  stopifnot(is.data.frame(history))
  d <- history[history$action == "add", , drop = FALSE]
  d <- d[order(d$ts), , drop = FALSE]
  if (!nrow(d)) {
    return(data.frame(position = integer(), document = character(),
                      new_codes = integer(), cumulative = integer(),
                      stringsAsFactors = FALSE))
  }
  docs <- character(0)
  seen <- character(0)
  counts <- integer(0)
  for (i in seq_len(nrow(d))) {
    doc <- as.character(d[[doc_col]][i])
    if (!doc %in% docs) { docs <- c(docs, doc); counts <- c(counts, 0L) }
    code <- as.character(d$code[i])
    if (!code %in% seen) {
      seen <- c(seen, code)
      counts[match(doc, docs)] <- counts[match(doc, docs)] + 1L
    }
  }
  data.frame(position = seq_along(docs), document = docs,
             new_codes = counts, cumulative = cumsum(counts),
             stringsAsFactors = FALSE)
}

#' Saturation as a number you can report
#'
#' A saturation curve shows a trend; it does not answer "how many documents
#' were enough".  Guest, Namey and Chen (2020)
#' \doi{10.1371/journal.pone.0232076} operationalised the question with three
#' parameters and one ratio: a base of documents whose codes count as what is
#' already known, a run of consecutive later documents inspected for new
#' codes, and the share of new information that still counts as saturated.
#'
#' The result is reported as `"6+2"`: saturation declared at document 6,
#' confirmed over a run of 2.
#'
#' @section What this is not:
#' This is *code* saturation, and only that.  Hennink, Kaiser and Marconi
#' (2017) \doi{10.1177/1049732316665344} distinguish it from meaning
#' saturation, which no algorithm can see, and Braun and Clarke (2019)
#' \doi{10.1080/2159676X.2019.1704846} reject saturation altogether as a
#' criterion for reflexive thematic analysis.  If you report the number,
#' report which conception it belongs to.
#'
#' @param new_codes New codes per document, in order -- either the data frame
#'   from [qda_new_codes()] or a plain numeric vector.
#' @param base_size Documents forming the base; Guest et al. recommend 4 and
#'   found the choice barely mattered.
#' @param run_length Consecutive documents per run, successive runs
#'   overlapping by one.
#' @param threshold Share of new information still counting as saturated.
#' @return A list with `notation` (the string for the paper, or `NULL`),
#'   `saturated_at`, `base_codes`, the table of `runs`, and `reason` when the
#'   question could not be answered.
#' @examples
#' qda_saturation_ratio(c(4, 3, 2, 1, 1, 0, 0, 0))$notation
#' qda_saturation_ratio(c(4, 4, 4, 4, 4, 4))$reason
#' @export
qda_saturation_ratio <- function(new_codes, base_size = 4, run_length = 2,
                                 threshold = 0.05) {
  counts <- if (is.data.frame(new_codes)) new_codes$new_codes else new_codes
  counts <- pmax(0, as.numeric(counts))
  counts[is.na(counts)] <- 0
  base <- max(1L, as.integer(base_size))
  run <- max(1L, as.integer(run_length))
  n <- length(counts)
  base_codes <- sum(counts[seq_len(min(base, n))])

  runs <- data.frame(from = integer(), to = integer(), new_codes = numeric(),
                     ratio = numeric(), ok = logical())
  reached <- NULL
  if (n >= base + run && base_codes > 0) {
    starts <- seq(base, n - run)
    runs <- do.call(rbind, lapply(starts, function(s) {
      new_in_run <- sum(counts[seq(s + 1L, s + run)])
      ratio <- new_in_run / base_codes
      data.frame(from = s + 1L, to = s + run, new_codes = new_in_run,
                 ratio = ratio, ok = ratio <= threshold)
    }))
    hit <- which(runs$ok)
    if (length(hit)) reached <- runs$from[hit[1]] - 1L
  }
  list(base_size = base, run_length = run, threshold = threshold,
       base_codes = base_codes, documents = n, runs = runs,
       saturated_at = reached,
       notation = if (is.null(reached)) NULL else paste0(reached, "+", run),
       reason = if (n < base + run) "too few documents for one full run"
         else if (base_codes == 0) "no codes in the base set" else "")
}

#' Did a coder's behaviour shift while the project ran?
#'
#' The coding log records who coded what and when, which is unusual: most
#' tools keep no such trail, so this question normally cannot be asked at all.
#' What it supports is the distribution of codes a coder used in successive
#' windows, reported as the total variation distance to that coder's first
#' window.  Nought means they are coding as they started, one that the two
#' windows share no code.
#'
#' It is a description, not a test.  A large distance can mean the coder
#' drifted, or simply that the later material was about something else.  Read
#' it next to what was coded, not on its own.
#'
#' @param history A history data frame from [qda_read_history()].
#' @param windows Number of equal-count windows per coder.  Equal counts
#'   rather than equal time, because a coder who worked in bursts would
#'   otherwise get empty windows.
#' @return A data frame with one row per coder and window: `coder`, `window`,
#'   `n`, `codes`, `from`, `to` and `distance`.
#' @examples
#' h <- data.frame(
#'   ts = sprintf("2026-01-%02dT09:00:00Z", 1:8), user = "ann",
#'   action = "add", code = c(rep("A", 4), rep("B", 4)), citekey = "d1"
#' )
#' qda_code_drift(h, windows = 2)
#' @export
qda_code_drift <- function(history, windows = 4) {
  stopifnot(is.data.frame(history))
  w <- max(2L, as.integer(windows))
  d <- history[history$action == "add" & nzchar(history$code) &
                 nzchar(history$user) & nzchar(history$ts), , drop = FALSE]
  if (!nrow(d)) {
    return(data.frame(coder = character(), window = integer(), n = integer(),
                      codes = integer(), from = character(), to = character(),
                      distance = numeric(), stringsAsFactors = FALSE))
  }
  d <- d[order(d$ts), , drop = FALSE]
  out <- lapply(sort(unique(as.character(d$user))), function(user) {
    rows <- d[d$user == user, , drop = FALSE]
    per <- ceiling(nrow(rows) / w)
    idx <- split(seq_len(nrow(rows)), ceiling(seq_len(nrow(rows)) / per))
    shares <- lapply(idx, function(i) {
      tab <- table(as.character(rows$code[i]))
      tab / sum(tab)
    })
    first <- shares[[1]]
    do.call(rbind, lapply(seq_along(idx), function(k) {
      i <- idx[[k]]
      s <- shares[[k]]
      keys <- union(names(first), names(s))
      tv <- sum(abs(unname(sapply(keys, function(c) {
        (if (c %in% names(first)) first[[c]] else 0) -
          (if (c %in% names(s)) s[[c]] else 0)
      })))) / 2
      data.frame(coder = user, window = k, n = length(i), codes = length(s),
                 from = as.character(rows$ts[i[1]]),
                 to = as.character(rows$ts[i[length(i)]]),
                 distance = tv, stringsAsFactors = FALSE)
    }))
  })
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}
