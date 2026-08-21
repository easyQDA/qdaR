#' Read a zotQDA exchange file
#'
#' Reads any of the files zotQDA writes and checks it against the contract:
#' the file must declare a known kind, a version this package understands,
#' and the columns the contract promises.  A file from a newer major version
#' is refused rather than guessed at.
#'
#' Files are UTF-8 with a byte-order mark and may use `,` or `;` as the
#' delimiter, depending on the setting in the plugin; both are detected.
#'
#' @param path Path to the CSV.
#' @param format Optional expected format, e.g. `"fragments"`.  When given,
#'   a file of a different kind is an error -- useful in scripts that must not
#'   silently accept the wrong export.
#' @param strict When `TRUE` (the default), a missing contract column is an
#'   error: every column the contract declares is part of it, so an export
#'   without one is broken rather than merely different.  Extra columns are
#'   always allowed -- readers address columns by name and ignore what they
#'   do not know.
#'
#' @return A data frame with the attributes `qda_format` (e.g. `"fragments"`),
#'   `qda_version` and `qda_grain`.
#' @examples
#' frag <- qda_read(qda_example("zotqda-fragments.csv"))
#' attr(frag, "qda_format")
#' names(frag)[1:4]
#' @export
qda_read <- function(path, format = NULL, strict = TRUE) {
  ct <- qda_contract()
  stamp <- contract_stamp(ct)

  raw <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  if (!length(raw)) stop("empty file: ", path, call. = FALSE)
  header <- sub("^\ufeff", "", raw[1])
  sep <- if (grepl(";", header, fixed = TRUE) &&
             !grepl(",", header, fixed = TRUE)) ";" else ","
  if (grepl(";", header, fixed = TRUE) && grepl(",", header, fixed = TRUE)) {
    sep <- if (lengths(regmatches(header, gregexpr(";", header)))[1] >
               lengths(regmatches(header, gregexpr(",", header)))[1]) ";" else ","
  }

  df <- utils::read.csv(path, sep = sep, colClasses = "character",
                        check.names = FALSE, encoding = "UTF-8",
                        stringsAsFactors = FALSE)
  names(df) <- sub("^\ufeff", "", names(df))

  if (!stamp %in% names(df)) {
    stop("not a zotQDA exchange file (no '", stamp, "' column): ", path,
         call. = FALSE)
  }
  ids <- unique(df[[stamp]])
  ids <- ids[nzchar(ids)]
  if (length(ids) != 1L) {
    stop("file mixes formats: ", paste(ids, collapse = ", "), call. = FALSE)
  }
  parts <- strsplit(ids, "/", fixed = TRUE)[[1]]
  kind <- parts[1]
  version <- suppressWarnings(as.integer(parts[2]))

  known <- vapply(ct$formats, function(f) f$id, character(1))
  if (!kind %in% names(ct$formats)) {
    stop("unknown export kind '", kind, "'; this package knows: ",
         paste(names(ct$formats), collapse = ", "), call. = FALSE)
  }
  if (is.na(version) || version > ct$version) {
    stop("file uses exchange version ", parts[2], ", this package implements ",
         ct$version, " -- please update qdaR", call. = FALSE)
  }
  if (!is.null(format) && !identical(format, kind)) {
    stop("expected a '", format, "' export but got '", kind, "'", call. = FALSE)
  }

  spec <- ct$formats[[kind]]
  want <- vapply(spec$columns, function(c) c$key, character(1))
  missing <- setdiff(want, names(df))
  if (length(missing) && strict) {
    stop("'", kind, "' export is missing contract columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  num <- vapply(spec$columns, function(c) identical(c$type, "number"), logical(1))
  for (nm in intersect(want[num], names(df))) {
    df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  }

  attr(df, "qda_format") <- kind
  attr(df, "qda_version") <- version
  attr(df, "qda_grain") <- spec$grain
  df
}

#' Read the coded fragments
#'
#' One row per annotation and code.  This is the table most analyses start
#' from.
#'
#' @inheritParams qda_read
#' @return A data frame; see [qda_read()].
#' @examples
#' frag <- qda_read_fragments(qda_example("zotqda-fragments.csv"))
#' nrow(frag)
#' @export
qda_read_fragments <- function(path) qda_read(path, format = "fragments")

#' Read the code system
#'
#' @inheritParams qda_read
#' @return A data frame; see [qda_read()].
#' @examples
#' cb <- qda_read_codebook(qda_example("zotqda-codebook.csv"))
#' cb$code
#' @export
qda_read_codebook <- function(path) qda_read(path, format = "codebook")

#' Read the coding history
#'
#' Every logged coding event, `add` and `remove`, oldest first.
#'
#' @inheritParams qda_read
#' @return A data frame; see [qda_read()].
#' @examples
#' h <- qda_read_history(qda_example("zotqda-history.csv"))
#' table(h$action)
#' @export
qda_read_history <- function(path) qda_read(path, format = "history")

#' Read the consensus mapping
#'
#' Which coder code corresponds to which consensus code.  This is what lets
#' phase-2 codings be analysed in terms of the consensus code system without
#' anyone rewriting a coding -- derived codings would inflate every
#' agreement figure by construction.
#'
#' @inheritParams qda_read
#' @return A data frame; see [qda_read()].
#' @examples
#' m <- qda_read_mapping(qda_example("zotqda-konsens-abbildung.csv"))
#' m$consensusCode
#' @export
qda_read_mapping <- function(path) qda_read(path, format = "consensus-mapping")

#' Apply a consensus mapping to coded fragments
#'
#' Adds a `consensusCode` column without touching `code`: the original
#' coding stays visible next to its consensus interpretation.
#'
#' @param fragments A fragments data frame.
#' @param mapping A consensus-mapping data frame.
#' @param coder_col Column holding the coder in `fragments`; defaults to
#'   `"codedBy"`.
#' @return `fragments` with an added `consensusCode` column.
#' @examples
#' frag <- qda_read_fragments(qda_example("zotqda-fragments.csv"))
#' map <- qda_read_mapping(qda_example("zotqda-konsens-abbildung.csv"))
#' out <- qda_apply_mapping(frag, map)
#' names(out)[ncol(out)]
#' @export
qda_apply_mapping <- function(fragments, mapping, coder_col = "codedBy") {
  stopifnot(is.data.frame(fragments), is.data.frame(mapping))
  key_f <- paste(fragments[[coder_col]], fragments$code, sep = "\r")
  key_m <- paste(mapping$coder, mapping$coderCode, sep = "\r")
  fragments$consensusCode <- mapping$consensusCode[match(key_f, key_m)]
  fragments
}
