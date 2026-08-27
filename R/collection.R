#' The easyQDA-CSV-Collection contract
#'
#' The collection (contract `easyqda-collection`, EXCHANGE.md section 6) is the
#' lossless, relational CSV serialisation of a project beside the REFI-QDA
#' `.qdpx`: thematic tables (`codes`, `selections`, `codings`, `history`, ...),
#' one stamped CSV each, packaged as `<project>.easyqda-csv.zip` or an unpacked
#' directory, with the binary sources carried alongside.
#'
#' @param path Optional path to a `collection-v*.json`.  Defaults to the copy
#'   shipped with this package, kept byte-identical with the one zotQDA writes.
#' @return A list with `contract`, `version` and `tables`.
#' @examples
#' ct <- qda_collection_contract()
#' ct$version
#' names(ct$tables)
#' @export
qda_collection_contract <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "collection-v1.json", package = "qdaR")
  }
  if (!nzchar(path) || !file.exists(path)) {
    stop("collection contract not found: ", path, call. = FALSE)
  }
  jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
}

# Read + check one stamped collection table from an already-read text vector.
.qda_read_collection_lines <- function(path, table, ct) {
  spec <- ct$tables[[table]]
  if (is.null(spec)) stop("unknown collection table: ", table, call. = FALSE)
  stamp <- spec$stampColumn

  raw <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  if (!length(raw)) stop("empty table file: ", table, call. = FALSE)
  header <- sub("^\ufeff", "", raw[1])
  sep <- if (grepl(";", header, fixed = TRUE) &&
             !grepl(",", header, fixed = TRUE)) ";" else ","

  df <- utils::read.csv(path, sep = sep, colClasses = "character",
                        check.names = FALSE, encoding = "UTF-8",
                        stringsAsFactors = FALSE)
  names(df) <- sub("^\ufeff", "", names(df))

  if (!stamp %in% names(df)) {
    stop("not a collection table (no '", stamp, "' column): ", table,
         call. = FALSE)
  }
  ids <- unique(df[[stamp]])
  ids <- ids[nzchar(ids)]
  if (length(ids) == 0L) {
    attr(df, "qda_table") <- table
    attr(df, "qda_version") <- ct$version
    return(df)
  }
  if (length(ids) != 1L) {
    stop("table ", table, " mixes stamps: ", paste(ids, collapse = ", "),
         call. = FALSE)
  }
  parts <- strsplit(ids, "/", fixed = TRUE)[[1]]
  kind <- parts[1]
  version <- suppressWarnings(as.integer(parts[2]))
  want_kind <- strsplit(spec$id, "/", fixed = TRUE)[[1]][1]
  if (!identical(kind, want_kind)) {
    stop("table ", table, " is stamped '", kind, "', expected '",
         want_kind, "'", call. = FALSE)
  }
  if (is.na(version) || version > ct$version) {
    stop("collection uses version ", parts[2], ", this package implements ",
         ct$version, " -- please update qdaR", call. = FALSE)
  }
  want <- vapply(spec$columns, function(c) c$key, character(1))
  missing <- setdiff(want, names(df))
  if (length(missing)) {
    stop("table ", table, " is missing contract columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  attr(df, "qda_table") <- table
  attr(df, "qda_version") <- version
  df
}

#' Read one collection table
#'
#' @param path Path to a table CSV (e.g. `codes.csv`).
#' @param table Table name; defaults to the file's base name.
#' @param contract Optional contract list (see [qda_collection_contract()]).
#' @return A data frame with attributes `qda_table` and `qda_version`.
#' @examples
#' d <- system.file("extdata", "collection-samples", package = "qdaR")
#' if (nzchar(d)) qda_read_collection_table(file.path(d, "codes.csv"))
#' @export
qda_read_collection_table <- function(path, table = NULL, contract = NULL) {
  if (is.null(table)) table <- tools::file_path_sans_ext(basename(path))
  ct <- if (is.null(contract)) qda_collection_contract() else contract
  .qda_read_collection_lines(path, table, ct)
}

#' Read a whole easyQDA-CSV-Collection
#'
#' @param src A `.easyqda-csv.zip` file or an unpacked `.easyqda-csv/`
#'   directory (both hold `tables/<name>.csv` and a `datapackage.json`).
#' @param contract Optional contract list (see [qda_collection_contract()]).
#' @return A named list of data frames, one per table that is present.
#' @examples
#' \dontrun{
#' tables <- qda_read_collection("study.easyqda-csv.zip")
#' names(tables)
#' }
#' @export
qda_read_collection <- function(src, contract = NULL) {
  ct <- if (is.null(contract)) qda_collection_contract() else contract
  out <- list()

  is_zip <- !dir.exists(src) && grepl("\\.zip$", src, ignore.case = TRUE)
  if (is_zip) {
    tmp <- tempfile("qda-coll-")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    utils::unzip(src, exdir = tmp)
    root <- tmp
  } else if (dir.exists(src)) {
    root <- src
  } else {
    stop("not a collection (.easyqda-csv.zip file or directory): ", src,
         call. = FALSE)
  }

  for (name in names(ct$tables)) {
    entry <- file.path(root, ct$tables[[name]]$file)
    if (file.exists(entry)) {
      out[[name]] <- .qda_read_collection_lines(entry, name, ct)
    }
  }
  out
}
