#' The zotQDA exchange contract
#'
#' zotQDA and qdaZ write versioned files described by a machine-readable
#' contract.  Every file states its kind and version in its first column, so
#' a reader can recognise what it is holding without relying on the file name
#' and can refuse a version it does not understand instead of quietly
#' computing something wrong.
#'
#' @param path Optional path to an `exchange-v*.json`.  Defaults to the copy
#'   shipped with this package.
#'
#' @return A list with `contract`, `version`, `csv` and `formats`.
#' @examples
#' ct <- qda_contract()
#' ct$version
#' names(ct$formats)
#' @export
qda_contract <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "exchange-v2.json", package = "qdaR")
  }
  if (!nzchar(path) || !file.exists(path)) {
    stop("exchange contract not found: ", path, call. = FALSE)
  }
  jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
}

#' Supported exchange formats
#'
#' @inheritParams qda_contract
#' @return A data frame with one row per format: `format`, `id`, `file`,
#'   `grain` and the number of columns.
#' @examples
#' qda_formats()
#' @export
qda_formats <- function(path = NULL) {
  ct <- qda_contract(path)
  rows <- lapply(names(ct$formats), function(nm) {
    f <- ct$formats[[nm]]
    data.frame(
      format = nm, id = f$id, file = f$file, grain = f$grain,
      columns = length(f$columns), stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Path to a bundled reference file
#'
#' The reference files from the contract are installed with this package, so
#' every example and test runs without a Zotero installation.  They contain
#' the awkward cases on purpose: quotes, the delimiter and a line break
#' inside a field.
#'
#' @param file File name, e.g. `"easyqda-fragments.csv"`.  Call without
#'   arguments to list what is available.
#' @return A file path, or the available names when called with no argument.
#' @examples
#' qda_example()
#' qda_example("easyqda-fragments.csv")
#' @export
qda_example <- function(file = NULL) {
  dir <- system.file("extdata", package = "qdaR")
  if (is.null(file)) {
    return(sort(list.files(dir, pattern = "\\.(csv|qdpx)$")))
  }
  path <- file.path(dir, file)
  if (!file.exists(path)) {
    stop("no such reference file: ", file, call. = FALSE)
  }
  path
}

# The stamp column and the expected format id, from the contract.
contract_stamp <- function(ct) {
  ids <- vapply(ct$formats, function(f) f$stampColumn, character(1))
  unname(ids[1])
}
