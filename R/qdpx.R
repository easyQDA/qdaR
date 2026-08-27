# Reading a REFI-QDA project (.qdpx) into the tables the rest of qdaR expects.
#
# This is the way in for people who do not use zotQDA: MAXQDA, ATLAS.ti, NVivo,
# QDA Miner and Dedoose all export .qdpx, and a .qdpx carries enough for a
# large part of this package.  It does not carry everything, and the missing
# parts are named rather than papered over -- see qda_read_qdpx().
#
# The Python twin qdapy reads the same archive through the same steps, and a
# test asserts the two agree field for field on a shipped fixture.

NS_QDPX <- c(q = "urn:QDA-XML:project:1.0")

FRAGMENT_COLUMNS <- c(
  "code", "codeId", "citekey", "creator", "year", "title", "itemKey",
  "attachmentKey", "attachmentTitle", "pageLabel", "annotationKey",
  "annotationType", "color", "text", "comment", "weight", "allTags",
  "codedBy", "codedAt", "dateAdded", "dateModified", "positionKind",
  "positionStart", "positionEnd", "positionPage", "positionRects"
)

CODEBOOK_COLUMNS <- c(
  "code", "codeId", "parent", "name", "level", "kind", "abbrev", "color",
  "memo", "nAnnotations", "nAnnotationsRecursive", "nDocumentsRecursive",
  "defined"
)

.qdpx_need_xml2 <- function() {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop(
      "Reading .qdpx needs the 'xml2' package: install.packages(\"xml2\").\n",
      "It is a suggested dependency, so the rest of qdaR works without it.",
      call. = FALSE
    )
  }
}

.qdpx_attr <- function(node, name, default = "") {
  v <- xml2::xml_attr(node, name)
  if (is.na(v)) default else v
}

# ---------------------------------------------------------------- unpacking --

.qdpx_unpack <- function(path) {
  entries <- utils::unzip(path, list = TRUE)$Name
  qde <- entries[tolower(basename(entries)) == "project.qde" &
                   !grepl("/.+/", entries)]
  if (length(qde) == 0L) {
    stop(sprintf("%s: no project.qde in the archive - is this a .qdpx?", path),
         call. = FALSE)
  }
  dir <- file.path(tempdir(), paste0("qdpx-", as.integer(Sys.time())))
  utils::unzip(path, exdir = dir)
  list(root = dir, qde = file.path(dir, qde[[1]]))
}

.qdpx_find_file <- function(root, ref) {
  if (!nzchar(ref)) return(NULL)
  want <- tolower(basename(ref))
  all <- list.files(root, recursive = TRUE, full.names = TRUE)
  hit <- all[tolower(basename(all)) == want]
  if (length(hit) == 0L) NULL else hit[[1]]
}

# ------------------------------------------------------------------- parts --

.qdpx_users <- function(doc) {
  nodes <- xml2::xml_find_all(doc, "//q:User", NS_QDPX)
  guid <- vapply(nodes, .qdpx_attr, "", name = "guid")
  name <- vapply(nodes, .qdpx_attr, "", name = "name")
  name[!nzchar(name)] <- guid[!nzchar(name)]
  stats::setNames(as.list(name), guid)
}

.qdpx_walk_codes <- function(node, parent, inherited, acc) {
  for (code in xml2::xml_find_all(node, "./q:Code", NS_QDPX)) {
    name <- trimws(.qdpx_attr(code, "name"))
    if (!nzchar(name)) name <- "(unnamed)"
    path <- if (nzchar(parent)) paste0(parent, "/", name) else name
    colour <- .qdpx_attr(code, "color")
    if (!nzchar(colour)) colour <- inherited
    memo <- xml2::xml_find_first(code, "./q:Description", NS_QDPX)
    acc$rows[[length(acc$rows) + 1L]] <- data.frame(
      code = path, codeId = .qdpx_attr(code, "guid"), parent = parent,
      name = name, level = lengths(regmatches(path, gregexpr("/", path))) + 1L,
      kind = "code", abbrev = "", color = colour,
      memo = if (inherits(memo, "xml_missing")) "" else trimws(xml2::xml_text(memo)),
      nAnnotations = NA_integer_, nAnnotationsRecursive = NA_integer_,
      nDocumentsRecursive = NA_integer_, defined = 1L,
      stringsAsFactors = FALSE
    )
    acc <- .qdpx_walk_codes(code, path, colour, acc)
  }
  acc
}

.qdpx_codebook <- function(doc) {
  acc <- list(rows = list())
  for (codes in xml2::xml_find_all(doc, "//q:CodeBook/q:Codes", NS_QDPX)) {
    acc <- .qdpx_walk_codes(codes, "", "", acc)
  }
  if (length(acc$rows) == 0L) {
    df <- as.data.frame(
      stats::setNames(rep(list(character(0)), length(CODEBOOK_COLUMNS)),
                      CODEBOOK_COLUMNS),
      stringsAsFactors = FALSE
    )
  } else {
    df <- do.call(rbind, acc$rows)
  }
  df <- cbind(easyqdaFormat = rep("codebook/2", nrow(df)), df,
              stringsAsFactors = FALSE)
  df
}

.qdpx_plain_text <- function(src, root) {
  inline <- xml2::xml_find_first(src, "./q:PlainTextContent", NS_QDPX)
  if (!inherits(inline, "xml_missing")) return(xml2::xml_text(inline))
  file <- .qdpx_find_file(root, .qdpx_attr(src, "plainTextPath"))
  if (is.null(file)) return(NULL)
  paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

.qdpx_selection_base <- function(src, sel, kind) {
  base <- stats::setNames(
    as.list(rep("", length(FRAGMENT_COLUMNS))), FRAGMENT_COLUMNS
  )
  base$title <- .qdpx_attr(src, "name")
  base$itemKey <- .qdpx_attr(src, "guid")
  base$attachmentKey <- base$itemKey
  base$attachmentTitle <- base$title
  base$annotationKey <- .qdpx_attr(sel, "guid")
  desc <- xml2::xml_find_first(sel, "./q:Description", NS_QDPX)
  base$comment <- if (inherits(desc, "xml_missing")) "" else trimws(xml2::xml_text(desc))
  base$positionKind <- kind
  base
}

.qdpx_fill_text <- function(base, sel, text) {
  start <- .qdpx_attr(sel, "startPosition")
  end <- .qdpx_attr(sel, "endPosition")
  base$positionStart <- start
  base$positionEnd <- end
  base$annotationType <- "highlight"
  sliceable <- !is.null(text) && nzchar(start) && nzchar(end)
  base$text <- if (sliceable) {
    substr(text, as.integer(start) + 1L, as.integer(end))
  } else {
    .qdpx_attr(sel, "name")
  }
  base
}

.qdpx_fill_pdf <- function(base, sel) {
  base$positionPage <- .qdpx_attr(sel, "page")
  base$pageLabel <- base$positionPage
  base$annotationType <- "image"
  corners <- vapply(c("firstX", "firstY", "secondX", "secondY"),
                    function(k) .qdpx_attr(sel, k), "")
  base$positionRects <- trimws(paste(corners, collapse = " "))
  base$text <- .qdpx_attr(sel, "name")
  base
}

.qdpx_coding_row <- function(base, coding, codes, users) {
  ref <- xml2::xml_find_first(coding, "./q:CodeRef", NS_QDPX)
  guid <- if (inherits(ref, "xml_missing")) "" else .qdpx_attr(ref, "targetGUID")
  hit <- codes[codes$codeId == guid, , drop = FALSE]
  path <- if (nrow(hit)) hit$code[[1]] else guid
  colour <- if (nrow(hit)) hit$color[[1]] else ""
  user <- .qdpx_attr(coding, "creatingUser")
  stamp <- .qdpx_attr(coding, "creationDateTime")
  modified <- .qdpx_attr(coding, "modifiedDateTime")
  row <- base
  row$code <- path
  row$codeId <- guid
  row$color <- colour
  row$codedBy <- if (!is.null(users[[user]])) users[[user]] else user
  row$codedAt <- stamp
  row$dateAdded <- stamp
  row$dateModified <- if (nzchar(modified)) modified else stamp
  list(row = row, path = path)
}

.qdpx_selection_rows <- function(src, sel, kind, text, codes, users) {
  base <- .qdpx_selection_base(src, sel, kind)
  base <- if (kind == "text") .qdpx_fill_text(base, sel, text) else
    .qdpx_fill_pdf(base, sel)
  codings <- xml2::xml_find_all(sel, "./q:Coding", NS_QDPX)
  if (length(codings) == 0L) return(list(base))
  pairs <- lapply(codings, function(c) .qdpx_coding_row(base, c, codes, users))
  joined <- paste(vapply(pairs, function(p) p$path, ""), collapse = "|")
  lapply(pairs, function(p) {
    p$row$allTags <- joined
    p$row
  })
}

.qdpx_source_rows <- function(src, tag, root, codes, users) {
  text <- if (tag == "TextSource") .qdpx_plain_text(src, root) else NULL
  rows <- list()
  for (sel in xml2::xml_find_all(src, "./q:PlainTextSelection", NS_QDPX)) {
    rows <- c(rows, .qdpx_selection_rows(src, sel, "text", text, codes, users))
  }
  for (sel in xml2::xml_find_all(src, "./q:PDFSelection", NS_QDPX)) {
    rows <- c(rows, .qdpx_selection_rows(src, sel, "pdf", NULL, codes, users))
  }
  rows
}

.qdpx_sources <- function(doc, root, codes, users) {
  rows <- list()
  names_seen <- character(0)
  skipped <- list()
  for (src in xml2::xml_find_all(doc, "//q:Sources/*", NS_QDPX)) {
    tag <- xml2::xml_name(src)
    if (!tag %in% c("TextSource", "PDFSource")) {
      skipped[[tag]] <- (skipped[[tag]] %||% 0L) + 1L
      next
    }
    nm <- .qdpx_attr(src, "name")
    if (!nzchar(nm)) nm <- .qdpx_attr(src, "guid", "(unnamed source)")
    names_seen <- c(names_seen, nm)
    rows <- c(rows, .qdpx_source_rows(src, tag, root, codes, users))
  }
  list(rows = rows, sources = names_seen, skipped = skipped)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

.qdpx_count_unsupported <- function(doc, skipped) {
  for (tag in c("Cases", "Variables", "Sets", "Notes", "Links", "Graphs")) {
    n <- length(xml2::xml_find_all(doc, sprintf("//q:%s/*", tag), NS_QDPX))
    if (n > 0L) skipped[[tag]] <- (skipped[[tag]] %||% 0L) + n
  }
  skipped
}

# Give the columns the types the CSV reader gives them.  Same names is not
# enough: qda_segments() tests the positions with is.finite(), and a column of
# strings quietly matches nothing.  The contract says which columns are
# numbers, so this reads it rather than keeping a second list.
.qdpx_coerce <- function(df, kind) {
  spec <- qda_contract()$formats[[kind]]
  if (is.null(spec)) return(df)
  types <- vapply(spec$columns, function(c) c$type %||% "", "")
  keys <- vapply(spec$columns, function(c) c$key %||% "", "")
  for (nm in intersect(keys[types == "number"], names(df))) {
    df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  }
  df
}

# ---------------------------------------------------------------- assembly --

.qdpx_history <- function(frag) {
  coded <- frag[nzchar(frag$code) & nzchar(frag$codedAt), , drop = FALSE]
  out <- data.frame(
    easyqdaFormat = rep("history/2", nrow(coded)),
    ts = coded$codedAt, user = coded$codedBy,
    action = rep("add", nrow(coded)), code = coded$code,
    annotationKey = coded$annotationKey, citekey = "", creator = "",
    year = "", title = coded$title, pageLabel = coded$pageLabel,
    text = coded$text, stringsAsFactors = FALSE
  )
  out[order(out$ts), , drop = FALSE]
}

.qdpx_multi_coded <- function(frag) {
  coded <- frag[nzchar(frag$code), , drop = FALSE]
  key <- paste(coded$annotationKey, coded$codedBy, sep = "\r")
  rows <- list()
  for (k in unique(key)) {
    grp <- coded[key == k, , drop = FALSE]
    distinct <- sort(unique(grp$code))
    if (length(distinct) > 1L) {
      rows[[length(rows) + 1L]] <- data.frame(
        easyqdaFormat = "multi-coded/2", document = grp$title[[1]],
        text = grp$text[[1]], codes = paste(distinct, collapse = "+"),
        n = length(distinct), coder = grp$codedBy[[1]],
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) {
    return(data.frame(easyqdaFormat = character(0), document = character(0),
                      text = character(0), codes = character(0),
                      n = integer(0), coder = character(0),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

.qdpx_limitations <- function(frag, uncoded) {
  out <- c(
    paste("no bibliographic metadata: citekey, creator and year are empty,",
          "so crosstabs by author, year or collection are not available"),
    paste("no consensus exports: the three-phase consensus analyses need",
          "files only zotQDA writes"),
    "no code abbreviations",
    paste("the coding log holds additions only: REFI records no removals,",
          "so qda_code_drift over withdrawn codings cannot be computed")
  )
  if (nrow(uncoded) == 0L) {
    out <- c(out, paste(
      "no uncoded segments: agreement therefore answers the friendlier",
      "question of how well the coders agreed about material one of them",
      "marked"))
  }
  if (!any(nzchar(frag$codedBy))) {
    out <- c(out, paste(
      "no coders recorded: this export has no creatingUser on its codings,",
      "so no agreement coefficient can be computed at all"))
  }
  if (!any(nzchar(frag$codedAt))) {
    out <- c(out, paste(
      "no timestamps: saturation, the timeline and coder drift need",
      "creationDateTime, which this export does not carry"))
  }
  if (!any(nzchar(frag$positionStart))) {
    out <- c(out, paste(
      "no text positions: the unitizing measures and gamma need",
      "startPosition and endPosition on text selections"))
  }
  out
}

#' Read a REFI-QDA project file
#'
#' Reads a `.qdpx` archive -- the interchange format MAXQDA, ATLAS.ti, NVivo,
#' QDA Miner and Dedoose all write -- into the same tables `qda_read()`
#' produces from the zotQDA CSV exports.  Everything downstream then works
#' unchanged.
#'
#' A `.qdpx` supports a **subset** of these analyses, and the difference is
#' not a detail.  The exchange CSVs were designed for this package; `.qdpx`
#' was designed to move a project between programs.  What is missing is
#' listed in the returned object's `limitations` and, unless `warn = FALSE`,
#' printed as a warning.  Nothing is guessed: a column that cannot be filled
#' is empty, and an analysis needing it fails rather than returning a
#' flattering number.
#'
#' What does survive is more than one might expect: the code tree with its
#' GUIDs as stable identities, the coders, the timestamps, and the character
#' positions of text selections -- so the unitizing measures work too.
#'
#' Needs the suggested package **xml2**.
#'
#' @param path Path to a `.qdpx` archive.
#' @param warn Emit a warning listing what the file cannot support.  Leave it
#'   on until you have read the list once.
#'
#' @return A list of class `qda_qdpx` with the elements `fragments`,
#'   `codebook`, `history`, `uncoded`, `multi_coded`, `coders`, `sources`,
#'   `skipped` and `limitations`.
#' @examples
#' if (requireNamespace("xml2", quietly = TRUE)) {
#'   p <- qda_read_qdpx(qda_example("sample.qdpx"), warn = FALSE)
#'   nrow(p$fragments)
#'   p$coders
#'   p$limitations[1]
#' }
#' @export
qda_read_qdpx <- function(path, warn = TRUE) {
  .qdpx_need_xml2()
  un <- .qdpx_unpack(path)
  doc <- xml2::read_xml(un$qde)

  users <- .qdpx_users(doc)
  codebook <- .qdpx_codebook(doc)
  found <- .qdpx_sources(doc, un$root, codebook, users)
  skipped <- .qdpx_count_unsupported(doc, found$skipped)

  frag <- if (length(found$rows) == 0L) {
    as.data.frame(
      stats::setNames(rep(list(character(0)), length(FRAGMENT_COLUMNS)),
                      FRAGMENT_COLUMNS),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, lapply(found$rows, function(r) {
      as.data.frame(r, stringsAsFactors = FALSE)
    }))
  }
  frag <- cbind(easyqdaFormat = rep("fragments/2", nrow(frag)), frag,
                stringsAsFactors = FALSE)
  frag <- .qdpx_coerce(frag, "fragments")
  codebook <- .qdpx_coerce(codebook, "codebook")
  rownames(frag) <- NULL

  uncoded <- frag[!nzchar(frag$code), , drop = FALSE]
  if (nrow(uncoded)) uncoded$easyqdaFormat <- "uncoded/2"
  rownames(uncoded) <- NULL
  coded <- frag[nzchar(frag$code), , drop = FALSE]
  rownames(coded) <- NULL

  for (nm in c("qda_format", "qda_version", "qda_origin")) {
    attr(coded, nm) <- switch(nm, qda_format = "fragments",
                              qda_version = 1L, qda_origin = "qdpx")
    attr(codebook, nm) <- switch(nm, qda_format = "codebook",
                                 qda_version = 1L, qda_origin = "qdpx")
  }

  out <- structure(
    list(
      fragments = coded, codebook = codebook,
      history = .qdpx_history(frag), uncoded = uncoded,
      multi_coded = .qdpx_multi_coded(frag),
      coders = sort(unique(coded$codedBy[nzchar(coded$codedBy)])),
      sources = found$sources, skipped = skipped,
      limitations = .qdpx_limitations(coded, uncoded)
    ),
    class = "qda_qdpx"
  )
  if (warn) .qdpx_warn(out)
  out
}

.qdpx_warn <- function(x) {
  lines <- sprintf("%d codings read from a REFI-QDA project.",
                   nrow(x$fragments))
  if (length(x$skipped)) {
    dropped <- paste(sprintf("%s: %d", names(x$skipped), unlist(x$skipped)),
                     collapse = ", ")
    lines <- c(lines, sprintf("Not represented in these tables - %s.", dropped))
  }
  lines <- c(lines, "A .qdpx supports a subset of the analyses:",
             paste0("  - ", x$limitations),
             "Pass warn = FALSE once you have read this.")
  warning(paste(lines, collapse = "\n"), call. = FALSE)
}

#' @export
print.qda_qdpx <- function(x, ...) {
  cat(sprintf("<qda_qdpx> %d codings, %d codes, %d coders, %d sources\n",
              nrow(x$fragments), nrow(x$codebook), length(x$coders),
              length(x$sources)))
  invisible(x)
}
