# Reading a REFI-QDA project, and agreeing with the Python twin about it.
#
# The fixture ships with the package, so these tests need neither Zotero nor
# MAXQDA.  qdpx-reference.json holds what the Python reader produces for the
# same archive; both packages carry the identical file, so the two readers are
# held to one reference rather than to each other.

skip_if_no_xml2 <- function() {
  testthat::skip_if_not_installed("xml2")
}

fixture <- function() qda_example("sample.qdpx")

project <- function() {
  skip_if_no_xml2()
  qda_read_qdpx(fixture(), warn = FALSE)
}

# ------------------------------------------------------------- the reading --

test_that("the fixture ships with the package", {
  expect_true(file.exists(fixture()))
  expect_true("sample.qdpx" %in% basename(qda_example()))
})

test_that("codings, codes, coders and sources arrive", {
  p <- project()
  expect_equal(nrow(p$fragments), 7L)
  expect_equal(nrow(p$codebook), 4L)
  expect_equal(p$coders, c("Ann", "Bob"))
  expect_equal(p$sources, c("Interview 01", "Interview 02", "Leitfaden"))
})

test_that("the code tree keeps its nesting and inherits colour", {
  p <- project()
  expect_setequal(
    p$codebook$code,
    c("Belastung", "Belastung/beruflich", "Belastung/privat", "Ressourcen")
  )
  child <- p$codebook[p$codebook$code == "Belastung/beruflich", ]
  expect_equal(child$parent, "Belastung")
  expect_equal(child$level, 2)
  expect_equal(child$color, "#CC3311")
})

test_that("codeId is the REFI GUID, so it survives a rename", {
  p <- project()
  top <- p$codebook[p$codebook$code == "Belastung", ]
  expect_equal(top$codeId, "AAAAAAA1-0000-0000-0000-000000000001")
  expect_true(all(p$fragments$codeId %in% p$codebook$codeId))
})

test_that("text is read from an internal file and from inline content", {
  p <- project()
  from_file <- p$fragments[p$fragments$title == "Interview 01", "text"][[1]]
  from_inline <- p$fragments[p$fragments$title == "Interview 02", "text"][[1]]
  expect_true(startsWith(from_file, "Die Arbeit war fordernd"))
  expect_true(startsWith(from_inline, "Am Anfang habe ich"))
})

test_that("character positions survive, which is what unitizing needs", {
  p <- project()
  txt <- p$fragments[p$fragments$positionKind == "text", ]
  expect_true(all(nzchar(txt$positionStart)))
  expect_true(all(nzchar(txt$positionEnd)))
})

test_that("a PDF selection keeps its page and rectangle", {
  p <- project()
  pdf <- p$fragments[p$fragments$positionKind == "pdf", ]
  expect_equal(nrow(pdf), 1L)
  expect_equal(pdf$positionPage, 2)   # numeric, like the CSV reader
  expect_equal(pdf$positionRects, "72 640 480 700")
})

test_that("a selection without a coding becomes an uncoded row", {
  p <- project()
  expect_equal(nrow(p$uncoded), 1L)
  expect_equal(p$uncoded$easyqdaFormat, "uncoded/2")
})

test_that("multiple coding by one coder is found", {
  p <- project()
  expect_equal(nrow(p$multi_coded), 1L)
  expect_equal(p$multi_coded$coder, "Ann")
  expect_equal(p$multi_coded$codes, "Belastung/beruflich+Belastung/privat")
})

test_that("the reconstructed log holds additions only", {
  p <- project()
  expect_setequal(p$history$action, "add")
  expect_false(is.unsorted(p$history$ts))
})

# ------------------------------------------------- what it refuses to hide --

test_that("unsupported elements are counted, not dropped in silence", {
  p <- project()
  expect_equal(p$skipped[["PictureSource"]], 1L)
  expect_equal(p$skipped[["Cases"]], 1L)
  expect_equal(p$skipped[["Notes"]], 1L)
})

test_that("the import warns by default", {
  skip_if_no_xml2()
  expect_warning(qda_read_qdpx(fixture()), "subset of the analyses")
})

test_that("the limitations name the missing pieces", {
  p <- project()
  joined <- paste(p$limitations, collapse = " ")
  for (expected in c("citekey", "consensus", "abbreviation", "additions only")) {
    expect_true(grepl(expected, joined, fixed = TRUE))
  }
})

test_that("a zip without project.qde is refused", {
  skip_if_no_xml2()
  bad <- file.path(tempdir(), "not-a-project.qdpx")
  note <- file.path(tempdir(), "readme.txt")
  writeLines("nothing to see", note)
  old <- setwd(tempdir()); on.exit(setwd(old), add = TRUE)
  utils::zip(bad, "readme.txt", flags = "-q")
  expect_error(qda_read_qdpx(bad, warn = FALSE), "no project.qde")
})

# ------------------------------------------- the analyses actually run on it --

test_that("counts, agreement and unitizing all run on a .qdpx", {
  p <- project()
  counts <- qda_code_counts(p$fragments)
  expect_equal(counts$n[counts$code == "Ressourcen"], 2L)

  u <- qda_units(p$fragments)
  res <- qda_agreement(u)
  expect_equal(as.integer(res$multi_set_aside), 1L)

  segs <- suppressWarnings(qda_segments(p$fragments))
  expect_setequal(names(segs), c("Ann", "Bob"))
  expect_gt(qda_unitizing_alpha(unname(segs))$Do, 0)
})

test_that("saturation runs off the reconstructed log", {
  p <- project()
  sat <- qda_saturation(p$history)
  expect_equal(max(sat$codes), 4L)
})

test_that("a frame from .qdpx has the same columns as one from CSV", {
  p <- project()
  csv <- qda_read_fragments(qda_example("easyqda-fragments.csv"))
  expect_equal(names(csv), names(p$fragments))
})

# ------------------------------------------------- congruence with the twin --

test_that("the reader agrees with the Python twin field for field", {
  skip_if_no_xml2()
  path <- testthat::test_path("qdpx-reference.json")
  skip_if_not(file.exists(path), "no frozen reference beside the tests")
  ref <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  p <- qda_read_qdpx(fixture(), warn = FALSE)

  # An absent value is null in JSON, NA or "" in a data frame.  Normalising
  # both sides compares what the two readers mean, not how each spells it.
  flat <- function(x) {
    if (is.null(x) || length(x) == 0L) return("")
    x <- as.character(x)[[1]]
    if (is.na(x)) "" else x
  }
  compare <- function(name, got) {
    want <- ref[[name]]
    expect_equal(nrow(got), length(want), info = name)
    for (i in seq_along(want)) {
      for (col in names(want[[i]])) {
        expect_equal(flat(got[[col]][[i]]), flat(want[[i]][[col]]),
                     info = sprintf("%s row %d column %s", name, i, col))
      }
    }
  }
  compare("fragments", p$fragments)
  compare("history", p$history)
  compare("uncoded", p$uncoded)
  compare("multi_coded", p$multi_coded)

  expect_equal(p$coders, unlist(ref$coders))
  expect_equal(p$sources, unlist(ref$sources))
  # The one legitimate difference: each package names its own function, so
  # R says qda_code_drift where Python says code_drift.
  expect_equal(sub("qda_", "", p$limitations, fixed = TRUE),
               unlist(ref$limitations))
})
