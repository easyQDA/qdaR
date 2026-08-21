test_that("every reference file reads and reports its format", {
  # qda_example() also offers sample.qdpx, which belongs to qda_read_qdpx().
  for (f in grep("\\.csv$", qda_example(), value = TRUE)) {
    df <- qda_read(qda_example(f))
    expect_s3_class(df, "data.frame")
    expect_true(nzchar(attr(df, "qda_format")))
    expect_equal(attr(df, "qda_version"), 1L)
  }
})

test_that("the reference files survive the awkward CSV cases", {
  # the samples deliberately contain quotes, the delimiter and a line break
  # inside a field -- a naive reader splits these into extra rows
  frag <- qda_read_fragments(qda_example("zotqda-fragments.csv"))
  expect_equal(nrow(frag), 2L)
  expect_true(any(grepl("\n", frag$text, fixed = TRUE)))
  expect_true(any(grepl('"', frag$text, fixed = TRUE)))
  expect_true(any(grepl(",", frag$code, fixed = TRUE)))
})

test_that("a wrong or unknown format is refused, not guessed at", {
  expect_error(qda_read_fragments(qda_example("zotqda-codebook.csv")),
               "expected a 'fragments' export")

  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  writeLines(c("zotqdaFormat,code", "fragments/99,X"), f)
  expect_error(qda_read(f), "please update qdaR")

  writeLines(c("zotqdaFormat,code", "unheard-of/1,X"), f)
  expect_error(qda_read(f), "unknown export kind")

  writeLines(c("a,b", "1,2"), f)
  expect_error(qda_read(f), "not a zotQDA exchange file")
})

test_that("codes carry their stable identity", {
  frag <- qda_read_fragments(qda_example("zotqda-fragments.csv"))
  cb <- qda_read_codebook(qda_example("zotqda-codebook.csv"))
  expect_true("codeId" %in% names(frag))
  expect_true("codeId" %in% names(cb))
  expect_true(all(nzchar(frag$codeId)))
})

test_that("numeric contract columns arrive as numbers", {
  cb <- qda_read_codebook(qda_example("zotqda-codebook.csv"))
  expect_type(cb$nAnnotations, "double")
})

test_that("the mapping is applied without touching the original coding", {
  frag <- qda_read_fragments(qda_example("zotqda-fragments.csv"))
  map <- qda_read_mapping(qda_example("zotqda-konsens-abbildung.csv"))
  out <- qda_apply_mapping(frag, map)
  expect_true("consensusCode" %in% names(out))
  expect_identical(out$code, frag$code)
})
