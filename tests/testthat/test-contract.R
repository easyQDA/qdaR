test_that("the contract describes every shipped reference file", {
  fm <- qda_formats()
  expect_gte(nrow(fm), 9L)
  for (i in seq_len(nrow(fm))) {
    expect_true(file.exists(qda_example(fm$file[i])), info = fm$file[i])
  }
})

test_that("qdaR implements the version it ships", {
  ct <- qda_contract()
  expect_identical(ct$contract, "easyqda-exchange")
  expect_identical(ct$version, 2L)
})

test_that("the reference files match the contract column by column", {
  ct <- qda_contract()
  for (nm in names(ct$formats)) {
    spec <- ct$formats[[nm]]
    df <- qda_read(qda_example(spec$file))
    want <- vapply(spec$columns, function(c) c$key, character(1))
    expect_identical(names(df)[seq_along(want)], want, info = nm)
  }
})

test_that("the position columns arrive in both shapes (E37.1)", {
  df <- qda_read_fragments(qda_example("easyqda-fragments.csv"))
  for (col in c("positionKind", "positionStart", "positionEnd",
                "positionPage", "positionRects")) {
    expect_true(col %in% names(df), info = col)
  }
  expect_equal(df$positionKind, c("text", "pdf"))
  txt <- df[df$positionKind == "text", ][1, ]
  expect_equal(txt$positionStart, 120)
  expect_equal(txt$positionEnd, 180)
  pdf <- df[df$positionKind == "pdf", ][1, ]
  expect_equal(pdf$positionPage, 3)
  expect_equal(pdf$positionRects, "10.5 20 110 32|10.5 33 90 45")
})

test_that("an export without the position columns is rejected", {
  # they are part of the contract, so a file lacking them is broken.
  # Nothing may rely on a fallback: unitizing reliability is impossible
  # without a position, and silently accepting the file would produce
  # agreement figures with the segmentation question quietly dropped.
  ct <- qda_contract()
  cols <- vapply(ct$formats$fragments$columns, function(c) c$key, character(1))
  cols <- cols[!startsWith(cols, "position")]
  f <- withr::local_tempfile(fileext = ".csv")
  vals <- ifelse(cols == "easyqdaFormat", "fragments/1", "x")
  writeLines(c(paste(cols, collapse = ","), paste(vals, collapse = ",")), f)
  expect_error(qda_read_fragments(f), "missing contract columns")
  # and the caller can still look at it if they know what they are doing
  df <- qda_read(f, "fragments", strict = FALSE)
  expect_false("positionKind" %in% names(df))
})

test_that("no column is optional", {
  for (kind in names(qda_contract()$formats)) {
    spec <- qda_contract()$formats[[kind]]
    optional <- vapply(spec$columns, function(c) isTRUE(c$optional), logical(1))
    expect_false(any(optional), info = kind)
  }
})
