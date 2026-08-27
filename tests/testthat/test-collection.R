test_that("the collection contract is the easyqda-collection contract", {
  ct <- qda_collection_contract()
  expect_identical(ct$contract, "easyqda-collection")
  expect_identical(ct$version, 1L)
  for (core in c("project", "codes", "selections", "codings", "history")) {
    expect_true(core %in% names(ct$tables))
  }
})

test_that("every sample table reads and matches the frozen expectations", {
  d <- system.file("extdata", "collection-samples", package = "qdaR")
  skip_if(!nzchar(d))
  expected <- jsonlite::fromJSON(file.path(d, "expected.json"),
                                 simplifyDataFrame = FALSE)
  ct <- qda_collection_contract()
  for (file in names(expected$files)) {
    exp <- expected$files[[file]]
    df <- qda_read_collection_table(file.path(d, file), exp$table)
    expect_identical(names(df), unlist(exp$columns))
    expect_identical(attr(df, "qda_table"), exp$table)
    expect_identical(attr(df, "qda_version"), 1L)
    stamp <- ct$tables[[exp$table]]$stampColumn
    expect_true(all(df[[stamp]] == exp$format))
  }
})

test_that("a whole collection reads from a directory", {
  d <- system.file("extdata", "collection-samples", package = "qdaR")
  skip_if(!nzchar(d))
  root <- tempfile("qda-coll-")
  dir.create(file.path(root, "tables"), recursive = TRUE)
  for (csv in list.files(d, pattern = "\\.csv$", full.names = TRUE)) {
    file.copy(csv, file.path(root, "tables", basename(csv)))
  }
  file.copy(file.path(d, "datapackage.json"), file.path(root, "datapackage.json"))
  tables <- qda_read_collection(root)
  for (core in c("codes", "selections", "codings", "history")) {
    expect_true(core %in% names(tables))
    expect_gt(nrow(tables[[core]]), 0L)
  }
})

test_that("a whole collection reads from a zip", {
  d <- system.file("extdata", "collection-samples", package = "qdaR")
  skip_if(!nzchar(d))
  root <- tempfile("qda-coll-")
  dir.create(file.path(root, "tables"), recursive = TRUE)
  for (csv in list.files(d, pattern = "\\.csv$", full.names = TRUE)) {
    file.copy(csv, file.path(root, "tables", basename(csv)))
  }
  zpath <- paste0(tempfile("demo"), ".easyqda-csv.zip")
  old <- setwd(root); on.exit(setwd(old))
  utils::zip(zpath, files = "tables", flags = "-rq")
  setwd(old)
  skip_if(!file.exists(zpath))
  tables <- qda_read_collection(zpath)
  expect_true("codes" %in% names(tables))
})

test_that("a newer version is refused", {
  p <- tempfile(fileext = ".csv")
  writeLines(c(
    "easyqdaFormat,codeId,parentId,name,path,color,isCodable,abbrev,memo",
    "collection-codes/2,X,,A,A,,1,,"), p)
  expect_error(qda_read_collection_table(p, "codes"), "please update qdaR")
})

test_that("a wrong stamp is rejected", {
  p <- tempfile(fileext = ".csv")
  writeLines(c(
    "easyqdaFormat,codeId,parentId,name,path,color,isCodable,abbrev,memo",
    "collection-selections/1,X,,A,A,,1,,"), p)
  expect_error(qda_read_collection_table(p, "codes"), "expected")
})
