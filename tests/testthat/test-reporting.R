test_that("all thirty-two items are present and numbered once", {
  expect_equal(length(COREQ_ITEMS), 32L)
  expect_equal(vapply(COREQ_ITEMS, function(i) as.integer(i[1]), integer(1)),
               1:32)
})

test_that("the three domains have the published sizes", {
  domains <- table(vapply(COREQ_ITEMS, function(i) i[2], character(1)))
  expect_equal(as.integer(domains[["Research team and reflexivity"]]), 8L)
  expect_equal(as.integer(domains[["Study design"]]), 15L)
  expect_equal(as.integer(domains[["Analysis and findings"]]), 9L)
})

test_that("an item is reproduced verbatim", {
  item22 <- Filter(function(i) i[1] == "22", COREQ_ITEMS)[[1]]
  expect_equal(item22[4], "Data saturation")
  expect_equal(item22[5], "Was data saturation discussed?")
})

test_that("the checklist fills what the data knows and no more", {
  frag <- qda_read_fragments(qda_example("easyqda-fragments.csv"))
  cq <- qda_coreq(frag)
  expect_equal(nrow(cq), 32L)
  expect_setequal(cq$item[cq$filled], c(12L, 24L, 27L, 29L))
  expect_true(all(cq$answer[!cq$filled] == ""))
})

test_that("the saturation item arrives with a history", {
  h <- data.frame(
    ts = sprintf("2026-01-%02dT09:00:00Z", 1:12), user = "ann", action = "add",
    code = c("A", "B", "C", "D", "E", "F", "G", "H", "A", "B", "A", "B"),
    citekey = paste0("d", 1:12), stringsAsFactors = FALSE
  )
  row <- qda_coreq(history = h)
  row <- row[row$item == 22, ]
  expect_true(row$filled)
  expect_match(row$answer, "saturation")
})

test_that("the coding tree item arrives with a codebook", {
  cb <- qda_read_codebook(qda_example("easyqda-codebook.csv"))
  row <- qda_coreq(codebook = cb)
  row <- row[row$item == 25, ]
  expect_true(row$filled)
  expect_match(row$answer, "codes over")
})

test_that("the software item names both tools and can be overridden", {
  row <- qda_coreq()
  row <- row[row$item == 27, ]
  expect_match(row$answer, "zotQDA")
  expect_match(row$answer, "qdaR")
  custom <- qda_coreq(software = "MAXQDA 24")
  expect_equal(custom$answer[custom$item == 27], "MAXQDA 24")
})

test_that("the markdown carries every item and the citation", {
  frag <- qda_read_fragments(qda_example("easyqda-fragments.csv"))
  md <- paste(qda_coreq_markdown(qda_coreq(frag)), collapse = "\n")
  expect_match(md, "doi:10.1093/intqhc/mzm042", fixed = TRUE)
  for (it in COREQ_ITEMS) {
    expect_match(md, paste0("**", it[1], ". ", it[4], "**"), fixed = TRUE)
  }
  expect_equal(lengths(regmatches(md, gregexpr("*To be completed.*", md,
                                               fixed = TRUE))), 28L)
})

test_that("all twenty-one SRQR standards are present and numbered once", {
  expect_equal(length(SRQR_ITEMS), 21L)
  expect_equal(vapply(SRQR_ITEMS, function(i) i[1], character(1)),
               paste0("S", 1:21))
})

test_that("the SRQR sections have the published sizes", {
  s <- table(vapply(SRQR_ITEMS, function(i) i[2], character(1)))
  expect_equal(as.integer(s[["Methods"]]), 11L)
  expect_equal(as.integer(s[["Title and abstract"]]), 2L)
  expect_equal(as.integer(s[["Results/findings"]]), 2L)
})

test_that("an SRQR standard is reproduced verbatim", {
  s15 <- Filter(function(i) i[1] == "S15", SRQR_ITEMS)[[1]]
  expect_equal(s15[3], "Techniques to enhance trustworthiness")
  expect_match(s15[4], "audit trail")
})

test_that("SRQR fills what the data knows", {
  frag <- qda_read_fragments(qda_example("easyqda-fragments.csv"))
  sq <- qda_srqr(frag)
  expect_equal(nrow(sq), 21L)
  expect_setequal(sq$item[sq$filled], c("S12", "S13", "S14", "S17"))
})

test_that("SRQR has a home for the agreement figure where COREQ has none", {
  # the difference that decides which checklist to use when both are allowed
  h <- data.frame(ts = sprintf("2026-01-%02dT09:00:00Z", 1:5), user = "ann",
                  action = "add", code = LETTERS[1:5],
                  citekey = paste0("d", 1:5), stringsAsFactors = FALSE)
  row <- qda_srqr(history = h)
  row <- row[row$item == "S15", ]
  expect_true(row$filled)
  expect_match(row$answer, "audit trail")
  expect_match(row$answer, "agreement")
})

test_that("the saturation standard warns which saturation it means", {
  h <- data.frame(
    ts = sprintf("2026-01-%02dT09:00:00Z", 1:12), user = "ann", action = "add",
    code = c("A", "B", "C", "D", "E", "F", "G", "H", "A", "B", "A", "B"),
    citekey = paste0("d", 1:12), stringsAsFactors = FALSE)
  row <- qda_srqr(history = h)
  answer <- row$answer[row$item == "S8"]
  expect_match(answer, "code saturation")
  expect_match(answer, "not meaning saturation")
})

test_that("the SRQR markdown carries every standard and the citation", {
  frag <- qda_read_fragments(qda_example("easyqda-fragments.csv"))
  md <- paste(qda_srqr_markdown(qda_srqr(frag)), collapse = "\n")
  expect_match(md, "doi:10.1097/ACM.0000000000000388", fixed = TRUE)
  for (it in SRQR_ITEMS) {
    expect_match(md, paste0("**", it[1], " ", it[3], "**"), fixed = TRUE)
  }
})
