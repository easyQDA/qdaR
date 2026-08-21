frag <- function() qda_read_fragments(qda_example("zotqda-fragments.csv"))
hist_ <- function() qda_read_history(qda_example("zotqda-history.csv"))

test_that("the ggplot2 replications build without drawing", {
  expect_s3_class(qda_plot_frequencies(frag()), "ggplot")
  expect_s3_class(qda_plot_code_matrix(frag()), "ggplot")
  expect_s3_class(qda_plot_timeline(hist_()), "ggplot")
  expect_s3_class(qda_plot_saturation(hist_()), "ggplot")
  expect_s3_class(qda_plot_mds(frag(), min_n = 1), "ggplot")
})

test_that("the saturation curve never decreases", {
  s <- qda_saturation(hist_())
  expect_true(all(diff(s$codes) >= 0))
})

test_that("the code matrix agrees with the counts", {
  m <- qda_code_matrix(frag(), long = FALSE)
  expect_equal(sum(m), sum(nzchar(frag()$code)))
})

test_that("rendering an original chart says what is missing", {
  skip_if(requireNamespace("vegawidget", quietly = TRUE))
  expect_error(qda_spec_render(list(mark = "bar")), "vegawidget")
})

test_that("a specification is read with its provenance", {
  f <- withr::local_tempfile(fileext = ".json")
  writeLines(paste0('{"$schema":"https://vega.github.io/schema/vega-lite/v5.json",',
                    '"mark":"bar","usermeta":{"contract":"zotqda-exchange",',
                    '"version":1,"producer":"qdaZ","analysis":"frequencies"}}'), f)
  spec <- qda_spec_read(f)
  expect_equal(spec$mark, "bar")
  expect_equal(attr(spec, "qdaz_analysis"), "frequencies")
  expect_equal(attr(spec, "qdaz_version"), 1)
})

test_that("a specification from a newer exchange version warns", {
  f <- withr::local_tempfile(fileext = ".json")
  writeLines('{"mark":"bar","usermeta":{"version":99}}', f)
  expect_warning(qda_spec_read(f), "newer exchange version")
})

test_that("a specification without provenance is still read", {
  f <- withr::local_tempfile(fileext = ".json")
  writeLines('{"mark":"point"}', f)
  expect_equal(qda_spec_read(f)$mark, "point")
  expect_null(attr(qda_spec_read(f), "qdaz_analysis"))
})

test_that("rendering inlines the data when vegawidget is available", {
  skip_if_not_installed("vegawidget")
  spec <- list(`$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
               mark = "bar")
  out <- qda_spec_render(spec, qda_code_counts(frag()))
  expect_s3_class(out, "vegaspec")
  expect_equal(length(out$data$values$code), nrow(qda_code_counts(frag())))
})
