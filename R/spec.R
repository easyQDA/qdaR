#' Read a chart specification written by qdaZ
#'
#' Every chart in qdaZ can be saved as a "data + spec" pair: the data as CSV
#' and the chart as a Vega-Lite specification.  The specification carries its
#' provenance in `usermeta`, so a reader can tell which analysis produced it.
#'
#' @param path Path to the `.json` specification.
#' @return The parsed specification, with the attributes `qdaz_analysis` and
#'   `qdaz_version` when the file states them.
#' @examples
#' f <- tempfile(fileext = ".json")
#' writeLines('{"mark":"bar","usermeta":{"contract":"zotqda-exchange",
#'   "version":1,"analysis":"demo"}}', f)
#' spec <- qda_spec_read(f)
#' attr(spec, "qdaz_analysis")
#' unlink(f)
#' @export
qda_spec_read <- function(path) {
  spec <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  meta <- spec$usermeta
  if (!is.null(meta)) {
    attr(spec, "qdaz_analysis") <- meta$analysis
    attr(spec, "qdaz_version") <- meta$version
    if (!is.null(meta$version) && meta$version > qda_contract()$version) {
      warning("specification uses a newer exchange version than qdaR knows",
              call. = FALSE)
    }
  }
  spec
}

#' Render a qdaZ chart specification
#'
#' Renders the original Vega-Lite chart, so a figure looks exactly as it did
#' in the plugin.  Requires the 'vegawidget' package; use the `qda_plot_*`
#' functions for 'ggplot2' versions that need no extra dependency.
#'
#' @param spec A specification from [qda_spec_read()], or a path to one.
#' @param data Optional data frame to inline into the specification, e.g.
#'   the CSV saved next to it.
#' @return A 'vegawidget' object.
#' @examples
#' if (requireNamespace("vegawidget", quietly = TRUE)) {
#'   spec <- list(`$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
#'                mark = "point")
#'   qda_spec_render(spec)
#' }
#' @export
qda_spec_render <- function(spec, data = NULL) {
  if (!requireNamespace("vegawidget", quietly = TRUE)) {
    stop("rendering the original chart needs the 'vegawidget' package; ",
         "the qda_plot_* functions draw the same figures with ggplot2",
         call. = FALSE)
  }
  if (is.character(spec) && length(spec) == 1L) spec <- qda_spec_read(spec)
  if (!is.null(data)) spec$data <- list(values = data)
  vegawidget::as_vegaspec(spec)
}
