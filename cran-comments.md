## Test environments
* local macOS (aarch64), R 4.6.1, `R CMD check --as-cran`
* GitHub Actions (R-CMD-check, all passing):
  - ubuntu-latest: R release, R oldrel-1
  - macOS-latest: R release
  - windows-latest: R release
* win-builder (R-devel): run immediately before submission

## R CMD check results
0 errors | 0 warnings

Expected notes:

* **New submission.** This is the first submission of qdaR.
* **HTML version of the manual.** The local HTML Tidy is older than the one
  `R CMD check` wants; this is a property of the checking machine, not of the
  package. It does not appear on the GitHub Actions runs.

## Notes for the reviewer
The package reads files written by two Zotero plugins. Reference files are
installed under `inst/extdata`, so every example, test and vignette chunk runs
without external software, without a Zotero installation and without network
access.

`vegawidget` is in Suggests and only used to re-render the original chart
specifications; every figure is also available through `ggplot2`, which is a
hard dependency.
