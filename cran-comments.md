## Test environments
* local macOS (aarch64), R 4.6.1

## R CMD check results
0 errors | 0 warnings | 2 notes

Both notes are expected:

* **New submission.** Both URLs in DESCRIPTION return 404 until the repository
  and the documentation site go live, which happens with this first release.
* **HTML version of the manual.** The local HTML Tidy is older than the one
  `R CMD check` wants; this is a property of the checking machine, not of the
  package.

## Notes for the reviewer
The package reads files written by two Zotero plugins. Reference files are
installed under `inst/extdata`, so every example, test and vignette chunk runs
without external software, without a Zotero installation and without network
access.

`vegawidget` is in Suggests and only used to re-render the original chart
specifications; every figure is also available through `ggplot2`, which is a
hard dependency.
