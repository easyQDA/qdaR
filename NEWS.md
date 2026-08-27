# qdaR 0.2.0

* Reads the **easyQDA-CSV-Collection** (contract `easyqda-collection`), the
  lossless relational CSV serialisation of a project beside the REFI-QDA
  `.qdpx`: `qda_read_collection()` returns a named list of data frames, one per
  thematic table, each checked against the shipped `collection-v1.json` (stamp,
  columns, version); `qda_read_collection_table()` reads a single table. The
  contract and reference samples are byte-identical with zotQDA.
* The exchange contract moved to v2 under the easyQDA brand: the stamp column
  is `easyqdaFormat`, the tables are `easyqda-*`, and format ids are `X/2`.

# qdaR 0.1.0

First release.

* Reads all nine formats of the zotQDA exchange contract v1, validating the
  format stamp, the version and the promised columns; refuses an export from
  a newer version rather than guessing at it. Reference files are installed
  with the package, so every example, test and vignette chunk runs without a
  Zotero installation and without network access.
* Projects from other QDA software can be read through the REFI-QDA
  interchange standard (`qda_read_qdpx()`); the subset a `.qdpx` file
  supports is reported on import.
* Reproduces the plugins' graphics with ggplot2 -- code frequencies, the
  code-document matrix, saturation, timeline, MDS -- and can additionally
  re-render the original Vega-Lite chart specifications unchanged via the
  suggested vegawidget package.
* Intercoder reliability reimplemented independently of the plugins:
  percentage agreement, Cohen's and Fleiss' kappa, Brennan and Prediger's
  coefficient, Krippendorff's alpha and Gwet's AC1, each with bootstrap or
  Wilson confidence intervals, plus level-wise agreement over a hierarchical
  code system, the confusion table and the annotation paradox diagnostics.
  Checked against frozen results of the plugins' JavaScript implementation.
* The reliability of the segmentation itself, before categories are even
  compared: Krippendorff's unitizing alpha, Mathet's gamma with its best
  alignment, WindowDiff and Pk.
* Progress and reporting: saturation measures with a stopping criterion,
  code drift across coding sessions, and generated COREQ and SRQR
  checklists.
* Inferential statistics the plugins deliberately leave out: chi-squared
  tests of code-by-group tables with Cramér's V and an exact or Monte Carlo
  fallback, Jaccard code distances, classical multidimensional scaling,
  hierarchical clustering with the cophenetic correlation, and
  correspondence analysis with the share of inertia shown.
* Licensed AGPL-3 with one additional permission on top: what you produce
  with the package -- results, figures, reports -- is yours,
  unconditionally.
