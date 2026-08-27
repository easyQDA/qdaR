# qdaR

[![CI](https://github.com/easyqda/qdaR/actions/workflows/ci.yml/badge.svg)](https://github.com/easyqda/qdaR/actions/workflows/ci.yml)
[![r-universe](https://easyqda.r-universe.dev/badges/qdaR)](https://easyqda.r-universe.dev/qdaR)
[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.22111278-blue)](https://doi.org/10.5281/zenodo.22111278)
[![R ≥ 4.1](https://img.shields.io/badge/R-%E2%89%A5%204.1-276DC3?logo=r&logoColor=white)](https://www.r-project.org)
[![Project status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License: AGPL-3](https://img.shields.io/github/license/easyqda/qdaR)](LICENSE.md)

Read and analyse the qualitative coding that the Zotero plugins
[zotQDA](https://zotqda.org) and qdaZ export.

Primarily the analysis half of the **zotQDA ecosystem** — that is where the
full range is available. It also reads **REFI-QDA** (`.qdpx`) projects, so a
study kept in MAXQDA, ATLAS.ti, NVivo, QDA Miner or Dedoose can borrow the
metrics those programs do not document: confidence intervals on every
agreement coefficient, the kappa paradox diagnostics, the reliability of the
segmentation itself, saturation as a reportable number, and sample-size
planning. A `.qdpx` supports a subset, reported on import.

```r
library(qdaR)

frag <- qda_read_fragments(qda_example("easyqda-fragments.csv"))
qda_plot_frequencies(frag)
```

Full documentation, including a section for anyone wanting to extend the
package: <https://qdar.zotqda.org/>

## What it does

**Reads the exchange files and checks them.** Every file zotQDA writes carries
a stamp in its first column that says what kind of export it is and which
version of the exchange format it uses. `qdaR` compares that stamp against the
contract file both sides ship (`qda_contract()`). A file that claims a newer
version than this package knows stops with an error instead of being guessed
at.

**Reproduces the plugin's figures with ggplot2**, frequencies, code by
document, coding progress, saturation, and can re-render the original
Vega-Lite charts unchanged when *vegawidget* is installed.

**Adds the statistics the plugins leave out on purpose.** qdaZ sticks to
description and never runs a significance test, because a test invites claims
that many qualitative designs cannot carry. If your design does support one,
this is where you run it, and you pick it yourself:

| Function | Question |
|---|---|
| `qda_chisq()` | Are codes distributed independently of a grouping variable? With Cramer's V, and an exact test when the expected counts are too small -- Fisher's for a two-by-two table, a seeded Monte Carlo p-value for anything larger. |
| `qda_ca()` | Which codes and documents attract each other, and how much of the table's inertia am I actually seeing? The share is relative to the whole table, so keeping two of four dimensions does not report 100 percent. |
| `qda_mds()`, `qda_plot_mds()` | A map of codes by the segments they share. |
| `qda_cluster()` | Groups of codes — with the cophenetic correlation, because a dendrogram always looks convincing. |

**Recomputes the reliability figures independently.** Percentage agreement,
Cohen's and Fleiss' kappa, Brennan and Prediger's kappa, Krippendorff's alpha
and Gwet's AC1 -- computed here in R, from the exported file alone. Because
the plugin computes the same coefficients in JavaScript, a figure that ends up
in a methods section has been produced twice by two code bases that share
nothing but the contract; the test suite checks the two against each other on
randomly generated coder matrices. `qda_level_agreement()` additionally shows
*where in the code system* the agreement is lost, by flattening the paths
level by level.

**Carries code identity through.** Codes get renamed, moved and merged while a
project matures. Every export therefore names each code twice: `code` holds the
path a person reads, `codeId` a stable identifier that stays put through all of
that. If your analysis groups by the path, a code vanishes from it as soon as
somebody renames or moves that code in Zotero. Grouping by `codeId` survives
such housekeeping.

## Installation

From [R-universe](https://easyqda.r-universe.dev/qdaR) — prebuilt binaries for
Windows, macOS and Linux, no compiler needed:

```r
install.packages("qdaR", repos = c("https://easyqda.r-universe.dev",
                                   "https://cloud.r-project.org"))
```

Or the development version straight from GitHub:

```r
# install.packages("remotes")
remotes::install_github("easyqda/qdaR")
```

## The Python twin

[qdaPy](https://qdapy.zotqda.org/) is the same tool written for Python.
It reads the same files, computes the same coefficients and draws the same
figures, over there with plotnine and seaborn. A shared set of frozen fixtures
keeps both packages honest: every release gets checked against the plugin's
results and against the other package. Two genuine bugs in this very package
turned up exactly that way.

## Licence

**AGPL-3.0-or-later**, the same terms as zotQDA, qdaZ and the Python twin
qdaPy. (The CRAN package records the plain `AGPL-3`; the AGPL text ships with
R itself.)

**What you produce with qdaR is yours.** Figures, data frames, coefficients,
reports: the licence places no condition on any of it, by an additional
permission under section 7 of the AGPL, written down in
[`AGPL-ADDITIONAL-PERMISSION.md`](https://github.com/easyqda/qdaR/blob/main/AGPL-ADDITIONAL-PERMISSION.md)
— the same wording as in the sibling projects. Strictly speaking it changes
nothing, a copyleft licence has never reached into a program's output, but a
figure in a submitted manuscript is not the place for a licensing question, so
it is written down.

**Commercial use does not need a commercial licence.** The AGPL does not forbid
it. What it asks is that a *modified version* you distribute, or let others use
over a network, comes with its source. That is the only condition, and it
holds for everyone: qdaR is not dual-licensed, and no proprietary exception is
for sale.

**Contributions** are welcome and nothing has to be signed. There is no
contributor licence agreement, because there is no second licence that would
need one. You contribute under the AGPL and keep your copyright.
