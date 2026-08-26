# Contributing to qdaR

Thank you for helping improve qdaR. Bug reports, fixes and features are all
welcome.

## Build and test

qdaR is an R package (R ≥ 4.1). The tests use testthat (edition 3) and live in
`tests/testthat`.

```r
devtools::test()   # run the test suite
```

Before submitting, run the full CRAN-style check, which is what CI gates on:

```sh
R CMD check --as-cran
```

Two NOTEs are expected and documented in `cran-comments.md`; the check should be
free of ERRORs and WARNINGs.

## Proposing changes

- For anything larger than a small fix, open an issue first so the approach can
  be agreed before you write code.
- Keep pull requests focused: one topic per PR.
- Run the tests before you submit, and add tests for new behaviour.
- Update the documentation (roxygen blocks, `NEWS.md`, and `doc/en`, `doc/de`)
  when behaviour changes.

## Licensing

qdaR is licensed under the **AGPL-3.0-or-later** (recorded as `AGPL-3` in the
package metadata). Contributions are accepted under the same licence. There is
**no contributor licence agreement (CLA)** to sign, and you **keep the
copyright** to your contribution.
