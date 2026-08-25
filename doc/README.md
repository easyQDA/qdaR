# Documentation

Quarto is the documentation system; the `.qmd` files in `en/` and `de/`
are the reference. One project per language, exactly parallel — the
same pages in the same order, which `gen_langmap.py` checks on every
build when it derives the language switcher's page map from the two
sidebars.

    build.sh          language map -> render en + de into ../site/{en,de}
                      -> offline repairs. Pins the Quarto version; the
                      header explains how to upgrade deliberately.
    gen_langmap.py    the page-to-page map for the language switcher,
                      derived from the two _quarto.yml sidebars
    _theme/           the one vendored copy of the theme extension and
                      the Noto fonts; build.sh mirrors it into en/ and
                      de/ before rendering (Quarto resolves extensions
                      only inside a project, and Nextcloud does not
                      sync symlinks) — the per-language copies are
                      untracked build artifacts
    update-theme.sh   re-vendors _theme/, offline/ and print/ from a
                      sibling zotqda-quarto-theme checkout
    offline/          vendored from the theme: postprocess.py (makes the
                      rendered site work from a plain file tree, fails
                      loudly when Quarto's output drifts), smoketest.py
                      (headless-browser proof), mathjax/
    print/            vendored from the theme: make_pdf.py builds one
                      linked Typst PDF per language from the sidebar
    shared/           versions.js (version banner master) and
                      references.bib. The bibliography is maintained in
                      qdaPy (doc/bibliography.py against the running
                      Zotero) — regenerate it there and copy it over
                      when the cited literature changes

The figures pages (`guide/figures.qmd` and its German twin) execute
their R chunks against the demo export that ships with the package, so
building the docs needs R with knitr, rmarkdown, ggplot2 and the
package itself installed:

    R CMD INSTALL .

Verify a build with

    python3 doc/offline/smoketest.py site/en site/de

Deployment is unchanged: `site/` is uploaded under a version directory,
`script/gen_versions.py` maintains `versions.json`, and the inlined
version banner does the rest.
