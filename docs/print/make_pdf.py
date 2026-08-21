#!/usr/bin/env python3
"""Render a documentation language project as one PDF.

    python3 make_pdf.py LANG_DIR OUTPUT_PDF

Assembles a temporary Quarto *book* from the pages of the website
project in LANG_DIR — the chapter order is the sidebar order, read from
the project's _quarto.yml, so the PDF cannot fall behind a page that was
added. Renders with Typst: no TeX toolchain, Noto Sans embedded from the
fonts/ directory beside this script, a linked table of contents, and
citations linked to one bibliography at the end (the pages' own
``::: {#refs}`` placements are stripped — in a single volume the
literature belongs in one place).

Executable pages run exactly as they do for the site; set QUARTO_PYTHON
before calling, as the site build already does. Needs PyYAML.
"""

import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

import yaml

FONTS = Path(__file__).resolve().parent / "fonts"

REFS_BLOCK = re.compile(
    r"\n#+ [^\n]*\{\.unnumbered\}\n+::: \{#refs\}\n:::\n?|\n::: \{#refs\}\n:::\n?")


def chapters(cfg):
    def walk(node):
        if isinstance(node, list):
            for item in node:
                yield from walk(item)
        elif isinstance(node, dict):
            if "href" in node:
                yield node["href"]
            yield from walk(node.get("contents", []))
    return list(walk(cfg["website"]["sidebar"]["contents"]))


def main(lang_dir, output_pdf):
    lang_dir = Path(lang_dir).resolve()
    output_pdf = Path(output_pdf).resolve()
    cfg = yaml.safe_load((lang_dir / "_quarto.yml").read_text("utf-8"))
    pages = chapters(cfg)
    lang = cfg.get("lang", "en")
    title = cfg["website"]["title"]
    subtitle = "Dokumentation" if lang == "de" else "Documentation"

    with tempfile.TemporaryDirectory(prefix="qda-pdf-") as tmp:
        tmp = Path(tmp)
        # index.qmd must lead a Quarto book; the sidebar starts with it.
        assert pages[0] == "index.qmd", "the sidebar must start with index.qmd"
        for page in pages:
            src = lang_dir / page
            dst = tmp / page
            dst.parent.mkdir(parents=True, exist_ok=True)
            dst.write_text(REFS_BLOCK.sub("\n", src.read_text("utf-8")),
                           encoding="utf-8")

        book = {
            "project": {"type": "book"},
            "lang": lang,
            # author is mandatory: the orange-book cover Quarto uses for
            # Typst books fails on a missing one
            "book": {"title": title, "subtitle": subtitle,
                     "author": "fre.ms", "chapters": pages},
            "format": {"typst": {
                "toc": True,
                "toc-depth": 2,
                "papersize": "a4",
                "mainfont": "Noto Sans",
                "font-paths": [str(FONTS)],
                "link-citations": True,
                "keep-typ": False,
            }},
        }
        bib = lang_dir / "references.bib"
        if bib.exists():
            shutil.copy(bib, tmp / "references.bib")
            book["bibliography"] = "references.bib"
        (tmp / "_quarto.yml").write_text(
            yaml.safe_dump(book, allow_unicode=True, sort_keys=False),
            encoding="utf-8")

        subprocess.run(["quarto", "render", str(tmp), "--to", "typst"],
                       check=True)
        made = list((tmp / "_book").glob("*.pdf"))
        assert len(made) == 1, f"expected one PDF, found {made}"
        output_pdf.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy(made[0], output_pdf)
    size = output_pdf.stat().st_size
    print(f"{output_pdf.name}: {size // 1024} KB, {len(pages)} chapters")


if __name__ == "__main__":
    if len(sys.argv) != 3:
        raise SystemExit(__doc__)
    main(sys.argv[1], sys.argv[2])
