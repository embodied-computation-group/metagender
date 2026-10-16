#!/usr/bin/env python3
"""reference_docx.py -- build/reference.docx, the Word template for the docx target.

pandoc ships a default reference.docx; this takes that file and patches three
things into it so the Word output matches the review PDF:

  * a character style `Revised`, the blue that changed.lua tags revised text with
  * Times New Roman at 12 pt, 1.5 line spacing, and 9 pt inside tables
  * A4 with 2.5 cm margins and continuous line numbers

The docx is a zip of XML, and the edits are small and local, so they are done
with plain string replacement rather than an XML library (no lxml dependency).

    python3 docs/manuscript/build/reference_docx.py [output.docx]
"""

import re
import subprocess
import sys
import zipfile
from pathlib import Path

# RGB 0,70,160 -- the same blue as RevBlue in header.tex.
REV_BLUE = "0046A0"

REVISED_STYLE = (
    '  <w:style w:type="character" w:customStyle="1" w:styleId="Revised">\n'
    '    <w:name w:val="Revised" />\n'
    '    <w:basedOn w:val="DefaultParagraphFont" />\n'
    '    <w:rPr>\n'
    f'      <w:color w:val="{REV_BLUE}" />\n'
    '    </w:rPr>\n'
    '  </w:style>\n'
)

# A4, 2.5 cm margins (1417 twips), line numbers running through the document.
SECT_PR = (
    '<w:sectPr>'
    '<w:pgSz w:w="11906" w:h="16838" />'
    '<w:pgMar w:top="1417" w:right="1417" w:bottom="1417" w:left="1417"'
    ' w:header="708" w:footer="708" w:gutter="0" />'
    '<w:lnNumType w:countBy="1" w:restart="continuous" />'
    '</w:sectPr>'
)


def replace_once(text, pattern, repl, what):
    """Regex-substitute exactly one match, or say which patch failed."""
    new, n = re.subn(pattern, repl, text, count=1, flags=re.S)
    if n != 1:
        sys.exit(f"ERROR: reference.docx patch '{what}' matched {n} times, expected 1")
    return new


def patch_styles(xml):
    # Body font: Times New Roman everywhere, dropping the theme references that
    # would otherwise win. The 24 half-points (12 pt) next to it stays as is.
    xml = replace_once(
        xml, r'<w:rFonts w:asciiTheme="minorHAnsi"[^>]*/>',
        '<w:rFonts w:ascii="Times New Roman" w:hAnsi="Times New Roman"'
        ' w:cs="Times New Roman" />', "body font")
    # 1.5 spacing (360 twips of a 240-twip line), as in the PDF draft.
    xml = replace_once(
        xml, r'<w:spacing w:after="200" />',
        '<w:spacing w:after="200" w:line="360" w:lineRule="auto" />', "line spacing")
    # 9 pt inside tables, so the wide supplementary tables fit the page.
    # rPr belongs before tblPr in a style definition.
    xml = replace_once(
        xml, r'(<w:style w:type="table" w:default="1" w:styleId="Table">.*?)<w:tblPr>',
        r'\1<w:rPr><w:sz w:val="18" /><w:szCs w:val="18" /></w:rPr>\n    <w:tblPr>',
        "table font size")
    xml = replace_once(xml, r'</w:styles>', REVISED_STYLE + '</w:styles>', "Revised style")
    return xml


def patch_document(xml):
    # The default has an empty <w:sectPr />; give it the page setup.
    if '<w:sectPr />' in xml:
        return xml.replace('<w:sectPr />', SECT_PR, 1)
    return replace_once(xml, r'</w:body>', SECT_PR + '</w:body>', "page setup")


def main():
    out = Path(sys.argv[1] if len(sys.argv) > 1
               else Path(__file__).with_name("reference.docx"))
    default = subprocess.run(
        ["pandoc", "--print-default-data-file", "reference.docx"],
        check=True, stdout=subprocess.PIPE).stdout
    src = Path(out).with_suffix(".default.docx")
    src.write_bytes(default)

    with zipfile.ZipFile(src) as zin, \
         zipfile.ZipFile(out, "w", zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            data = zin.read(item.filename)
            if item.filename == "word/styles.xml":
                data = patch_styles(data.decode("utf-8")).encode("utf-8")
            elif item.filename == "word/document.xml":
                data = patch_document(data.decode("utf-8")).encode("utf-8")
            zout.writestr(item, data)
    src.unlink()
    print(f"wrote {out}")


if __name__ == "__main__":
    main()
