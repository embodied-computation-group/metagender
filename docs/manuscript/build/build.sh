#!/usr/bin/env bash
# Build the manuscript and supplement PDFs (or Word files) from the markdown
# sections. Run from anywhere:  bash docs/manuscript/build/build.sh
set -euo pipefail

# --------------------------------------------------------------------------
# 1. Paths (resolved from this script's own location, so cwd does not matter)
# --------------------------------------------------------------------------
BUILD="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MS="$(dirname "$BUILD")"          # docs/manuscript
ROOT="$(cd "$MS/../.." && pwd)"   # repo root -- figure paths are relative to this

# Intermediate assembled markdown is disposable; never leave it behind.
trap 'rm -f "$BUILD"/.assembled.*.md' EXIT

MAIN_SECTIONS=(00_frontmatter.md 01_introduction.md 02_methods.md \
               03_results.md 04_discussion.md 05_references.md)
SUPP_SECTION=06_supplement.md

# --------------------------------------------------------------------------
# 2. Preflight: what is installed?
# --------------------------------------------------------------------------
command -v pandoc >/dev/null || { echo "ERROR: pandoc not found. See $BUILD/README.md"; exit 1; }

ENGINE=""
for e in xelatex lualatex pdflatex tectonic; do
  command -v "$e" >/dev/null && { ENGINE="$e"; break; }
done
[ -n "$ENGINE" ] || { echo "ERROR: no LaTeX engine found. See $BUILD/README.md"; exit 1; }

# pdflatex cannot read unicode on its own; header.tex maps the characters the
# manuscript uses, but only if newunicodechar is installed.
if [ "$ENGINE" = pdflatex ] && ! kpsewhich newunicodechar.sty >/dev/null 2>&1; then
  echo "WARNING: pdflatex without newunicodechar -- characters like d' and Delta"
  echo "         will abort the build. Run: bash $BUILD/install-deps.sh"
fi

# apa7 gives a proper APA draft; article is the fallback (styled in header.tex).
# apa7 pulls in tikz for its ORCID icon, and this TinyTeX carries apa7 without
# pgf, so require both rather than aborting halfway through the first render.
if kpsewhich apa7.cls >/dev/null 2>&1 && kpsewhich tikz.sty >/dev/null 2>&1; then
  DOCCLASS=apa7
  # `doc` mode = readable draft with figures inline, rather than `man` (tables
  # and figures banished to the end of the document).
  CLASSOPTS=(-V classoption=doc -V classoption=donotrepeattitle)
else
  DOCCLASS=article
  CLASSOPTS=()
fi

# Unicode font, only meaningful for the engines that read system fonts.
# GLYPHS is the set of non-ASCII characters the section files and tables
# actually use. A font missing any one of them drops the glyph silently, so
# candidates are checked against the set and the first fully-covering one wins.
# Families fontconfig only serves as Type1 (.pfb/.t1) are skipped: xdvipdfmx
# cannot always embed them, which fails the build late and confusingly.
GLYPHS="2212 2013 2014 00d7 0394 2032 2033 00b1 2264 2265 2248 2192
        00b2 00b3 00b9 2070 2074 2075 2076 2077 2078 2079
        00e9 00f6 00f8 00fc 0107 0155"

pick_font() {        # pick_font <family> [family ...] -- sets FONTOPTS
  FONTOPTS=()
  [ "$ENGINE" = pdflatex ] && return 0
  local f cp ok
  for f in "$@"; do
    fc-list "$f" file 2>/dev/null | grep -q . || continue
    fc-list "$f" file 2>/dev/null | grep -qE '\.(pfb|t1):' && continue
    ok=1
    for cp in $GLYPHS; do
      fc-list ":family=$f:charset=$cp" family 2>/dev/null | grep -q . || { ok=0; break; }
    done
    [ "$ok" = 1 ] || continue
    FONTOPTS=(-V mainfont="$f"); return 0
  done
  echo "WARNING: no fully unicode-covering serif font found; using the LaTeX default"
}

pick_sans() {        # pick_sans <family> [family ...] -- sets SANSOPTS
  SANSOPTS=()
  [ "$ENGINE" = pdflatex ] && return 0
  local f
  for f in "$@"; do
    fc-list "$f" file 2>/dev/null | grep -q . || continue
    fc-list "$f" file 2>/dev/null | grep -qE '\.(pfb|t1):' && continue
    SANSOPTS=(-V sansfont="$f"); return 0
  done
}

# Defaults: the review draft. The preprint target overrides all of these.
METAFILE="$BUILD/metadata.yaml"
HEADERFILE="$BUILD/header.tex"
FILTERS=(--lua-filter="$BUILD/changed.lua")
EXTRAOPTS=()
SANSOPTS=()
pick_font "Liberation Serif" "FreeSerif" "Charis SIL" "DejaVu Serif" "TeX Gyre Termes"

echo "engine=$ENGINE  class=$DOCCLASS  ${FONTOPTS[*]:-font=default}"

# --------------------------------------------------------------------------
# 3. Include expansion
#    A line that is exactly `{{include: tables/T1.md}}` (path relative to
#    docs/manuscript/) is replaced by that file. A missing file becomes a
#    visible placeholder so the build still produces a readable PDF.
# --------------------------------------------------------------------------
expand_includes() {
  local line target
  while IFS= read -r line || [ -n "$line" ]; do
    if [[ "$line" =~ ^[[:space:]]*\{\{include:[[:space:]]*(.+[^[:space:]])[[:space:]]*\}\}[[:space:]]*$ ]]; then
      target="${BASH_REMATCH[1]}"
      if [ -f "$MS/$target" ]; then
        echo ""; cat "$MS/$target"; echo ""
      else
        echo ""
        echo "**[TABLE $(basename "$target" .md | tr '[:lower:]' '[:upper:]') MISSING]**"
        echo ""
        echo "  -> missing include: $target" >&2
      fi
    else
      printf '%s\n' "$line"
    fi
  done < "$1"
}

# --------------------------------------------------------------------------
# 4. Assemble + render
#    reference.docx is generated, not versioned; rebuild it whenever the script
#    that writes it is newer.
# --------------------------------------------------------------------------
make_reference_docx() {
  local ref="$BUILD/reference.docx" src="$BUILD/reference_docx.py"
  [ -f "$ref" ] && [ "$ref" -nt "$src" ] && return 0
  command -v python3 >/dev/null || { echo "ERROR: python3 not found, needed for $ref"; exit 1; }
  python3 "$src" "$ref"
}

render() {           # render <output.pdf|.docx> <section.md> [section.md ...]
  local out="$1"; shift
  local tmp; tmp="$(mktemp "$BUILD/.assembled.XXXXXX.md")"
  trap 'rm -f "$tmp"' RETURN

  local missing=0 s src
  for s in "$@"; do
    # section names are relative to docs/manuscript/, but a full path also works
    if   [ -f "$MS/$s" ]; then src="$MS/$s"
    elif [ -f "$s" ];     then src="$s"
    else
      echo "  -> missing section: $s" >&2
      missing=$((missing + 1)); continue
    fi
    expand_includes "$src" >> "$tmp"
    printf '\n\n' >> "$tmp"
  done
  if [ ! -s "$tmp" ]; then
    echo "SKIP $(basename "$out"): none of its source sections exist yet"; return 0
  fi

  # Word output shares the assembled markdown, the metadata and the change
  # filter, but none of the LaTeX layout: the page setup, fonts and styles come
  # from reference.docx instead (see reference_docx.py).
  local fmtopts=(--include-in-header="$HEADERFILE" --pdf-engine="$ENGINE" \
                 -V documentclass="$DOCCLASS" \
                 ${CLASSOPTS[@]+"${CLASSOPTS[@]}"} ${FONTOPTS[@]+"${FONTOPTS[@]}"} ${SANSOPTS[@]+"${SANSOPTS[@]}"})
  if [ "${out##*.}" = docx ]; then
    make_reference_docx
    fmtopts=(--reference-doc="$BUILD/reference.docx")
  fi

  # --columns is what pandoc measures a source line against: any pipe table with
  # a longer line is given relative column widths, which wraps every cell into a
  # narrow column and stretches the wide supplementary tables over many pages.
  # 250 clears every generated table (the widest source line is S7's, 226) so
  # their columns size themselves, while Table S1, whose cells are whole
  # paragraphs of prompt text, stays above the threshold and keeps wrapping.
  pandoc "$tmp" \
    --from=markdown+pipe_tables+implicit_figures+tex_math_dollars+raw_tex+autolink_bare_uris \
    --columns=250 \
    --metadata-file="$METAFILE" \
    "${FILTERS[@]}" \
    --resource-path="$ROOT:$MS:$BUILD" \
    "${fmtopts[@]}" ${EXTRAOPTS[@]+"${EXTRAOPTS[@]}"} \
    -o "$out"
  [ "$missing" -eq 0 ] && echo "wrote $out" || echo "wrote $out  ($missing section(s) not written yet)"
}

# --------------------------------------------------------------------------
# 5. Preprint: the clean manuscript typeset as a two-column paper in the
#    HenriquesLab preprint class (build/preprint/), with its bioRxiv branding
#    switched off in preprint-template.tex. The class is written for pdflatex,
#    so this target needs it; the template maps the manuscript's unicode.
# --------------------------------------------------------------------------
preprint_setup() {
  command -v pdflatex >/dev/null || { echo "ERROR: the preprint target needs pdflatex"; exit 1; }
  kpsewhich newunicodechar.sty >/dev/null 2>&1 || { echo "ERROR: newunicodechar.sty missing; run install-deps.sh"; exit 1; }
  ENGINE=pdflatex
  METAFILE="$BUILD/preprint-metadata.yaml"
  HEADERFILE=/dev/null
  FILTERS=(--lua-filter="$BUILD/changed.lua" --lua-filter="$BUILD/preprint.lua")
  EXTRAOPTS=(-M clean=true --template="$BUILD/preprint-template.tex")
  CLASSOPTS=(); FONTOPTS=(); SANSOPTS=()
  export TEXINPUTS="$BUILD/preprint//:${TEXINPUTS:-}"
  echo "preprint: HenriquesLab class, pdflatex"
}

# --------------------------------------------------------------------------
# 6. Clean: the review draft's layout with the change marking switched off.
#    changed.lua reads the `clean` flag exactly as it reads `preprint`, so the
#    blue disappears and the notes about it are dropped from the text.
# --------------------------------------------------------------------------
clean_setup() {
  EXTRAOPTS=(-M clean=true)
  echo "clean: no change marking"
}

case "${1:-all}" in
  main) render "$BUILD/manuscript.pdf" "${MAIN_SECTIONS[@]}" ;;
  preprint) preprint_setup
        render "$BUILD/preprint.pdf" "${MAIN_SECTIONS[@]}" ;;
  supp) render "$BUILD/supplement.pdf" "$SUPP_SECTION" ;;
  clean) clean_setup
        render "$BUILD/manuscript_clean.pdf" "${MAIN_SECTIONS[@]}"
        render "$BUILD/supplement_clean.pdf" "$SUPP_SECTION" ;;
  # Word, for coauthors who comment in Word. Same sections and same blue
  # marking as the review draft, with reference.docx supplying the layout.
  docx) render "$BUILD/manuscript.docx" "${MAIN_SECTIONS[@]}"
        render "$BUILD/supplement.docx" "$SUPP_SECTION" ;;
  # Response to reviewers (docs/response_to_reviewers.md, gitignored): Word for
  # coauthors to comment in, and a PDF alongside for reading.
  response) render "$BUILD/response_to_reviewers.docx" "$ROOT/docs/response_to_reviewers.md"
        render "$BUILD/response_to_reviewers.pdf" "$ROOT/docs/response_to_reviewers.md" ;;
  all)  render "$BUILD/manuscript.pdf" "${MAIN_SECTIONS[@]}"
        render "$BUILD/supplement.pdf" "$SUPP_SECTION" ;;
  # Any other argument is a single markdown file to render on its own -- handy
  # for previewing one section, or for smoke-testing this script.
  *)    [ -f "$1" ] || { echo "usage: bash build.sh [all|main|supp|clean|preprint|docx|response|<file.md>]"; exit 2; }
        render "$BUILD/$(basename "${1%.md}").pdf" "$1" ;;
esac
