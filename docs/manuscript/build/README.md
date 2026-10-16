# Manuscript build

    bash docs/manuscript/build/build.sh          # both PDFs (runs from anywhere)
    bash docs/manuscript/build/build.sh main     # manuscript only
    bash docs/manuscript/build/build.sh supp     # supplement only
    bash docs/manuscript/build/build.sh clean    # both PDFs, no change marking
    bash docs/manuscript/build/build.sh preprint # two-column preprint
    bash docs/manuscript/build/build.sh docx     # both as Word files
    bash docs/manuscript/build/build.sh response # response to reviewers, Word + PDF
    bash docs/manuscript/build/build.sh docs/manuscript/03_results.md   # one section

`build.sh` concatenates `00_frontmatter.md` .. `05_references.md` into
`manuscript.pdf` and `06_supplement.md` into `supplement.pdf`, both written here.
A line reading `{{include: tables/T1.md}}` is replaced by that file (path relative
to `docs/manuscript/`); a missing file becomes **[TABLE T1 MISSING]** instead of an
error, and sections that do not exist yet are skipped with a warning.

`clean` writes `manuscript_clean.pdf` and `supplement_clean.pdf` from the same
sections as `all`: same layout, but `changed.lua` sees `clean=true` and leaves the
revised text unmarked, dropping the `.marknote` notes that explain the blue.

`preprint` writes `preprint.pdf`: the clean manuscript set two-column in the
HenriquesLab preprint class (`preprint/`, GPL-3), via `preprint-template.tex` and
`preprint.lua`. The template switches off the class's bioRxiv footer branding, blue
links and bold abstract. The filter lifts title, authors, affiliations, abstract and
keywords from `00_frontmatter.md`; the running footer, corresponding author and the
list of full-width figures are in `preprint-metadata.yaml`. Needs pdflatex and
newunicodechar.

`docx` writes `manuscript.docx` and `supplement.docx` from the same assembled
markdown as the PDFs, for coauthors who comment in Word. The blue change marking
survives: `changed.lua` tags revised text with a `Revised` character style instead
of a LaTeX colour. Layout comes from `reference.docx` (Times New Roman 12pt, 1.5
spacing, 9pt tables, A4, 2.5 cm margins, continuous line numbers), which
`reference_docx.py` patches out of pandoc's default template; build.sh regenerates
it whenever that script is newer, and neither file is versioned.

Needs pandoc and a LaTeX engine (xelatex preferred, then lualatex, then pdflatex).
`apa7.cls` is used when it and its `tikz` dependency are both installed, otherwise
`article` styled by `header.tex` (12pt, 1.5 spacing, A4, 2.5 cm margins, line
numbers, unicode serif). Run `bash install-deps.sh` in a normal terminal to get
apa7; build.sh then switches by itself. `changed.lua` renders `[x]{.changed}` and
`::: changed` blocks in blue, and `[x]{.marknote}` blue in the review draft but
gone (with its paragraph) from the `clean` and `preprint` targets.
Figure paths are relative to the repo root. Title and authors live in
`00_frontmatter.md`, not in `metadata.yaml`.

pandoc gives a pipe table relative column widths, and so wraps every cell into a
narrow column, as soon as one of its source lines is longer than `--columns`.
The default 72 turned the wide supplementary tables into tens of pages, so the
build passes `--columns=250`: past every generated table, but still under Table
S1, whose cells are paragraphs of prompt text and should wrap. `header.tex` then
sets longtables in `\scriptsize`, the largest size at which the widest of them
stay inside the text block.
