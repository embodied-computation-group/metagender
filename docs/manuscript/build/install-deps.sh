#!/usr/bin/env bash
# Optional TeX packages. No sudo: this TeX Live lives in ~/.TinyTeX and tlmgr
# installs into it as your own user. Run it in a normal terminal -- tlmgr hangs
# with no output under the agent sandbox, which is why the build still uses the
# `article` class rather than apa7.
#
#   apa7 (+ endfloat threeparttablex environ trimspaces apacite scalerel)
#        -> build.sh switches itself to a real APA7 draft, no edits needed
#   newunicodechar
#        -> only matters if xelatex disappears and pdflatex has to be used
set -euo pipefail
# TeX Live 2023 is frozen; the current CTAN tlnet would refuse this installation.
tlmgr option repository \
  https://ftp.math.utah.edu/pub/tex/historic/systems/texlive/2023/tlnet-final
tlmgr install apa7 endfloat threeparttablex environ trimspaces apacite \
              scalerel newunicodechar
