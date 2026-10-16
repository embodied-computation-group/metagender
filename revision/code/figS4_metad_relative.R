# Revision Figure S4 -- Figure 4 recomputed on the RELATIVE-criterion fits
# =============================================================================
# The hmetad family can place the type-2 criteria at fixed positions on the
# decision axis (metac_absolute = TRUE, the variant the first edition's MATLAB
# HMeta-d used, and this revision's primary model) or relative to meta-d'
# (metac_absolute = FALSE). Metacognitive EFFICIENCY is nearly insensitive to the
# choice; metacognitive BIAS is not, because meta-Delta is measured on the
# criterion scale itself. This figure is the robustness check: exactly Figure 4,
# same axes, same labels, drawn from the two relative-criterion fits, so a reader
# can put the two side by side and see how little (or how much) moves.
#
# It is deliberately not a copy of the plotting code -- it hands two arguments to
# fig4_metad_posteriors.R and lets that script do the work.
#
# Inputs   revision/data/{draws,labels}_{fe_relative,vmp_relative}.csv
#          (07_contrast_draws.R --tag fe_relative / vmp_relative)
# Outputs  revision/figures/FigureS4_metad_relative.{png,pdf} and its caption file
#
# Run from anywhere:  Rscript revision/code/figS4_metad_relative.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

FIG_ARGS <- c("--tags", "fe_relative,vmp_relative",
              "--out",  "FigureS4_metad_relative",
              "--variant", "relative-criterion (robustness check; Figure 4 is the fixed-criterion primary model)")
source("revision/code/fig4_metad_posteriors.R")
