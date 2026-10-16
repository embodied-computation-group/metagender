# Revision Figures S1 and S2 -- the positive controls, drawn raw
# =============================================================================
# The editor asked that the "task difficulty was robustly controlled between
# genders" claim be supported rather than asserted. Model 26 supplies the Bayes
# factors and the ROPE; these two figures supply the raw distributions behind
# them, so a reader can see that the null is a null and not a floor, a ceiling
# or a pair of overlapping bimodal humps.
#
# S1  per cent correct per participant, one facet per domain
# S2  mean type-1 reaction time (s) per participant, one facet per domain
# Both: gender on the x axis, half boxplot with the participant points beside it,
# free y scale per facet (accuracy and RT differ by task by more than the gender
# effect ever could).
#
# Inputs   revision/data/cells.rds            (20_prep_cells.R)
# Outputs  revision/figures/FigureS1_accuracy.{png,pdf}
#          revision/figures/FigureS2_rt.{png,pdf}
#          revision/figures/FigureS1_accuracy_caption.txt
#          revision/figures/FigureS2_rt_caption.txt
#
# Run from anywhere:  Rscript revision/code/figS1S2_positive_controls.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

suppressMessages({
  library(ggplot2); library(dplyr)
  library(gghalves); library(cowplot)
})
source("code/theme_high_impact.R")

GENDER_COLS <- c(Men = "#0072B2", Women = "#D55E00")   # Okabe-Ito, as in Figures 2-4
DOMAIN_LAB  <- c(memory = "Memory", vision = "Vision", GDP = "GDP", Calories = "Calories")

cells <- readRDS("revision/data/cells.rds") |>
  mutate(gender2  = factor(ifelse(gender == "Feminin", "Women", "Men"),
                           levels = c("Women", "Men")),
         domain_f = factor(DOMAIN_LAB[as.character(domain)], levels = unname(DOMAIN_LAB)))
stopifnot(nlevels(droplevels(cells$subj)) == 317, !anyNA(cells$accuracy), !anyNA(cells$rt))

# ------------------------------------------------------------- the panel ----
# One drawing function, two DVs: the figures differ only in the column and the
# axis label, and drawing them the same way is the point of a positive control.
control_panel <- function(dv, ylab, title, subtitle) {
  ggplot(cells, aes(x = gender2, y = .data[[dv]])) +
    geom_half_boxplot(aes(fill = gender2), side = "l", width = 0.5, alpha = 0.85,
                      outlier.shape = NA, linewidth = 1) +
    geom_point(aes(x = as.numeric(gender2) + 0.22, colour = gender2),
               position = position_jitter(width = 0.07, height = 0),
               size = 0.9, alpha = 0.45) +
    facet_wrap(~ domain_f, nrow = 1, scales = "free_y") +
    scale_fill_manual(values = GENDER_COLS, guide = "none") +
    scale_colour_manual(values = GENDER_COLS, guide = "none") +
    labs(x = NULL, y = ylab, title = title, subtitle = subtitle) +
    theme_high_impact()
}

pS1 <- control_panel(
  "accuracy", "Correct (%)",
  "Task accuracy by gender and domain",
  "One point per participant; half boxplots show the median, quartiles and 1.5 IQR whiskers")

pS2 <- control_panel(
  "rt", "Mean type-1 RT (s)",
  "Type-1 reaction time by gender and domain",
  "One point per participant; half boxplots show the median, quartiles and 1.5 IQR whiskers")

# ----------------------------------------------------------------- output ----
dir.create("revision/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("revision/figures/FigureS1_accuracy.png", pS1, dpi = 300,
       width = 13, height = 5.5, units = "in")
ggsave("revision/figures/FigureS1_accuracy.pdf", pS1,
       width = 13, height = 5.5, units = "in")
ggsave("revision/figures/FigureS2_rt.png", pS2, dpi = 300,
       width = 13, height = 5.5, units = "in")
ggsave("revision/figures/FigureS2_rt.pdf", pS2,
       width = 13, height = 5.5, units = "in")

writeLines(c(
"Figure S1. Task accuracy by gender and task domain (N = 317; 172 women / 145 men).",
"Per cent correct for each participant in each domain, women in vermillion and men in blue;",
"half boxplots give the median, quartiles and 1.5 IQR whiskers, with one point per participant beside them, and the y scale is free per panel."),
  "revision/figures/FigureS1_accuracy_caption.txt")

writeLines(c(
"Figure S2. Type-1 reaction time by gender and task domain (N = 317; 172 women / 145 men).",
"Mean first-order response time in seconds for each participant in each domain, women in vermillion and men in blue;",
"half boxplots give the median, quartiles and 1.5 IQR whiskers, with one point per participant beside them, and the y scale is free per panel."),
  "revision/figures/FigureS2_rt_caption.txt")

cat("wrote revision/figures/FigureS1_accuracy.{png,pdf}, FigureS2_rt.{png,pdf} and captions\n")
