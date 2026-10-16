# Revision Figure 4 -- unified-model gender contrasts, both samples
# =============================================================================
# Reviewer 3 asked for one hierarchical model, per-domain as well as domain-general
# effects, and a SHARED X-AXIS so the panels can be compared by eye. Both columns
# below are drawn on one scale across both rows, and the row order is the two
# samples: the first edition on top, the VMP replication underneath.
#
#   left  column  log M-ratio    gender contrast, Women - Men (log units)
#   right column  meta-Delta     gender contrast, Women - Men, averaged over the
#                                two response sides
#   top    row    first edition  domain-general + memory, vision, GDP, Calories
#   bottom row    VMP            domain-general + GDP, Calories
#
# The density is shaded by sign in the gender palette used throughout the paper:
# vermillion where the contrast favours women, blue where it favours men. The grey
# band on the left column is the +/-0.05 log-unit ROPE.
#
# READING meta-Delta: it is the distance between the average confidence criterion
# and meta-c, so it runs BACKWARDS relative to confidence. A positive Women - Men
# contrast means women's criteria sit further out, i.e. women use lower confidence
# and are less metacognitively biased.
#
# ONE SCRIPT, TWO FIGURES. Figure 4 uses the FIXED-criterion fits (the primary
# model). Figure S4 is the identical plot of the RELATIVE-criterion fits, so it is
# produced by this same script through --tags / --out rather than by a copy of it;
# figS4_metad_relative.R is the two-line entry point that sets those arguments.
#
#   Rscript revision/code/fig4_metad_posteriors.R
#   Rscript revision/code/fig4_metad_posteriors.R --tags fe_relative,vmp_relative \
#           --out FigureS4_metad_relative
#
# Inputs   revision/data/draws_<tag>.csv, revision/data/labels_<tag>.csv  (07_contrast_draws.R)
# Outputs  revision/figures/<out>.{png,pdf} and <out>_caption.txt
#          (the model formula lives in the caption file, not in a panel subtitle)

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

suppressMessages({
  library(ggplot2); library(dplyr); library(ggdist); library(patchwork); library(cowplot)
})
source("code/theme_high_impact.R")
source("revision/code/helpers.R")

# figS4_metad_relative.R sets FIG_ARGS before sourcing this file; from the command
# line FIG_ARGS does not exist and the real arguments are used.
ARGS <- if (exists("FIG_ARGS")) FIG_ARGS else commandArgs(trailingOnly = TRUE)

TAGS    <- strsplit(getarg("--tags", "fe_fixed,vmp_fixed", ARGS), ",")[[1]]
OUT     <- getarg("--out", "Figure4", ARGS)
VARIANT <- getarg("--variant", "fixed-criterion", ARGS)

GENDER_COLS <- c(`TRUE` = "#D55E00", `FALSE` = "#0072B2")   # Okabe-Ito: women / men
ROW_LAB     <- c("Discovery sample (318 participants, four domains)",
                 "Replication sample (456 participants, the same two general knowledge domains)")
DOMAIN_LAB  <- c(`domain-general` = "Domain-general", memory = "Memory",
                 vision = "Vision", GDP = "GDP", Calories = "Calories")
ROPE        <- 0.05

# ------------------------------------------------------------------ data ----
rd <- function(what, tag) {
  f <- sprintf("revision/data/%s_%s.csv", what, tag)
  if (!file.exists(f)) stop("missing ", f, " -- run 07_contrast_draws.R --tag ", tag)
  read.csv(f, stringsAsFactors = FALSE)
}
draws  <- bind_rows(lapply(TAGS, function(t) rd("draws",  t)))
labels <- bind_rows(lapply(TAGS, function(t) rd("labels", t)))

# Domain-general at the top of each panel, then the domains in the paper's order.
ord <- function(x) factor(unname(DOMAIN_LAB[x]),
                          levels = rev(unname(DOMAIN_LAB[names(DOMAIN_LAB) %in% x])))
draws$domain_f  <- ord(draws$domain)
labels$domain_f <- ord(labels$domain)

# Shared x-axis WITHIN a column, ACROSS both rows (Reviewer 3). The label sits in the
# right-hand margin, so the limits are widened to make room for it.
xlim <- draws |> group_by(quantity) |>
  summarise(lo = quantile(value, .0005), hi = quantile(value, .9995), .groups = "drop") |>
  mutate(pad = 0.18 * (hi - lo), lo = pmin(lo, -pad) - 0.25 * pad, hi = hi + 2.1 * pad)

fmt_bf <- function(x) ifelse(is.na(x), "BF10 = NA",
                      ifelse(x >= 1000 | x < 0.001, sprintf("BF10 = %.1e", x),
                             sprintf("BF10 = %.2f", x)))

# ----------------------------------------------------------------- panel ----
panel <- function(tag, quantity, row, col) {
  d <- filter(draws,  tag == !!tag, quantity == !!quantity)
  l <- filter(labels, tag == !!tag, quantity == !!quantity)
  x <- filter(xlim, quantity == !!quantity)
  # if(), not ifelse(): `quantity` is a length-1 argument, so ifelse() would recycle a
  # single row's label onto every density.
  l$txt <- if (quantity == "logM")
    sprintf("%s, %.0f%% in ROPE", fmt_bf(l$bf10), l$pct_in_rope) else fmt_bf(l$bf10)

  p <- ggplot(d, aes(x = value, y = domain_f))
  if (quantity == "logM")
    p <- p + annotate("rect", xmin = -ROPE, xmax = ROPE, ymin = -Inf, ymax = Inf,
                      fill = "grey85", alpha = 0.55)
  p <- p +
    geom_vline(xintercept = 0, linewidth = 0.7, colour = "grey30") +
    stat_halfeye(aes(fill = after_stat(x > 0)), .width = c(.66, .95), point_size = 2,
                 slab_colour = "grey25", slab_linewidth = 0.35, normalize = "xy",
                 height = 0.85) +
    geom_text(data = l, aes(x = x$hi, y = domain_f, label = txt),
              inherit.aes = FALSE, hjust = 1, vjust = -0.9, size = 4.2, colour = "grey20") +
    scale_fill_manual(values = GENDER_COLS, breaks = c("TRUE", "FALSE"),
                      labels = c("Women > Men", "Men > Women"), name = NULL) +
    coord_cartesian(xlim = c(x$lo, x$hi), clip = "off") +
    labs(x = if (row == 2)
              c(logM = "Gender contrast on log M-ratio (log units)",
                metadelta = "Gender contrast on meta-Δ")[quantity] else NULL,
         y = NULL,
         title = if (row == 1)
                   c(logM = "Metacognitive efficiency",
                     metadelta = "Metacognitive bias")[quantity] else NULL,
         tag = LETTERS[(row - 1) * 2 + col]) +
    theme_high_impact() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 15),
          legend.position = "none")
  if (col == 2) p <- p + theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.line.y = element_blank())
  p
}

rows <- lapply(seq_along(TAGS), function(i)
  wrap_elements(panel(TAGS[i], "logM", i, 1) + panel(TAGS[i], "metadelta", i, 2) +
                  plot_layout(widths = c(1.15, 1)) +
                  plot_annotation(title = ROW_LAB[i],
                                  theme = theme(plot.title = element_text(
                                    size = 14, face = "bold", hjust = 0, colour = "grey25")))))

# One legend for the whole figure. patchwork cannot collect guides through the
# wrap_elements() used to give each row its own title, so the guide box is pulled off a
# throwaway copy of the first panel and laid out as a third row.
leg <- get_plot_component(
  panel(TAGS[1], "logM", 1, 1) +
    theme(legend.position = "bottom", legend.justification = "center",
          legend.text = element_text(size = 13)),
  "guide-box-bottom", return_all = FALSE)

fig <- (rows[[1]] / rows[[2]] / wrap_elements(full = leg)) +
  plot_layout(heights = c(1.35, 1, 0.09))

dir.create("revision/figures", showWarnings = FALSE, recursive = TRUE)
ggsave(sprintf("revision/figures/%s.png", OUT), fig, dpi = 300,
       width = 13, height = 9.5, units = "in")
# cairo_pdf, not the default pdf device: the default cannot render the Delta in "meta-Δ".
ggsave(sprintf("revision/figures/%s.pdf", OUT), fig, device = cairo_pdf,
       width = 13, height = 9.5, units = "in")

# ------------------------------------------------------------- caption -----
writeLines(c(
sprintf("Gender contrasts (Women - Men) on metacognitive efficiency and metacognitive bias,"),
"from the unified hierarchical meta-d' model fitted to the first-edition sample (top row;",
"318 participants, four domains) and to the VMP replication sample (bottom row; 456",
sprintf("participants, the same two general knowledge domains). Confidence model: %s.", VARIANT),
"",
"Model. All fifteen distributional parameters of the hmetad family (log M-ratio, d', c and",
"the twelve type-2 criterion log-spacings for K = 7) share the linear predictor",
"",
"    <parameter> ~ 0 + domain + domain:gender + (0 + domain | participant),",
"",
"with gender sum-coded (+0.5 women / -0.5 men), so domain:gender is the Women - Men contrast",
"WITHIN a domain and the domain-general effect is the draw-wise mean of the per-domain",
"contrasts rather than a separate coefficient. Priors: domain cell means normal(0, 0.5) on",
"log M-ratio and c, normal(1, 0.5) on d', normal(-0.5, 1) on the criteria; every gender",
"contrast normal(0, 0.5); sd normal(0, 1); correlations lkj(2).",
"",
"Panels. Posterior densities with the median and the 66% and 95% quantile intervals, shaded",
"vermillion where the contrast favours women and blue where it favours men. The x-axis is",
"shared between the two rows within each column. Left: log M-ratio in log units, with the",
"+/-0.05 log-unit region of practical equivalence shaded grey. Right: meta-Delta averaged",
"over the two response sides. meta-Delta is the distance between the average confidence",
"criterion and meta-c, so a POSITIVE contrast means women use lower confidence and are LESS",
"metacognitively biased. Labels give the Savage-Dickey BF10 against a point null at zero",
"and, for log M-ratio, the percentage of the posterior inside the ROPE."),
  sprintf("revision/figures/%s_caption.txt", OUT))

cat(sprintf("wrote revision/figures/%s.{png,pdf} and %s_caption.txt\n", OUT, OUT))
