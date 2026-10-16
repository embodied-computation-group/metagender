# Revision figure 2 -- global metacognitive calibration (self-belief - accuracy)
# =============================================================================
# Replaces the first edition's Figure 2 (raw self-belief). Two changes the
# reviewers asked for:
#   * the plotted quantity is now calibration, self-belief minus accuracy on one
#     percent-correct scale, so zero has a meaning (Reviewer 1 point 1);
#   * the colour map on the individual difference lines is gone -- it was
#     redundant with the slope of the line itself (Reviewer 3).
#
# A  subject-mean calibration pre vs post, one thin grey line per participant,
#    half boxplots per gender x time, zero marked as "calibrated".
# B  per-domain calibration by gender, posterior mean and 95% HDI from model 21
#    (revision/data/effects_calibration.csv), pre and post as small multiples.
#
# Inputs   revision/data/cells.rds
#          revision/data/effects_calibration.csv   (run 21_global_calibration.R first)
# Outputs  revision/figures/Figure2.png, revision/figures/Figure2.pdf
#
# Run from anywhere:  Rscript revision/code/fig2_calibration.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

suppressMessages({
  library(ggplot2); library(dplyr); library(tidyr)
  library(gghalves); library(cowplot); library(patchwork)
})
source("code/theme_high_impact.R")

# Okabe-Ito blue / vermillion: distinguishable under all common colour-vision
# deficiencies and in greyscale.
GENDER_COLS <- c(Men = "#0072B2", Women = "#D55E00")
DOMAIN_LAB  <- c(memory = "Memory", vision = "Vision", GDP = "GDP", Calories = "Calories")

# ---------------------------------------------------------------- panel A ----
cells <- readRDS("revision/data/cells.rds")

subj_calib <- cells |>
  group_by(subj, gender) |>
  summarise(pre = mean(calib_pre), post = mean(calib_post), .groups = "drop") |>
  pivot_longer(c(pre, post), names_to = "time", values_to = "calib") |>
  mutate(time    = factor(time, levels = c("pre", "post"), labels = c("Pre", "Post")),
         gender2 = factor(ifelse(gender == "Feminin", "Women", "Men"),
                          levels = c("Women", "Men")))

pA <- ggplot(subj_calib, aes(x = time, y = calib)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30", linewidth = 0.8) +
  geom_line(aes(group = subj), colour = "grey65", alpha = 0.35, linewidth = 0.3) +
  geom_half_boxplot(data = filter(subj_calib, time == "Pre"),
                    aes(fill = gender2), side = "l", width = 0.28, alpha = 0.85,
                    outlier.shape = NA, linewidth = 1) +
  geom_half_boxplot(data = filter(subj_calib, time == "Post"),
                    aes(fill = gender2), side = "r", width = 0.28, alpha = 0.85,
                    outlier.shape = NA, linewidth = 1) +
  facet_wrap(~ gender2) +
  scale_fill_manual(values = GENDER_COLS, guide = "none") +
  # the zero line is labelled on a duplicated right-hand axis rather than with a
  # free-floating annotation, which cannot be positioned on a discrete x scale
  scale_y_continuous(sec.axis = dup_axis(breaks = 0, labels = "calibrated", name = NULL)) +
  labs(x = "Time", y = "Calibration (pp): median, quartiles, 1.5 IQR", tag = "A",
       title = "Global calibration before and after testing") +
  theme_high_impact()

# ---------------------------------------------------------------- panel B ----
eff <- read.csv("revision/data/effects_calibration.csv") |>
  filter(model == "calibration_cellmean") |>
  tidyr::extract(effect, c("domain", "gender2", "time"),
                 "calibration: (\\S+), (\\S+), (\\S+)") |>
  filter(domain != "DOMAIN-GENERAL") |>
  mutate(domain  = factor(DOMAIN_LAB[domain], levels = unname(DOMAIN_LAB)),
         gender2 = factor(gender2, levels = c("Women", "Men")),
         time    = factor(time, levels = c("pre", "post"), labels = c("Pre", "Post")))
stopifnot(nrow(eff) == 16)

dodge <- position_dodge(width = 0.45)
pB <- ggplot(eff, aes(x = domain, y = mean, colour = gender2, group = gender2)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30", linewidth = 0.8) +
  # no lines between domains: the domains are categories, not a series (A. Tyrer)
  geom_errorbar(aes(ymin = hdi_lo, ymax = hdi_hi), width = 0, linewidth = 1.4,
                position = dodge) +
  geom_point(position = dodge, size = 3.2, shape = 21, fill = "white", stroke = 1.4) +
  facet_wrap(~ time) +
  scale_colour_manual(values = GENDER_COLS, name = NULL) +
  scale_y_continuous(sec.axis = dup_axis(breaks = 0, labels = "calibrated", name = NULL)) +
  labs(x = NULL, y = "Calibration (pp): posterior mean, 95% HDI", tag = "B",
       title = "Calibration by task domain and gender") +
  theme_high_impact() +
  theme(legend.position = "top")

# ----------------------------------------------------------------- output ----
fig <- pA / pB
dir.create("revision/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("revision/figures/Figure2.png", fig, dpi = 300, width = 11, height = 11, units = "in")
ggsave("revision/figures/Figure2.pdf", fig,          width = 11, height = 11, units = "in")
cat("wrote revision/figures/Figure2.{png,pdf}\n")
