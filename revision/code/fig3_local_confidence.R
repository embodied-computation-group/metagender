# Revision figure 3 -- local confidence by gender and trial accuracy
# =============================================================================
# Restyle of the first edition's Figure 3. The colour map on the individual
# difference lines is removed (Reviewer 3): the slope of each line already
# carries that information. Panel B is now model-based rather than raw +/-1.96 SE,
# and the underlying model controls for each participant's task accuracy
# (Reviewer 2), so the domain pattern can be read as a confidence effect rather
# than a performance effect.
#
# A  subject-mean confidence on correct vs incorrect trials, one thin grey line
#    per participant, half boxplots per gender x accuracy.
# B  per-domain confidence by gender x accuracy, posterior mean and 95% HDI from
#    model 22 (revision/data/effects_confidence.csv).
#
# Inputs   revision/data/cells.rds
#          revision/data/effects_confidence.csv    (run 22_local_confidence.R first)
# Outputs  revision/figures/Figure3.png, revision/figures/Figure3.pdf
#          revision/figures/Figure3_caption.txt   (the model formula lives here, not in
#          a panel subtitle -- 2026-09-05 figure review)
#
# Run from anywhere:  Rscript revision/code/fig3_local_confidence.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

suppressMessages({
  library(ggplot2); library(dplyr); library(tidyr)
  library(gghalves); library(cowplot); library(patchwork)
})
source("code/theme_high_impact.R")

GENDER_COLS <- c(Men = "#0072B2", Women = "#D55E00")   # Okabe-Ito, as in Figure 2
DOMAIN_LAB  <- c(memory = "Memory", vision = "Vision", GDP = "GDP", Calories = "Calories")

# ---------------------------------------------------------------- panel A ----
cells <- readRDS("revision/data/cells.rds")

subj_conf <- cells |>
  group_by(subj, gender) |>
  summarise(Correct = mean(conf_cor), Incorrect = mean(conf_inc), .groups = "drop") |>
  pivot_longer(c(Correct, Incorrect), names_to = "acc", values_to = "conf") |>
  mutate(acc     = factor(acc, levels = c("Correct", "Incorrect")),
         gender2 = factor(ifelse(gender == "Feminin", "Women", "Men"),
                          levels = c("Women", "Men")))

pA <- ggplot(subj_conf, aes(x = acc, y = conf)) +
  geom_line(aes(group = subj), colour = "grey65", alpha = 0.35, linewidth = 0.3) +
  geom_half_boxplot(data = filter(subj_conf, acc == "Correct"),
                    aes(fill = gender2), side = "l", width = 0.28, alpha = 0.85,
                    outlier.shape = NA, linewidth = 1) +
  geom_half_boxplot(data = filter(subj_conf, acc == "Incorrect"),
                    aes(fill = gender2), side = "r", width = 0.28, alpha = 0.85,
                    outlier.shape = NA, linewidth = 1) +
  facet_wrap(~ gender2) +
  scale_fill_manual(values = GENDER_COLS, guide = "none") +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
  labs(x = "Trial-wise accuracy", y = "Mean confidence (1-7): median, quartiles, 1.5 IQR", tag = "A",
       title = "Local confidence on correct and incorrect trials") +
  theme_high_impact()

# ---------------------------------------------------------------- panel B ----
eff <- read.csv("revision/data/effects_confidence.csv") |>
  filter(model == "confidence_cellmean") |>
  tidyr::extract(effect, c("domain", "gender2", "acc"),
                 "confidence: (\\S+), (\\S+), (\\S+)") |>
  mutate(domain  = factor(DOMAIN_LAB[domain], levels = unname(DOMAIN_LAB)),
         gender2 = factor(gender2, levels = c("Women", "Men")),
         acc     = factor(acc, levels = c("correct", "incorrect"),
                          labels = c("Correct", "Incorrect")))
stopifnot(nrow(eff) == 16)

dodge <- position_dodge(width = 0.45)
pB <- ggplot(eff, aes(x = domain, y = mean, colour = gender2, group = gender2)) +
  # no lines between domains: the domains are categories, not a series (A. Tyrer)
  geom_errorbar(aes(ymin = hdi_lo, ymax = hdi_hi), width = 0, linewidth = 1.4,
                position = dodge) +
  geom_point(position = dodge, size = 3.2, shape = 21, fill = "white", stroke = 1.4) +
  facet_wrap(~ acc) +
  scale_colour_manual(values = GENDER_COLS, name = NULL) +
  scale_y_continuous(limits = c(2, 6.5), breaks = 2:6) +
  labs(x = NULL, y = "Mean confidence (1-7): posterior mean, 95% HDI", tag = "B",
       title = "Confidence by task domain, gender and accuracy") +
  theme_high_impact() +
  theme(legend.position = "top")

# ----------------------------------------------------------------- output ----
fig <- pA / pB
dir.create("revision/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("revision/figures/Figure3.png", fig, dpi = 300, width = 11, height = 11, units = "in")
ggsave("revision/figures/Figure3.pdf", fig,          width = 11, height = 11, units = "in")
writeLines(c(
"Figure 3. Local confidence by gender and trial accuracy (N = 317; 172 women / 145 men).",
"",
"(A) Mean confidence on correct and on incorrect trials, averaged over the four task",
"domains. Each grey line is one participant; half boxplots show the median, quartiles and",
"1.5 IQR whiskers per gender and accuracy. The colour map on the individual lines used in",
"the first edition has been removed (Reviewer 3): the slope of each line already carries",
"that information.",
"",
"(B) Model-implied mean confidence per domain, gender and trial accuracy, posterior mean",
"with the 95% highest-density interval, from",
"",
"    confidence ~ 0 + domain + domain:gender + domain:accuracy + domain:gender:accuracy",
"                 + task accuracy + age + education + (1 | participant),",
"    sigma      ~ 0 + domain:accuracy,",
"",
"with gender sum-coded (+0.5 women / -0.5 men), trial accuracy sum-coded (+0.5 correct /",
"-0.5 incorrect), domain cell-means coded, and task accuracy the participant's overall",
"accuracy in that domain, z-scored within domain (the accuracy control Reviewer 2 asked",
"for). Priors: cell means and covariates normal(0, SD(DV)), contrast coefficients (anything",
"on gender or trial accuracy) normal(0, 0.5 x SD(DV)), sd normal(0, SD(DV)), sigma",
"student_t(3, 0, SD(DV)).",
"",
"Mean confidence is reported here as a DESCRIPTIVE quantity. It is not a metacognitive bias",
"in the normative sense (Reviewer 2); the bias proper is meta-Delta in Figure 4."),
  "revision/figures/Figure3_caption.txt")

cat("wrote revision/figures/Figure3.{png,pdf} and Figure3_caption.txt\n")
