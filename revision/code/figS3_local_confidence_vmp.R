# Revision Figure S3 -- Figure 3 in the VMP replication sample
# =============================================================================
# The behavioural replication figure: the same two panels as Figure 3, drawn from
# 457 independent participants who did the same GDP and Calories general knowledge task on
# the same 7-point confidence scale. Two domains instead of four, and age as the
# only covariate (the VMP phenotype file records no years of education).
#
# A  subject-mean confidence on correct vs incorrect trials, one thin grey line
#    per participant, half boxplots per gender x accuracy.
# B  per-domain confidence by gender x accuracy, posterior mean and 95% HDI from
#    model 22b.
#
# Inputs   revision/data/cells_vmp.rds              (20b_prep_cells_vmp.R)
#          revision/data/effects_confidence_vmp.csv (22b_local_confidence_vmp.R)
# Outputs  revision/figures/FigureS3_vmp_confidence.{png,pdf}
#          revision/figures/FigureS3_vmp_confidence_caption.txt
#
# Run from anywhere:  Rscript revision/code/figS3_local_confidence_vmp.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

suppressMessages({
  library(ggplot2); library(dplyr); library(tidyr)
  library(gghalves); library(cowplot); library(patchwork)
})
source("code/theme_high_impact.R")

GENDER_COLS <- c(Men = "#0072B2", Women = "#D55E00")   # Okabe-Ito, as in Figures 2-4
DOMAIN_LAB  <- c(GDP = "GDP", Calories = "Calories")

# ---------------------------------------------------------------- panel A ----
cells <- readRDS("revision/data/cells_vmp.rds")

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
       title = "Local confidence on correct and incorrect trials, replication sample") +
  theme_high_impact()

# ---------------------------------------------------------------- panel B ----
eff <- read.csv("revision/data/effects_confidence_vmp.csv") |>
  filter(model == "confidence_vmp_cellmean") |>
  tidyr::extract(effect, c("domain", "gender2", "acc"),
                 "confidence: (\\S+), (\\S+), (\\S+)") |>
  mutate(domain  = factor(DOMAIN_LAB[domain], levels = unname(DOMAIN_LAB)),
         gender2 = factor(gender2, levels = c("Women", "Men")),
         acc     = factor(acc, levels = c("correct", "incorrect"),
                          labels = c("Correct", "Incorrect")))
stopifnot(nrow(eff) == 8)

dodge <- position_dodge(width = 0.45)
pB <- ggplot(eff, aes(x = domain, y = mean, colour = gender2, group = gender2)) +
  geom_errorbar(aes(ymin = hdi_lo, ymax = hdi_hi), width = 0, linewidth = 1.4,
                position = dodge) +
  geom_point(position = dodge, size = 3.2, shape = 21, fill = "white", stroke = 1.4) +
  facet_wrap(~ acc) +
  scale_colour_manual(values = GENDER_COLS, name = NULL) +
  scale_y_continuous(limits = c(2, 6.5), breaks = 2:6) +
  labs(x = NULL, y = "Mean confidence (1-7): posterior mean, 95% HDI", tag = "B",
       title = "Confidence by general knowledge domain, gender and accuracy") +
  theme_high_impact() +
  theme(legend.position = "top")

# ----------------------------------------------------------------- output ----
fig <- pA / pB
dir.create("revision/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("revision/figures/FigureS3_vmp_confidence.png", fig, dpi = 300,
       width = 10, height = 9.5, units = "in")
ggsave("revision/figures/FigureS3_vmp_confidence.pdf", fig,
       width = 10, height = 9.5, units = "in")

writeLines(c(
"Figure S3. Local confidence in the VMP replication sample (457 participants, 289 women /",
"168 men) on the same GDP and Calories general knowledge task and the same 7-point confidence scale",
"as the first edition. This is the replication counterpart of Figure 3.",
"",
"(A) Mean confidence on correct and on incorrect trials, averaged over the two domains.",
"Each grey line is one participant; half boxplots show the median, quartiles and 1.5 IQR",
"whiskers per gender and accuracy.",
"",
"(B) Model-implied mean confidence per domain, gender and trial accuracy, posterior mean",
"with the 95% highest-density interval, from",
"",
"    confidence ~ 0 + domain + domain:gender + domain:accuracy + domain:gender:accuracy",
"                 + task accuracy + age + (1 | participant),",
"    sigma      ~ 0 + domain:accuracy,",
"",
"with gender sum-coded (+0.5 women / -0.5 men), trial accuracy sum-coded (+0.5 correct /",
"-0.5 incorrect), domain cell-means coded, and task accuracy the participant's overall",
"accuracy in that domain, z-scored within domain. Priors: cell means and covariates",
"normal(0, SD(DV)), contrast coefficients (anything on gender or trial accuracy)",
"normal(0, 0.5 x SD(DV)), sd normal(0, SD(DV)), sigma student_t(3, 0, SD(DV)).",
"The model is model 22 with two forced changes: two domains",
"instead of four, and no years-of-education covariate, which the VMP phenotype file does",
"not record."),
  "revision/figures/FigureS3_vmp_confidence_caption.txt")

cat("wrote revision/figures/FigureS3_vmp_confidence.{png,pdf} and its caption\n")
