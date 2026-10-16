# Revision analysis 04 -- posterior predictive checks ---------------------
# =========================================================================
# Does the fitted model reproduce the observed data, per gender x modality?
# Three views + a numeric table:
#   1. Joint-response distribution  (the 2K categories the multinomial models)
#   2. Type-2 ROC                   (the metacognition signal: conf | correct vs incorrect)
#   3. Confidence-by-accuracy       (confidence distribution split by correct/incorrect)
#   4. Numeric PPC table            (observed vs predicted type-1 accuracy & mean confidence,
#                                    with 95% CrI + interval coverage)
#
# Predictions are made per subject (including their random effects) and integrated
# over subjects within each gender x modality cell -> a faithful group-level PPC.
#
# The check that matters for a GENDER claim is not overall fit but whether misfit is
# gender-specific: if confidence-scale usage leaked into log M, the model will miss the
# confident bins in OPPOSITE directions for men and women. An aggregate PPC looks fine
# and hides exactly that. Everything below is split by gender x domain.
#
# Input  : a fit from 03_fit_unified.R + the trial data it was fitted to
# Output : revision/figures/ppc/<tag>/*.png
#          revision/data/ppc_summary_<tag>.csv   accuracy and mean confidence per cell
#          revision/data/ppc_bins_<tag>.csv      observed - predicted per confidence bin,
#                                                which is what answers the gender question
#
# Run from anywhere:
#   Rscript revision/code/04_posterior_predictive.R \
#     --fit revision/models/fit_firstedition_fixed.rds \
#     --data revision/data/hmetad_trial.rds --tag firstedition_fixed [--ndraws 300]

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

suppressMessages({
  library(dplyr); library(tidyr); library(purrr); library(ggplot2)
  library(tidybayes); library(brms); library(hmetad)
})

source("revision/code/helpers.R")

MODEL  <- getarg("--fit",  "revision/models/fit_firstedition_fixed.rds")
DATA   <- getarg("--data", "revision/data/hmetad_trial.rds")
NDRAWS <- as.integer(getarg("--ndraws", "300"))
TAG    <- getarg("--tag", sub("\\.rds$", "", basename(MODEL)))
OUTDIR <- file.path("revision/figures/ppc", TAG)
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

stopifnot(file.exists(MODEL), file.exists(DATA))
m   <- readRDS(MODEL)
fam <- fitted_confidence_model(m)
K   <- fam$K                       # read K off the fit, never hardcode
cat(sprintf("model: %s (K = %d, %s criteria) | data: %s | %d draws | tag: %s\n",
            MODEL, K, fam$confidence_model, DATA, NDRAWS, TAG))

# -- observed trial data ----------------------------------------------------
d <- readRDS(DATA) |>
  mutate(correct        = as.integer(response == stimulus),
         joint_response = joint_response(response, confidence, K))

# A pilot fit holds a subset of subjects; predicting for the rest would error on new
# levels. Restrict the check to whoever is actually in the fit.
fit_subj <- unique(as.character(m$data$subj))
if (!all(levels(d$subj) %in% fit_subj)) {
  cat(sprintf("data has %d subjects, fit has %d -- restricting the PPC to the fitted subjects\n",
              nlevels(d$subj), length(fit_subj)))
  d <- droplevels(d[as.character(d$subj) %in% fit_subj, ])
}

# subject-level newdata. It must carry every predictor the model uses -- gender is
# only in the model through the sum-coded fem_c, so fem_c has to be here too.
nd <- d |> distinct(subj, gender, fem_c, mod) |> arrange(subj, mod)
wts <- d |> count(subj, gender, mod, name = "trials")     # trials per subject-cell
nd  <- nd |> left_join(wts, by = c("subj", "gender", "mod")) |>
  mutate(.row = row_number())
# hmetad's *_draws helpers carry EVERY newdata column through, `trials` included, so
# nothing needs joining back on afterwards -- a join would only produce .x/.y suffixes
# and lose the columns everything below groups by. They also return a GROUPED tibble;
# ungroup() before doing anything else.

cells <- d |> distinct(gender, mod)
cat("Cells (gender x mod):", nrow(cells), " subjects:", nrow(nd), "\n")

# expected category probabilities per subject x stimulus x joint_response x draw
# (computed ONCE and reused across the epred-based checks below)
cat("Computing epred draws (once) ...\n")
ep <- epred_draws_metad(m, nd, ndraws = NDRAWS) |> ungroup()

# ===========================================================================
# 1. JOINT-RESPONSE DISTRIBUTION PPC
# ===========================================================================
cat("\n[1/4] joint-response distribution ...\n")

obs_jr <- d |>
  count(gender, mod, stimulus, joint_response, name = "n") |>
  complete(nesting(gender, mod), stimulus, joint_response = 1:(2 * K),
           fill = list(n = 0)) |>
  group_by(gender, mod, stimulus) |>
  mutate(p_obs = n / sum(n)) |> ungroup()

# predicted: per-subject epred -> weight by trials -> group mean per draw
pred_jr <- ep |>
  group_by(gender, mod, stimulus, joint_response, .draw) |>
  summarise(.epred = weighted.mean(.epred, trials), .groups = "drop") |>
  group_by(gender, mod, stimulus, joint_response) |>
  median_qi(.epred, .width = .95) |> ungroup()

jr <- obs_jr |>
  left_join(pred_jr, by = c("gender", "mod", "stimulus", "joint_response"))

p1 <- ggplot(jr, aes(x = factor(joint_response))) +
  geom_col(aes(y = p_obs, fill = factor(stimulus)),
           position = position_dodge(.9), alpha = .55) +
  geom_pointrange(aes(y = .epred, ymin = .lower, ymax = .upper,
                      group = factor(stimulus)),
                  position = position_dodge(.9), size = .2, fatten = 1.2) +
  facet_grid(gender ~ mod) +
  scale_fill_manual("Stimulus", values = c("0" = "#4C72B0", "1" = "#DD8452")) +
  labs(x = "Joint response (1 = confident '0' ... 2K = confident '1')",
       y = "Probability",
       title = "PPC: joint-response distribution",
       subtitle = "bars = observed; points+bars = posterior predictive (95% CrI)") +
  theme_bw(11) + theme(panel.grid = element_blank(),
                       axis.text.x = element_text(size = 6))
ggsave(file.path(OUTDIR, paste0(TAG, "_ppc_joint_response.png")), p1, width = 12, height = 6, dpi = 150)

# ===========================================================================
# 2. TYPE-2 ROC PPC  (metacognition)
# ===========================================================================
cat("[2/4] type-2 ROC ...\n")

# observed type-2 ROC per gender x mod (collapsed over response)
obs_roc2 <- d |>
  group_by(gender, mod, correct) |>
  count(confidence) |>
  complete(confidence = 1:K, fill = list(n = 0)) |>
  mutate(p = n / sum(n)) |>
  select(-n) |> pivot_wider(names_from = correct, values_from = p,
                            names_prefix = "corr") |>
  group_by(gender, mod) |> arrange(desc(confidence), .by_group = TRUE) |>
  mutate(p_hit2 = cumsum(corr1), p_fa2 = cumsum(corr0)) |> ungroup()

# predicted type-2 ROC: per-subject roc2 draws -> average responses -> group mean
pred_roc2 <- roc2_draws(m, nd, ndraws = NDRAWS) |> ungroup() |>
  mutate(confidence = ifelse(response == 0, confidence - 1L, confidence)) |>
  group_by(gender, mod, subj, trials, .draw, confidence) |>
  summarise(p_hit2 = mean(p_hit2), p_fa2 = mean(p_fa2), .groups = "drop") |>
  group_by(gender, mod, .draw, confidence) |>
  summarise(p_hit2 = weighted.mean(p_hit2, trials),
            p_fa2  = weighted.mean(p_fa2,  trials), .groups = "drop") |>
  group_by(gender, mod, confidence) |>
  median_qi(p_hit2, p_fa2, .width = .95) |> ungroup()

p2 <- ggplot(pred_roc2, aes(x = p_fa2, y = p_hit2)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
  geom_ribbon(aes(ymin = p_hit2.lower, ymax = p_hit2.upper), alpha = .2, fill = "#C44E52") +
  geom_line(colour = "#C44E52") +
  geom_point(data = obs_roc2, aes(x = p_fa2, y = p_hit2), inherit.aes = FALSE, size = 1) +
  geom_line(data = obs_roc2, aes(x = p_fa2, y = p_hit2), inherit.aes = FALSE) +
  facet_grid(gender ~ mod) + coord_fixed(xlim = 0:1, ylim = 0:1) +
  labs(x = "P(confidence ≥ k | incorrect)", y = "P(confidence ≥ k | correct)",
       title = "PPC: type-2 ROC (metacognitive sensitivity)",
       subtitle = "points/line = observed; band = posterior predictive (95% CrI)") +
  theme_bw(11) + theme(panel.grid = element_blank())
ggsave(file.path(OUTDIR, paste0(TAG, "_ppc_type2_roc.png")), p2, width = 12, height = 6, dpi = 150)

# ===========================================================================
# 3. CONFIDENCE-BY-ACCURACY PPC
# ===========================================================================
cat("[3/4] confidence-by-accuracy ...\n")

obs_conf <- d |>
  group_by(gender, mod, correct) |>
  count(confidence) |>
  complete(confidence = 1:K, fill = list(n = 0)) |>
  mutate(p_obs = n / sum(n)) |> ungroup()

# predicted confidence | correctness from epred: map joint_response -> correct + confidence
pred_conf <- ep |>
  mutate(correct = as.integer(response == stimulus)) |>
  group_by(gender, mod, subj, trials, correct, confidence, .draw) |>
  summarise(.epred = sum(.epred), .groups = "drop") |>              # over the two stimuli
  group_by(gender, mod, subj, trials, correct, .draw) |>
  mutate(.epred = .epred / sum(.epred)) |>                          # normalise within correctness
  group_by(gender, mod, correct, confidence, .draw) |>
  summarise(.epred = weighted.mean(.epred, trials), .groups = "drop") |>
  group_by(gender, mod, correct, confidence) |>
  median_qi(.epred, .width = .95) |> ungroup()

conf <- obs_conf |> left_join(pred_conf,
          by = c("gender", "mod", "correct", "confidence")) |>
  mutate(resid = p_obs - .epred,
         in_CrI = p_obs >= .lower & p_obs <= .upper)
write.csv(conf, sprintf("revision/data/ppc_bins_%s.csv", TAG), row.names = FALSE)

# THE CHECK THAT MATTERS FOR A GENDER CLAIM. If confidence-scale usage had leaked into
# log M, the model would miss the confident bins in OPPOSITE directions for men and
# women. Same-signed residuals mean the misfit is a property of the likelihood, not of
# the gender contrast.
top <- conf |> filter(confidence >= K - 1) |>
  select(gender, mod, correct, confidence, p_obs, .epred, resid) |>
  pivot_wider(names_from = gender, values_from = c(p_obs, .epred, resid)) |>
  mutate(opposite_sign = sign(resid_Masculin) != sign(resid_Feminin))
cat("\n=== residuals (observed - predicted) in the top two confidence bins ===\n")
print(as.data.frame(top |> select(mod, correct, confidence,
                                  resid_Masculin, resid_Feminin, opposite_sign)),
      row.names = FALSE, digits = 3)
cat(sprintf("\ngender-specific misfit: %d of %d top-bin cells have OPPOSITE-signed residuals\n",
            sum(top$opposite_sign), nrow(top)))
cat(sprintf("bins inside the 95%% CrI: %d of %d (all bins, all cells)\n",
            sum(conf$in_CrI), nrow(conf)))

p3 <- ggplot(conf, aes(x = factor(confidence))) +
  geom_col(aes(y = p_obs, fill = factor(correct)),
           position = position_dodge(.9), alpha = .55) +
  geom_pointrange(aes(y = .epred, ymin = .lower, ymax = .upper,
                      group = factor(correct)),
                  position = position_dodge(.9), size = .2, fatten = 1.2) +
  facet_grid(gender ~ mod) +
  scale_fill_manual("Correct", values = c("0" = "#DD8452", "1" = "#55A868")) +
  labs(x = "Confidence", y = "Probability",
       title = "PPC: confidence distribution by accuracy",
       subtitle = "bars = observed; points+bars = posterior predictive (95% CrI)") +
  theme_bw(11) + theme(panel.grid = element_blank())
ggsave(file.path(OUTDIR, paste0(TAG, "_ppc_confidence_by_accuracy.png")), p3, width = 12, height = 6, dpi = 150)

# ===========================================================================
# 4. NUMERIC PPC TABLE: accuracy & mean confidence, observed vs predicted
# ===========================================================================
cat("[4/4] numeric summaries ...\n")

obs_stat <- d |> group_by(gender, mod) |>
  summarise(acc_obs = mean(correct), conf_obs = mean(confidence), .groups = "drop")

# predicted per-draw accuracy & mean confidence per cell (subject-weighted; reuses `ep`)
pred_acc <- ep |>
  mutate(correct = as.integer(response == stimulus)) |>
  group_by(gender, mod, subj, trials, .draw) |>
  summarise(acc = sum(.epred[correct == 1]) / sum(.epred), .groups = "drop") |>
  group_by(gender, mod, .draw) |>
  summarise(acc = weighted.mean(acc, trials), .groups = "drop") |>
  group_by(gender, mod) |>
  median_qi(acc, .width = .95) |> ungroup() |>
  # ggdist names the bounds `.lower`/`.upper` when a single column is summarised, so
  # rename here -- the two summaries are joined below and would otherwise collide.
  select(gender, mod, acc, acc_lo = .lower, acc_hi = .upper)

pred_conf_mean <- ep |>
  group_by(gender, mod, subj, trials, .draw) |>
  summarise(mc = sum(.epred * confidence) / sum(.epred), .groups = "drop") |>
  group_by(gender, mod, .draw) |>
  summarise(mc = weighted.mean(mc, trials), .groups = "drop") |>
  group_by(gender, mod) |>
  median_qi(mc, .width = .95) |> ungroup() |>
  select(gender, mod, mc, mc_lo = .lower, mc_hi = .upper)

tab <- obs_stat |>
  left_join(pred_acc,       by = c("gender", "mod")) |>
  left_join(pred_conf_mean, by = c("gender", "mod")) |>
  mutate(acc_in_CrI  = acc_obs  >= acc_lo & acc_obs  <= acc_hi,
         conf_in_CrI = conf_obs >= mc_lo  & conf_obs <= mc_hi) |>
  transmute(gender, mod,
            acc_obs = round(acc_obs, 3),
            acc_pred = round(acc, 3), acc_lo = round(acc_lo, 3),
            acc_hi = round(acc_hi, 3), acc_in_CrI,
            conf_obs = round(conf_obs, 2),
            conf_pred = round(mc, 2), conf_lo = round(mc_lo, 2),
            conf_hi = round(mc_hi, 2), conf_in_CrI)

write.csv(tab, sprintf("revision/data/ppc_summary_%s.csv", TAG), row.names = FALSE)
cat("\n=== PPC numeric summary (observed vs predicted, 95% CrI) ===\n")
print(as.data.frame(tab))
cat(sprintf("\nCoverage: accuracy %d/%d cells, mean-confidence %d/%d cells inside 95%% CrI\n",
            sum(tab$acc_in_CrI), nrow(tab), sum(tab$conf_in_CrI), nrow(tab)))
cat(sprintf("\nSaved:\n  %s/%s_ppc_joint_response.png\n  %s/%s_ppc_type2_roc.png\n  %s/%s_ppc_confidence_by_accuracy.png\n  revision/data/ppc_summary_%s.csv\n  revision/data/ppc_bins_%s.csv\n",
            OUTDIR, TAG, OUTDIR, TAG, OUTDIR, TAG, TAG, TAG))
