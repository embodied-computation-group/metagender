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
# Input  : revision/models/fit_full_maximal.rds (run 03_fit_model.R first)
#          revision/data/hmetad_trial.rds
# Output : revision/figures/ppc/*.png, revision/data/ppc_summary.csv
#
# Run from anywhere:  Rscript revision/code/04_posterior_predictive.R [ndraws]
#   ndraws: posterior draws to use (default 300; lower = faster/lighter).

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

suppressMessages({
  library(dplyr); library(tidyr); library(purrr); library(ggplot2)
  library(tidybayes); library(brms); library(hmetad)
})

args   <- commandArgs(trailingOnly = TRUE)
NDRAWS <- if (length(args) >= 1) as.integer(args[1]) else 300L
K      <- 7L
MODEL  <- "revision/models/fit_full_maximal.rds"
OUTDIR <- "revision/figures/ppc"
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

stopifnot(file.exists(MODEL))
m <- readRDS(MODEL)
cat("Loaded model:", MODEL, "| using", NDRAWS, "posterior draws\n")

# -- observed trial data ----------------------------------------------------
d <- readRDS("revision/data/hmetad_trial.rds") |>
  mutate(correct        = as.integer(response == stimulus),
         joint_response = joint_response(response, confidence, K))

# subject-level newdata (carries gender, mod) + per-cell trial weights
nd <- d |> distinct(subj, gender, mod) |> arrange(subj, mod)
wts <- d |> count(subj, gender, mod, name = "trials")     # trials per subject-cell
nd  <- nd |> left_join(wts, by = c("subj", "gender", "mod")) |>
  mutate(.row = row_number())
key <- nd |> select(.row, subj, gender, mod, trials)

cells <- d |> distinct(gender, mod)
cat("Cells (gender x mod):", nrow(cells), " subjects:", nrow(nd), "\n")

# expected category probabilities per subject x stimulus x joint_response x draw
# (computed ONCE and reused across the epred-based checks below)
cat("Computing epred draws (once) ...\n")
ep <- epred_draws_metad(m, nd, ndraws = NDRAWS) |> left_join(key, by = ".row")

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
ggsave(file.path(OUTDIR, "ppc_joint_response.png"), p1, width = 12, height = 6, dpi = 150)

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
pred_roc2 <- roc2_draws(m, nd, ndraws = NDRAWS) |>
  left_join(key, by = ".row") |>
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
ggsave(file.path(OUTDIR, "ppc_type2_roc.png"), p2, width = 12, height = 6, dpi = 150)

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
          by = c("gender", "mod", "correct", "confidence"))

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
ggsave(file.path(OUTDIR, "ppc_confidence_by_accuracy.png"), p3, width = 12, height = 6, dpi = 150)

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
  median_qi(acc, .width = .95) |> ungroup()

pred_conf_mean <- ep |>
  group_by(gender, mod, subj, trials, .draw) |>
  summarise(mc = sum(.epred * confidence) / sum(.epred), .groups = "drop") |>
  group_by(gender, mod, .draw) |>
  summarise(mc = weighted.mean(mc, trials), .groups = "drop") |>
  group_by(gender, mod) |>
  median_qi(mc, .width = .95) |> ungroup()

tab <- obs_stat |>
  left_join(pred_acc,       by = c("gender", "mod")) |>
  left_join(pred_conf_mean, by = c("gender", "mod")) |>
  mutate(acc_in_CrI  = acc_obs  >= acc.lower & acc_obs  <= acc.upper,
         conf_in_CrI = conf_obs >= mc.lower  & conf_obs <= mc.upper) |>
  transmute(gender, mod,
            acc_obs = round(acc_obs, 3),
            acc_pred = round(acc, 3), acc_lo = round(acc.lower, 3),
            acc_hi = round(acc.upper, 3), acc_in_CrI,
            conf_obs = round(conf_obs, 2),
            conf_pred = round(mc, 2), conf_lo = round(mc.lower, 2),
            conf_hi = round(mc.upper, 2), conf_in_CrI)

write.csv(tab, "revision/data/ppc_summary.csv", row.names = FALSE)
cat("\n=== PPC numeric summary (observed vs predicted, 95% CrI) ===\n")
print(as.data.frame(tab))
cat(sprintf("\nCoverage: accuracy %d/%d cells, mean-confidence %d/%d cells inside 95%% CrI\n",
            sum(tab$acc_in_CrI), nrow(tab), sum(tab$conf_in_CrI), nrow(tab)))
cat("\nSaved:\n  revision/figures/ppc/ppc_joint_response.png",
    "\n  revision/figures/ppc/ppc_type2_roc.png",
    "\n  revision/figures/ppc/ppc_confidence_by_accuracy.png",
    "\n  revision/data/ppc_summary.csv\n")
