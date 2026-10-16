# Revision analysis 01 -- prepare the first-edition trial data
# ===========================================================================
# Produces the trial-level frame `hmetad::fit_metad` expects -- `stimulus`,
# `response`, `confidence` (1:K) -- plus grouping columns `subj`, `gender`,
# `mod` and the sum-coded gender contrast `fem_c`.
#
# Input  : data/Hmetad_data.csv            (first-edition raw data, unchanged)
# Output : revision/data/hmetad_trial.rds
#          revision/data/cell_qc.csv       (every subject x domain cell, kept or not)
#
# Run from anywhere:  Rscript revision/code/01_prep_data.R
#                     Rscript revision/code/01_prep_data.R --no-qc
#
# ---------------------------------------------------------------------------
# fem_c = +0.5 Feminin / -0.5 Masculin. The model (03_fit_unified.R) is cell-means
# coded on domain, so gender must be sum-coded: with a 0/1 dummy the domain cell
# means would silently become the reference gender's means. Sign convention is
# Feminin - Masculin throughout, matching the published Figure 4 direction.
#
# QC. The log link on the M-ratio cannot represent d' <= 0: a cell at or below
# chance is a disengaged subject, not a metacognition measurement, and it will not
# fit -- it shows up as divergences that no amount of adapt_delta will remove. We
# drop cells with d' <= 0 or fewer than MIN_TRIALS trials. The SAME rule is applied
# to the VMP sample (01b_prep_vmp.R) so the two fits are QC-matched. Drop counts are
# reported BY GENDER: if a rule removed a much larger share of one gender's cells,
# the contrast would be selected on, and no modelling downstream could fix that.
# ---------------------------------------------------------------------------

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

suppressMessages({ library(dplyr) })

args       <- commandArgs(trailingOnly = TRUE)
USE_QC     <- !("--no-qc" %in% args)
MIN_TRIALS <- 30L
K          <- 7L

# d' with the standard 1/(2N) clip. NA when a cell holds only one stimulus class --
# d' is undefined there, which is a different fact about the cell than "d' is low".
dprime_cell <- function(stimulus, response) {
  n1 <- sum(stimulus == 1); n0 <- sum(stimulus == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  hr <- min(max(sum(stimulus == 1 & response == 1) / n1, 1 / (2 * n1)), 1 - 1 / (2 * n1))
  fa <- min(max(sum(stimulus == 0 & response == 1) / n0, 1 / (2 * n0)), 1 - 1 / (2 * n0))
  qnorm(hr) - qnorm(fa)
}

d <- read.csv("data/Hmetad_data.csv") |>
  transmute(
    subj       = factor(subj),
    gender     = factor(gender),                     # Feminin / Masculin
    mod        = factor(mod),                        # memory / vision / GDP / Calories
    trial      = trial,
    stimulus   = as.integer(signal),
    response   = as.integer(response),
    confidence = as.integer(confidence),
    dataset    = factor("first_edition", levels = c("first_edition", "vmp"))
  ) |>
  filter(!is.na(confidence), !is.na(stimulus), !is.na(response))   # drop missed trials

stopifnot(all(d$stimulus %in% c(0, 1)), all(d$response %in% c(0, 1)),
          all(d$confidence %in% 1:K))

# Level order fixes the sign of every contrast downstream.
d$gender <- factor(d$gender, levels = c("Masculin", "Feminin"))
d$mod    <- relevel(d$mod, ref = "memory")

# ------------------------------------------------------------------- QC ----
n_cells_0 <- nrow(distinct(d, subj, mod))
qc <- d |>
  group_by(subj, gender, mod) |>
  summarise(n_trials = n(), dprime = dprime_cell(stimulus, response), .groups = "drop") |>
  mutate(drop_reason = case_when(
           n_trials < MIN_TRIALS       ~ "thin",
           is.na(dprime) | dprime <= 0 ~ "dprime",
           TRUE                        ~ ""),
         drop = drop_reason != "")
write.csv(qc, "revision/data/cell_qc.csv", row.names = FALSE)

cat("=== cell QC ===\n")
cat(sprintf("  %d of %d cells dropped (%s)\n", sum(qc$drop), nrow(qc),
            if (any(qc$drop))
              paste(sprintf("%s=%d", names(table(qc$drop_reason[qc$drop])),
                            table(qc$drop_reason[qc$drop])), collapse = " ") else "none"))
cat("  drop rate by gender:\n")
print(as.data.frame(qc |> group_by(gender) |>
        summarise(cells = n(), dropped = sum(drop), pct = 100 * mean(drop), .groups = "drop")),
      row.names = FALSE, digits = 3)
cat("  drop rate by gender x domain:\n")
print(as.data.frame(qc |> group_by(gender, mod) |>
        summarise(cells = n(), dropped = sum(drop), .groups = "drop")), row.names = FALSE)

if (USE_QC) {
  d <- d |> semi_join(filter(qc, !drop), by = c("subj", "mod"))
  d$subj <- droplevels(d$subj)
} else {
  message("QC DISABLED (--no-qc): cells kept regardless of d' and trial count")
}

# --------------------------------------------------------------- output ----
d$fem_c <- ifelse(d$gender == "Feminin", 0.5, -0.5)

dir.create("revision/data", showWarnings = FALSE, recursive = TRUE)
saveRDS(d, "revision/data/hmetad_trial.rds")

cat("\nPrepared first-edition sample -> revision/data/hmetad_trial.rds\n")
cat("  trials    :", nrow(d), "\n")
cat("  subjects  :", nlevels(d$subj), "\n")
cat("  cells     :", nrow(distinct(d, subj, mod)), "of", n_cells_0, "before QC\n")
cat("  K (levels):", max(d$confidence), "\n")
cat("  gender    :", paste(levels(d$gender), collapse = ", "),
    " (fem_c = +0.5 Feminin / -0.5 Masculin)\n")
cat("  domains   :", paste(levels(d$mod), collapse = ", "), "\n\n")
print(table(distinct(d, subj, gender)$gender))
cat("\naccuracy and mean confidence by gender x domain (the raw signal):\n")
print(as.data.frame(d |> group_by(gender, mod) |>
        summarise(n = n(), accuracy = mean(stimulus == response),
                  mean_conf = mean(confidence), .groups = "drop")),
      row.names = FALSE, digits = 3)
