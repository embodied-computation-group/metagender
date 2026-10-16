# Revision analysis 01b -- prepare the VMP replication sample
# ===========================================================================
# The Visceral Mind Project (Aarhus) ran the SAME two semantic-trivia domains as the
# first edition -- GDP and Calories -- in an independent sample of 456 subjects on the
# same 7-point confidence scale. It is therefore a direct replication sample for the
# trivia half of this study, at 1.4x the subject count.
#
#   first edition   318 subjects (146 Masculin / 172 Feminin)   memory, vision, GDP, Calories
#   VMP             456 subjects (168 Masculin / 288 Feminin)   GDP, Calories
#
# Input  : revision/data/vmp/trivia_trials.tsv   trial-level, from the VMP derivatives
#          revision/data/vmp/demographics.tsv    BIDS phenotype/demographics
# Output : revision/data/hmetad_trial_vmp.rds    same schema as hmetad_trial.rds
#
# Run from anywhere:  Rscript revision/code/01b_prep_vmp.R
#
# ---------------------------------------------------------------------------
# CONFIDENCE IS KEPT ON ITS NATIVE 7-POINT SCALE. The VMP trivia task records confidence
# as k/6 in [0,1], i.e. exactly 7 levels, and the sibling meta_intero_brain project only
# ever collapsed it to 4 bins in order to make trivia commensurate with its cardiac and
# respiratory tasks. Nothing here needs that: K = 7 matches the first edition exactly,
# which is what makes the two fits comparable. Do not bin.
#
# STIMULUS IS RECONSTRUCTED, NOT RECORDED. The VMP task stores which option was *chosen*
# and whether that was correct; stimulus identity is recovered as `response` on correct
# trials and its complement on errors. For a 2AFC this reconstruction is exact (not an
# approximation): stimulus is a deterministic function of (response, accuracy). The S1/S2
# labelling is arbitrary but internally consistent, which is all SDT requires. The first
# edition records `signal` directly. Both are valid; the asymmetry is recorded here so
# nobody later mistakes it for a bug.
#
# EXCLUSIONS. The log link on M-ratio cannot represent d' < 0: a cell at or below chance
# is a disengaged subject, not a metacognition measurement, and it will not fit. We drop
# cells with d' <= 0 or fewer than MIN_TRIALS trials and report exactly how many. The SAME
# rule is applied to the first-edition sample (01_prep_data.R), so the two fits are
# QC-matched. Pass --no-qc to disable.
# ---------------------------------------------------------------------------

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

suppressMessages({ library(dplyr); library(tidyr) })

args      <- commandArgs(trailingOnly = TRUE)
USE_QC    <- !("--no-qc" %in% args)
MIN_TRIALS <- 30L
K          <- 7L

# ---------------------------------------------------------------------------
# THE MODALITY REGISTRY -- the hook for interoception.
#
# VMP also ran cardiac (HRD: intero + extero) and respiratory (RRST) metacognition on the
# same subjects. We are not using them: this revision is about the trivia domains that the
# first edition actually measured. But adding them later should be a matter of one entry
# here plus one loader, not a rewrite of this file, so the prep is written modality-agnostic.
#
# BEFORE ADDING INTERO, READ THIS. HRD-exteroceptive and RRST are both driven by adaptive
# STAIRCASES. In the sibling project we found that a staircase makes confidence track
# trial-wise difficulty rather than internal evidence alone, which inflates meta-d',
# collapses the between-subject variance in log M, and destroys its correlations with other
# domains. Trivia is not staircased. The four domains would therefore NOT be on equal
# footing, and a "domain-general" gender effect estimated across them would be partly an
# artifact of task design. That is a scientific decision, not a plumbing one.
MODALITIES <- list(
  GDP      = list(source = "vmp_trivia", condition = 0L),
  Calories = list(source = "vmp_trivia", condition = 1L)
  # hrd_intero = list(source = "vmp_hrd",  modality = "Intero"),   # <- staircased? no (intero)
  # hrd_extero = list(source = "vmp_hrd",  modality = "Extero"),   # <- STAIRCASED
  # rrst       = list(source = "vmp_rrst")                         # <- STAIRCASED
)

# d' with the standard 1/(2N) clip. NA when a cell holds only one stimulus class -- d' is
# undefined there, which is a different fact about the cell than "d' is low".
dprime_cell <- function(stimulus, response) {
  n1 <- sum(stimulus == 1); n0 <- sum(stimulus == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  hr <- min(max(sum(stimulus == 1 & response == 1) / n1, 1 / (2 * n1)), 1 - 1 / (2 * n1))
  fa <- min(max(sum(stimulus == 0 & response == 1) / n0, 1 / (2 * n0)), 1 - 1 / (2 * n0))
  qnorm(hr) - qnorm(fa)
}

# --------------------------------------------------------------- load ----
trials <- read.delim("revision/data/vmp/trivia_trials.tsv")
demo   <- read.delim("revision/data/vmp/demographics.tsv")
stopifnot(!anyDuplicated(demo$participant_id))

# The registry maps VMP's numeric Condition to the first edition's modality labels.
cond_map <- setNames(names(MODALITIES),
                     vapply(MODALITIES, function(m) as.character(m$condition), character(1)))
trials$mod <- unname(cond_map[as.character(trials$Condition)])
stopifnot(!anyNA(trials$mod))

# Gender: BIDS codes 1 = Female, 2 = Male, 3 = Other. Mapped onto the first edition's
# Feminin / Masculin labels so both datasets speak one language.
#
# "Other" (n = 1) is DROPPED. A level with a single subject cannot identify a coefficient;
# it would fit that person's noise and nothing else. Recorded as a decision, not a filter.
trials$gender_code <- demo$gender[match(trials$Subject, demo$participant_id)]
n_other <- length(unique(trials$Subject[trials$gender_code == 3 & !is.na(trials$gender_code)]))
n_na    <- length(unique(trials$Subject[is.na(trials$gender_code)]))
if (n_other) message("dropping ", n_other, " subject(s) with gender = Other (n too small to identify)")
if (n_na)    message("dropping ", n_na, " subject(s) with no demographics row")
trials <- trials[trials$gender_code %in% c(1, 2), ]

d <- trials |>
  transmute(
    subj       = factor(paste0("vmp_", Subject)),   # namespaced: VMP and first-edition IDs must not collide
    gender     = factor(ifelse(gender_code == 1, "Feminin", "Masculin")),
    mod        = factor(mod, levels = names(MODALITIES)),
    stimulus   = as.integer(Stimuli),
    response   = as.integer(Responses),
    confidence = as.integer(Confidence),
    dataset    = factor("vmp", levels = c("first_edition", "vmp"))
  ) |>
  filter(!is.na(confidence), !is.na(stimulus), !is.na(response))

stopifnot(
  all(d$stimulus %in% c(0, 1)),
  all(d$response %in% c(0, 1)),
  all(d$confidence %in% 1:K)
)

# ----------------------------------------------------------------- QC ----
n_cells_0 <- nrow(distinct(d, subj, mod))
if (USE_QC) {
  qc <- d |>
    group_by(subj, gender, mod) |>
    summarise(n_trials = n(), dprime = dprime_cell(stimulus, response), .groups = "drop") |>
    mutate(drop_reason = case_when(
      n_trials < MIN_TRIALS        ~ "thin",
      is.na(dprime) | dprime <= 0  ~ "dprime",   # log link on M cannot represent d' < 0
      TRUE                         ~ ""
    ), drop = drop_reason != "")

  cat(sprintf("QC: %d of %d cells dropped (%s)\n", sum(qc$drop), nrow(qc),
              paste(sprintf("%s=%d", names(table(qc$drop_reason[qc$drop])),
                            table(qc$drop_reason[qc$drop])), collapse = " ")))
  # Drop rate BY GENDER: a rule that removes a much larger share of one gender's cells
  # selects on the contrast, and nothing downstream can fix that.
  cat("  drop rate by gender:\n")
  print(as.data.frame(qc |> group_by(gender) |>
          summarise(cells = n(), dropped = sum(drop), pct = 100 * mean(drop), .groups = "drop")),
        row.names = FALSE, digits = 3)
  d <- d |> semi_join(filter(qc, !drop), by = c("subj", "mod"))
  write.csv(qc, "revision/data/vmp_cell_qc.csv", row.names = FALSE)
} else {
  message("QC DISABLED (--no-qc): matching the first edition's filtering exactly")
}
d$subj <- droplevels(d$subj)

# Level order and the sum-coded contrast match 01_prep_data.R exactly, so the unified
# model (03_fit_unified.R) reads the same quantity out of both samples:
# fem_c = +0.5 Feminin / -0.5 Masculin, i.e. every contrast is Feminin - Masculin,
# the direction of the published Figure 4.
d$gender <- factor(d$gender, levels = c("Masculin", "Feminin"))
d$fem_c  <- ifelse(d$gender == "Feminin", 0.5, -0.5)

dir.create("revision/data", showWarnings = FALSE, recursive = TRUE)
saveRDS(d, "revision/data/hmetad_trial_vmp.rds")

cat("\nPrepared VMP replication sample -> revision/data/hmetad_trial_vmp.rds\n")
cat("  trials    :", nrow(d), "\n")
cat("  subjects  :", nlevels(d$subj), "\n")
cat("  cells     :", nrow(distinct(d, subj, mod)), "of", n_cells_0, "before QC\n")
cat("  K (levels):", max(d$confidence), "  (native 7-point; NOT binned)\n")
cat("  gender    :", paste(levels(d$gender), collapse = ", "),
    " (fem_c = +0.5 Feminin / -0.5 Masculin)\n")
cat("  modality  :", paste(levels(d$mod), collapse = ", "), "\n\n")
print(d |> count(gender, mod))
cat("\nsubjects per gender:\n")
print(table(distinct(d, subj, gender)$gender))
cat("\naccuracy and mean confidence by gender x modality (the raw signal):\n")
print(as.data.frame(d |> group_by(gender, mod) |>
  summarise(accuracy = mean(stimulus == response), mean_conf = mean(confidence), .groups = "drop")),
  row.names = FALSE, digits = 3)
