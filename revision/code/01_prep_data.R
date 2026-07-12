# Revision analysis 01 -- prepare trial-level data for the brms hmetad model
# =========================================================================
# Produces a clean trial-level data frame in the format `hmetad::fit_metad`
# expects: columns `stimulus`, `response`, `confidence` (1:K), plus grouping
# columns `subj`, `gender`, `mod`.
#
# Replaces the deprecated MATLAB HMeta-d workflow (separate male/female group
# fits per domain, subtracting mu_logMratio posteriors). Here gender enters a
# single hierarchical model as a predictor.
#
# Input  : data/Hmetad_data.csv          (first-edition raw data, unchanged)
# Output : revision/data/hmetad_trial.rds
#
# Run from anywhere:  Rscript revision/code/01_prep_data.R

# --- resolve repo root so the script runs from any working directory --------
.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

suppressMessages(library(dplyr))

d <- read.csv("data/Hmetad_data.csv")

d_clean <- d |>
  transmute(
    subj       = factor(subj),
    gender     = factor(gender),                    # Feminin / Masculin
    mod        = factor(mod),                        # memory / vision / GDP / Calories
    trial      = trial,
    stimulus   = as.integer(signal),                 # 0 / 1  -> hmetad `stimulus`
    response   = as.integer(response),               # 0 / 1  -> hmetad `response`
    confidence = as.integer(confidence)              # 1:7    -> hmetad `confidence`
  ) |>
  filter(!is.na(confidence), !is.na(stimulus), !is.na(response))  # drop missed trials

stopifnot(
  all(d_clean$stimulus %in% c(0, 1)),
  all(d_clean$response %in% c(0, 1)),
  all(d_clean$confidence %in% 1:7)
)

# reference levels: Masculin & memory as baseline, so the gender coefficient is
# Feminin - Masculin (matches the old Female - Male contrast direction, Fig 4)
d_clean$gender <- relevel(d_clean$gender, ref = "Masculin")
d_clean$mod    <- relevel(d_clean$mod,    ref = "memory")

dir.create("revision/data", showWarnings = FALSE, recursive = TRUE)
saveRDS(d_clean, "revision/data/hmetad_trial.rds")

cat("Prepared dataset -> revision/data/hmetad_trial.rds\n")
cat("  trials    :", nrow(d_clean), "\n")
cat("  subjects  :", nlevels(d_clean$subj), "\n")
cat("  K (levels):", max(d_clean$confidence), "\n")
cat("  gender    :", paste(levels(d_clean$gender), collapse = " (ref), "), "\n")
cat("  modality  :", paste(levels(d_clean$mod), collapse = " (ref), "), "\n")
print(d_clean |> count(gender, mod))
