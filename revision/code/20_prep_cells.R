# Revision analysis 20 -- build the subject x domain cell table
# =============================================================================
# One row per participant per task domain, carrying everything the light
# Bayesian models (21-26) and Figures 2-3 need. Nothing is modelled here.
#
# Inputs
#   data/metacognition_TrialData_master.csv   trial level, 4 domains
#   data/self_belief_pre_labels.csv           ";"-separated, demographics + pre
#   data/self_belief_post_labels.csv          ";"-separated, post
#   code/remove_outliers.R                    the first edition's RT filter
#
# Outputs
#   revision/data/cells.rds            subject x domain (long over domain)
#   revision/data/subjects.rds         one row per subject (demographics)
#   revision/data/raw_cell_means.csv   raw means by gender x domain
#
# PREPROCESSING. Identical to the first edition (code/0_Create_dataframe.R) so
# that N and the descriptive means reproduce the submitted paper:
#   1. drop trials with rt <= 0.05 s;
#   2. flag RT and confidence-RT outliers, then drop those trials;
#   3. drop trials with a missing confidence rating;
#   4. drop the one participant not identifying as female or male, and
#      participant 214 (excluded as an outlier in the first edition);
#   5. drop participants with an empty subject x domain x accuracy cell.
#
# NOTE ON STEP 2. code/remove_outliers.R is described in the manuscript as a
# MAD filter but is in fact a Tukey 1.5 x IQR fence applied to the pooled
# trial distribution (all subjects and domains together). We reproduce it
# exactly rather than silently changing the sample; the discrepancy is a
# Methods wording fix, not an analysis change.
#
# Run from anywhere:  Rscript revision/code/20_prep_cells.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

suppressMessages({ library(dplyr); library(tidyr) })

DOMAINS <- c("memory", "vision", "GDP", "Calories")

# ---------------------------------------------------------------- load ----
trial <- read.csv("data/metacognition_TrialData_master.csv")
sb_pre  <- read.table("data/self_belief_pre_labels.csv",  sep = ";", header = TRUE)
sb_post <- read.table("data/self_belief_post_labels.csv", sep = ";", header = TRUE)

# the questionnaire exports contain a handful of duplicate submissions
sb_pre  <- sb_pre[!duplicated(sb_pre$Sid), ]
sb_post <- sb_post[!duplicated(sb_post$Sid), ]

cat("raw: ", nrow(trial), " trials, ", length(unique(trial$subject)), " subjects\n", sep = "")

# ------------------------------------------------------- trial cleaning ----
source("code/remove_outliers.R")
n0 <- nrow(trial)
trial <- dplyr::filter(trial, rt > 0.05)
trial$rt      <- remove_outliers(trial$rt)
trial$rt_conf <- remove_outliers(trial$rt_conf)
trial <- na.omit(trial)
cat("after RT / confidence cleaning: ", nrow(trial), " trials (",
    round(100 * (1 - nrow(trial) / n0), 1), "% removed)\n", sep = "")

# -------------------------------------------------------- demographics ----
subjects <- sb_pre |>
  dplyr::select(subj = Sid, age, gender, edu = years_edu,
                sb_pre_memory   = self_memory_pre,
                sb_pre_vision   = self_visual_pre,
                sb_pre_GDP      = self_gdp_pre,
                sb_pre_Calories = self_calories_pre) |>
  dplyr::inner_join(
    sb_post |> dplyr::select(subj = Sid,
                             sb_post_memory   = self_memory_post,
                             sb_post_vision   = self_visual_post,
                             sb_post_GDP      = self_gdp_post,
                             sb_post_Calories = self_calories_post),
    by = "subj")

n_sb <- nrow(subjects)
subjects <- subjects |>
  dplyr::filter(subj %in% unique(trial$subject)) |>   # must have task data
  dplyr::filter(gender != "non-binary") |>            # 1 participant
  dplyr::filter(subj != 214)                          # first-edition exclusion
cat("self-belief records: ", n_sb, " -> ", nrow(subjects),
    " after task-data / gender / outlier exclusions\n", sep = "")

# ----------------------------------------------- empty-cell exclusions ----
# A subject needs both a correct and an incorrect trial in every domain for the
# accuracy-split models (21-23) to have a complete design.
complete_subj <- trial |>
  dplyr::filter(subject %in% subjects$subj) |>
  dplyr::count(subject, modality, accuracy) |>
  dplyr::group_by(subject) |>
  dplyr::summarise(n_cells = dplyr::n(), .groups = "drop") |>
  dplyr::filter(n_cells == 8) |>
  dplyr::pull(subject)
dropped <- setdiff(subjects$subj, complete_subj)
if (length(dropped))
  cat("dropped for an empty domain x accuracy cell: subject(s) ",
      paste(dropped, collapse = ", "), "\n", sep = "")
subjects <- dplyr::filter(subjects, subj %in% complete_subj)

trial <- dplyr::filter(trial, subject %in% subjects$subj)

# ------------------------------------------------------- coding scheme ----
# Sum coding to +/-0.5 so that a coefficient is the plain Feminin - Masculin
# difference; Masculin is the reference level for display purposes.
subjects <- subjects |>
  dplyr::mutate(
    subj   = factor(subj),
    gender = factor(gender, levels = c("Masculin", "Feminin")),
    fem_c  = ifelse(gender == "Feminin", 0.5, -0.5),
    age_z  = as.numeric(scale(age)),
    edu_z  = as.numeric(scale(edu))
  )

cat("\nFINAL N = ", nrow(subjects), "  (",
    sum(subjects$gender == "Feminin"), " Feminin, ",
    sum(subjects$gender == "Masculin"), " Masculin)\n", sep = "")
cat("submitted manuscript reports 317 (172 F, 145 M): ",
    ifelse(nrow(subjects) == 317, "MATCH", "MISMATCH -- see log above"), "\n", sep = "")

# --------------------------------------------------- task-level summaries ----
task <- trial |>
  dplyr::group_by(subj = subject, domain = modality) |>
  dplyr::summarise(
    n_trials  = dplyr::n(),
    accuracy  = 100 * mean(accuracy),                       # percent, matches self-belief
    conf      = mean(confidence),
    rt        = mean(rt),
    # staircase variability: SD of the absolute stimulus level across trials.
    # Memory used three fixed blockwise levels, so it has no staircase and is NA.
    sc_sd     = ifelse(domain[1] == "memory", NA_real_, stats::sd(abs(contrast))),
    sc_mean   = ifelse(domain[1] == "memory", NA_real_, mean(abs(contrast))),
    .groups   = "drop")

task_acc <- trial |>
  dplyr::group_by(subj = subject, domain = modality, accuracy) |>
  dplyr::summarise(conf = mean(confidence), rt = mean(rt), .groups = "drop") |>
  tidyr::pivot_wider(names_from = accuracy, values_from = c(conf, rt),
                     names_glue = "{.value}_{ifelse(accuracy == 1, 'cor', 'inc')}")

# ----------------------------------------------------- self-belief long ----
sb_long <- subjects |>
  dplyr::select(subj, dplyr::starts_with("sb_")) |>
  tidyr::pivot_longer(-subj, names_to = c("time", "domain"),
                      names_pattern = "sb_(pre|post)_(.*)", values_to = "sb") |>
  tidyr::pivot_wider(names_from = time, values_from = sb,
                     names_prefix = "sb_")

# ------------------------------------------------------------- assemble ----
cells <- subjects |>
  dplyr::select(subj, gender, fem_c, age, edu, age_z, edu_z) |>
  dplyr::mutate(subj = as.character(subj)) |>
  dplyr::left_join(dplyr::mutate(task,     subj = as.character(subj)), by = "subj") |>
  dplyr::left_join(dplyr::mutate(task_acc, subj = as.character(subj)),
                   by = c("subj", "domain")) |>
  dplyr::left_join(sb_long, by = c("subj", "domain")) |>
  dplyr::mutate(
    subj       = factor(subj),
    domain     = factor(domain, levels = DOMAINS),
    calib_pre  = sb_pre  - accuracy,      # >0 over-estimation, <0 under-estimation
    calib_post = sb_post - accuracy
  ) |>
  dplyr::group_by(domain) |>
  dplyr::mutate(
    acc_z   = as.numeric(scale(accuracy)),   # accuracy z-scored WITHIN domain
    sc_sd_z = as.numeric(scale(sc_sd)),      # staircase SD, ditto (units differ per task)
    sc_cv   = sc_sd / sc_mean                # unitless difficulty variability
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(subj, domain)

stopifnot(nrow(cells) == nrow(subjects) * 4, !anyNA(cells$calib_pre), !anyNA(cells$conf))

# ------------------------------------------------------------- raw means ----
raw_means <- cells |>
  dplyr::group_by(gender, domain) |>
  dplyr::summarise(
    n          = dplyr::n(),
    accuracy   = mean(accuracy),
    sb_pre     = mean(sb_pre),
    sb_post    = mean(sb_post),
    calib_pre  = mean(calib_pre),
    calib_post = mean(calib_post),
    conf       = mean(conf),
    conf_cor   = mean(conf_cor),
    conf_inc   = mean(conf_inc),
    rt         = mean(rt),
    rt_cor     = mean(rt_cor),
    rt_inc     = mean(rt_inc),
    sc_sd      = mean(sc_sd),
    n_trials   = mean(n_trials),
    .groups = "drop") |>
  dplyr::arrange(domain, gender)

cat("\n=== RAW MEANS BY GENDER x DOMAIN (no model, no covariates) ===\n")
cat("calibration = self-belief - accuracy, both in percent; positive = over-estimation\n")
print(as.data.frame(raw_means), row.names = FALSE, digits = 3)

cat("\n=== demographics ===\n")
print(as.data.frame(subjects |> dplyr::group_by(gender) |>
        dplyr::summarise(n = dplyr::n(), age_sd = sd(age), age_m = mean(age),
                         edu_sd = sd(edu), edu_m = mean(edu), .groups = "drop") |>
        dplyr::select(gender, n, age_m, age_sd, edu_m, edu_sd)),
      row.names = FALSE, digits = 3)

# ----------------------------------------------------------------- save ----
dir.create("revision/data", showWarnings = FALSE, recursive = TRUE)
saveRDS(cells,    "revision/data/cells.rds")
saveRDS(subjects, "revision/data/subjects.rds")
write.csv(raw_means, "revision/data/raw_cell_means.csv", row.names = FALSE)
cat("\nwrote revision/data/{cells.rds, subjects.rds, raw_cell_means.csv}\n")
