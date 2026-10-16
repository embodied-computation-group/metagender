# Revision analysis 20b -- subject x domain cell table for the VMP replication
# =============================================================================
# The behavioural mirror of 20_prep_cells.R for the Visceral Mind Project sample
# (456 independent subjects, the same GDP and Calories trivia tasks, the same
# 7-point confidence scale). It exists so that the local-confidence model
# (22_local_confidence.R -> 22b) can be refitted on independent data.
#
# WHAT THE VMP SAMPLE DOES NOT HAVE, and what that costs:
#   - no self-belief questionnaire  -> no calibration, no Figure 2 mirror, no model 21;
#   - no years of education         -> the covariate set is AGE ONLY. Model 22b therefore
#                                      drops edu_z; nothing else about the model changes;
#   - no memory or vision task      -> two domains instead of four;
#   - no staircase                  -> trivia is not adaptive in either sample, so there
#                                      is nothing to mirror from model 24 either.
#
# PREPROCESSING is matched to 20_prep_cells.R so the two samples are comparable:
#   1. drop trials with RT <= 0.05 s (VMP records RT in MILLISECONDS; converted here);
#   2. the first edition's pooled Tukey 1.5 x IQR fence on RT (code/remove_outliers.R);
#   3. drop the one participant coded gender = "Other" (n = 1 cannot identify a level),
#      exactly as 01b_prep_vmp.R does;
#   4. drop participants with an empty domain x accuracy cell, since conf_cor / conf_inc
#      and rt_cor / rt_inc must exist for every cell the accuracy-split model uses.
#   The d' <= 0 / thin-cell QC of 01b_prep_vmp.R is NOT applied: it exists because the
#   log link on M-ratio cannot represent a below-chance cell, which is a constraint of
#   the hmetad fit and not of a gaussian model of mean confidence. The cell counts
#   therefore differ slightly from hmetad_trial_vmp.rds, and that is intended.
#
# Input   revision/data/vmp/trivia_trials.tsv   (Subject, Condition, Responses, Stimuli,
#                                                Accuracy, Confidence, RT)
#         revision/data/vmp/demographics.tsv    (participant_id, gender, age)
#         code/remove_outliers.R
# Output  revision/data/cells_vmp.rds
#         revision/data/raw_cell_means_vmp.csv
#
# Run from anywhere:  Rscript revision/code/20b_prep_cells_vmp.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

suppressMessages({ library(dplyr); library(tidyr) })

DOMAINS  <- c("GDP", "Calories")
COND_MAP <- c("0" = "GDP", "1" = "Calories")   # as in 01b_prep_vmp.R

# ---------------------------------------------------------------- load ----
trials <- read.delim("revision/data/vmp/trivia_trials.tsv")
demo   <- read.delim("revision/data/vmp/demographics.tsv")
stopifnot(!anyDuplicated(demo$participant_id))

trials$domain <- unname(COND_MAP[as.character(trials$Condition)])
stopifnot(!anyNA(trials$domain))

cat("raw: ", nrow(trials), " trials, ", length(unique(trials$Subject)),
    " subjects\n", sep = "")

# --------------------------------------------------------- demographics ----
# BIDS gender codes: 1 = Female, 2 = Male, 3 = Other.
i <- match(trials$Subject, demo$participant_id)
trials$gender_code <- demo$gender[i]
trials$age         <- demo$age[i]

n_other <- length(unique(trials$Subject[trials$gender_code == 3 & !is.na(trials$gender_code)]))
n_na    <- length(unique(trials$Subject[is.na(trials$gender_code)]))
if (n_other) cat("dropping ", n_other, " subject(s) with gender = Other\n", sep = "")
if (n_na)    cat("dropping ", n_na, " subject(s) with no demographics row\n", sep = "")
trials <- trials[trials$gender_code %in% c(1, 2) & !is.na(trials$age), ]

# ------------------------------------------------------- trial cleaning ----
source("code/remove_outliers.R")
n0 <- nrow(trials)
trials$rt <- trials$RT / 1000                       # ms -> s, to match the first edition
trials <- dplyr::filter(trials, rt > 0.05, !is.na(Confidence))
trials$rt <- remove_outliers(trials$rt)             # pooled Tukey fence, as in 20_prep_cells.R
trials <- trials[!is.na(trials$rt), ]
cat("after RT / confidence cleaning: ", nrow(trials), " trials (",
    round(100 * (1 - nrow(trials) / n0), 1), "% removed)\n", sep = "")

# ----------------------------------------------- empty-cell exclusions ----
complete_subj <- trials |>
  dplyr::count(Subject, domain, Accuracy) |>
  dplyr::count(Subject, name = "n_cells") |>
  dplyr::filter(n_cells == 2 * length(DOMAINS)) |>
  dplyr::pull(Subject)
dropped <- setdiff(unique(trials$Subject), complete_subj)
if (length(dropped))
  cat("dropped for an empty domain x accuracy cell: ", length(dropped),
      " subject(s)\n", sep = "")
trials <- dplyr::filter(trials, Subject %in% complete_subj)

# ------------------------------------------------------- coding scheme ----
# Identical to 20_prep_cells.R: fem_c = +0.5 Feminin / -0.5 Masculin, so every
# contrast reads Feminin - Masculin.
subjects <- trials |>
  dplyr::distinct(subj = Subject, gender_code, age) |>
  dplyr::mutate(
    subj   = factor(paste0("vmp_", subj)),
    gender = factor(ifelse(gender_code == 1, "Feminin", "Masculin"),
                    levels = c("Masculin", "Feminin")),
    fem_c  = ifelse(gender == "Feminin", 0.5, -0.5),
    age_z  = as.numeric(scale(age))
  ) |>
  dplyr::select(subj, gender, fem_c, age, age_z)

cat("\nFINAL N = ", nrow(subjects), "  (",
    sum(subjects$gender == "Feminin"), " Feminin, ",
    sum(subjects$gender == "Masculin"), " Masculin)\n", sep = "")

# --------------------------------------------------- task-level summaries ----
trials$subj <- factor(paste0("vmp_", trials$Subject), levels = levels(subjects$subj))

task <- trials |>
  dplyr::group_by(subj, domain) |>
  dplyr::summarise(n_trials = dplyr::n(),
                   accuracy = 100 * mean(Accuracy),      # percent, as in 20_prep_cells.R
                   conf     = mean(Confidence),
                   rt       = mean(rt), .groups = "drop")

task_acc <- trials |>
  dplyr::group_by(subj, domain, Accuracy) |>
  dplyr::summarise(conf = mean(Confidence), rt = mean(rt), .groups = "drop") |>
  tidyr::pivot_wider(names_from = Accuracy, values_from = c(conf, rt),
                     names_glue = "{.value}_{ifelse(Accuracy == 1, 'cor', 'inc')}")

# ------------------------------------------------------------- assemble ----
cells <- subjects |>
  dplyr::left_join(task,     by = "subj") |>
  dplyr::left_join(task_acc, by = c("subj", "domain")) |>
  dplyr::mutate(domain = factor(domain, levels = DOMAINS)) |>
  dplyr::group_by(domain) |>
  dplyr::mutate(acc_z = as.numeric(scale(accuracy))) |>   # z-scored WITHIN domain
  dplyr::ungroup() |>
  dplyr::arrange(subj, domain)

stopifnot(nrow(cells) == nrow(subjects) * length(DOMAINS),
          !anyNA(cells$conf_cor), !anyNA(cells$conf_inc),
          !anyNA(cells$rt_cor),   !anyNA(cells$rt_inc))

# ------------------------------------------------------------- raw means ----
raw_means <- cells |>
  dplyr::group_by(gender, domain) |>
  dplyr::summarise(n = dplyr::n(), accuracy = mean(accuracy), conf = mean(conf),
                   conf_cor = mean(conf_cor), conf_inc = mean(conf_inc),
                   rt = mean(rt), rt_cor = mean(rt_cor), rt_inc = mean(rt_inc),
                   n_trials = mean(n_trials), .groups = "drop") |>
  dplyr::arrange(domain, gender)

cat("\n=== RAW MEANS BY GENDER x DOMAIN, VMP (no model, no covariates) ===\n")
print(as.data.frame(raw_means), row.names = FALSE, digits = 3)

cat("\n=== demographics ===\n")
print(as.data.frame(subjects |> dplyr::group_by(gender) |>
        dplyr::summarise(n = dplyr::n(), age_m = mean(age), age_sd = sd(age),
                         .groups = "drop")),
      row.names = FALSE, digits = 3)

# ----------------------------------------------------------------- save ----
dir.create("revision/data", showWarnings = FALSE, recursive = TRUE)
saveRDS(cells, "revision/data/cells_vmp.rds")
write.csv(raw_means, "revision/data/raw_cell_means_vmp.csv", row.names = FALSE)
cat("\nwrote revision/data/{cells_vmp.rds, raw_cell_means_vmp.csv}\n")
