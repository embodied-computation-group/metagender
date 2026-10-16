# Revision analysis 25 -- the local <-> global metacognition link, by gender
# =============================================================================
# Answers Reviewer 3: "Have gender differences in the link between local and
# global metacognition been tested?"
#
# LOCAL  = the unified hmetad fit's subject-level estimates (06_subject_estimates.R):
#          log M-ratio (metacognitive EFFICIENCY) and meta-Delta (metacognitive
#          BIAS, averaged over the two response sides).
# GLOBAL = self-belief minus accuracy from the questionnaire (20_prep_cells.R):
#          calib_pre, the participant's calibration BEFORE doing the task, and
#          update = calib_post - calib_pre, how far the task moved them.
#
# The regression runs LOCAL ~ GLOBAL, not the other way round, because global
# self-belief is measured before and after the task while the local estimates are
# a property of task behaviour; nothing here is causal in either direction.
#
# DVs      subject x domain log M-ratio; subject x domain meta-Delta.
# Model    y ~ 0 + domain + domain:calib_z + domain:update_z + domain:fem_c
#                + domain:calib_z:fem_c + domain:update_z:fem_c
#                + age_z + edu_z + (1 | subj),
#          sigma ~ 0 + domain
# Coding   fem_c = +0.5 Feminin / -0.5 Masculin; domain cell-means; calib_z and
#          update_z z-scored WITHIN domain, so a slope is "change in the local
#          measure per SD of global calibration" and the four are comparable.
# Read-out domain-general coupling  = draw-wise mean of the four domain:calib_z;
#          gender moderation        = draw-wise mean of the four domain:calib_z:fem_c;
#          the same for update_z. BF10 (Savage-Dickey) and ROPE = 0.1 x SD(DV)
#          exactly as in scripts 21-24.
#
# Priors   s = SD(DV). Cell means and the standardised covariates ~ normal(0, s);
#          CONTRAST coefficients (anything on fem_c) ~ normal(0, k*s) with
#          k = 0.5; sd ~ normal(0, s); sigma ~ student_t(3, 0, s);
#          sample_prior = "yes" so Savage-Dickey BF10 is available for every
#          effect. k is swept over 0.25 / 0.5 / 1 by
#          29_prior_sensitivity_behavioural.R -> prior_sensitivity_behavioural.csv.
#
# TWO CAVEATS, both of which belong in the manuscript wherever this is reported:
#   1. SHRINKAGE. The subject-level log M and meta-Delta are hierarchical
#      posterior means, pulled towards the group mean AND towards their own
#      gender's cell mean. That attenuates every slope below and, because the
#      two genders are pulled towards different points, can in principle create a
#      gender moderation that is not in the data.
#   2. MEASUREMENT ERROR. Their posterior SDs (also in the input files) are
#      ignored: they enter as point predictors. This is the conventional
#      two-stage analysis, and it is anticonservative.
#
# Input   revision/data/cells.rds
#         revision/data/subject_logM.csv        (06_subject_estimates.R)
#         revision/data/subject_metadelta.csv   (06_subject_estimates.R)
# Output  revision/models/m25_{logM,metadelta}.rds   (gitignored)
#         revision/data/effects_local_global.csv
#
# Run from anywhere:  Rscript revision/code/25_local_global_link.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

source("revision/code/utils_bayes.R")
suppressMessages({ library(dplyr); library(tidyr) })

DOMAINS <- c("memory", "vision", "GDP", "Calories")

# ------------------------------------------------------------------ data ----
cells <- readRDS("revision/data/cells.rds") |>
  transmute(subj = as.character(subj), domain = as.character(domain),
            age_z, edu_z, calib_pre, update = calib_post - calib_pre)

# subj is numeric in the first-edition sample, so read.csv types it as integer while
# cells.rds carries it as a factor label; both are forced to character before joining.
logM  <- read.csv("revision/data/subject_logM.csv", stringsAsFactors = FALSE) |>
  mutate(subj = as.character(subj))
metad <- read.csv("revision/data/subject_metadelta.csv", stringsAsFactors = FALSE) |>
  mutate(subj = as.character(subj))

# The hmetad fit dropped cells with d' <= 0 or < 30 trials (01_prep_data.R QC), and the
# behavioural table dropped participants with an empty domain x accuracy cell, so the two
# sets do not coincide. An inner join is the whole exclusion rule; it is reported.
dat <- cells |>
  inner_join(logM  |> select(subj, gender, fem_c, domain, logM = logM_mean),
             by = c("subj", "domain")) |>
  inner_join(metad |> select(subj, domain, md = md_mean), by = c("subj", "domain")) |>
  group_by(domain) |>
  mutate(calib_z = as.numeric(scale(calib_pre)),
         update_z = as.numeric(scale(update))) |>
  ungroup() |>
  mutate(subj = factor(subj), domain = factor(domain, levels = DOMAINS))

cat(sprintf("cells: %d behavioural, %d with an hmetad estimate -> %d analysed (%d subjects)\n",
            nrow(cells), nrow(logM), nrow(dat), nlevels(dat$subj)))
print(as.data.frame(dat |> count(gender, domain) |>
                      pivot_wider(names_from = domain, values_from = n)),
      row.names = FALSE)

# ----------------------------------------------------------------- model ----
dir.create("revision/models", showWarnings = FALSE, recursive = TRUE)

t_calib  <- paste0("domain", DOMAINS, ":calib_z")
t_update <- paste0("domain", DOMAINS, ":update_z")
t_cxg    <- paste0("domain", DOMAINS, ":calib_z:fem_c")
t_uxg    <- paste0("domain", DOMAINS, ":update_z:fem_c")
t_gender <- paste0("domain", DOMAINS, ":fem_c")

# One function, run twice: the two outcomes differ only in the DV and its label.
run_one <- function(dv, label, file) {
  s    <- sd(dat[[dv]])
  ROPE <- 0.1 * s
  cat(sprintf("\n--- %s: SD(DV) = %.4f, ROPE = +/-%.4f ---\n", label, s, ROPE))

  form <- bf(as.formula(paste(dv,
    "~ 0 + domain + domain:calib_z + domain:update_z + domain:fem_c +",
    "domain:calib_z:fem_c + domain:update_z:fem_c + age_z + edu_z + (1 | subj)")),
    sigma ~ 0 + domain)

  m <- fit_light_model(form, dat, s = s, file = file)
  check_convergence(m)

  dr <- posterior::as_draws_df(m)
  b  <- function(term) {
    nm <- paste0("b_", term)
    if (!nm %in% names(dr)) stop("no such coefficient: ", nm, "\nhave: ",
                                paste(grep("^b_", names(dr), value = TRUE), collapse = ", "))
    dr[[nm]]
  }
  avg <- function(tt) Reduce(`+`, lapply(tt, b)) / length(tt)
  mh  <- function(tt) paste0("(", paste(tt, collapse = " + "), ")/", length(tt))

  block <- function(tt, what)
    bind_rows(
      bind_rows(lapply(seq_along(DOMAINS), function(i)
        eff_row(b(tt[i]), paste0(what, ": ", DOMAINS[i]), ROPE, m, tt[i]))),
      eff_row(avg(tt), paste0(what, ": DOMAIN-GENERAL"), ROPE, m, mh(tt)))

  eff <- bind_rows(
    block(t_calib,  "coupling to calibration (pre)"),
    block(t_cxg,    "coupling x gender (calibration)"),
    block(t_update, "coupling to pre->post update"),
    block(t_uxg,    "coupling x gender (update)"),
    block(t_gender, "gender (F-M)"),
    eff_row(b("age_z"), "covariate: age (z)", ROPE, m, "age_z"),
    eff_row(b("edu_z"), "covariate: education (z)", ROPE, m, "edu_z")
  ) |> mutate(model = label)

  print_effects(filter(eff, grepl("^coupling to calibration", eff$effect)),
                paste(label, "~ global calibration (pre), per SD"))
  print_effects(filter(eff, grepl("^coupling x gender \\(calibration", eff$effect)),
                paste(label, ": is the coupling different in women? (F-M)"))
  print_effects(filter(eff, grepl("^coupling to pre", eff$effect)),
                paste(label, "~ pre->post update in self-belief, per SD"))
  print_effects(filter(eff, grepl("^coupling x gender \\(update", eff$effect)),
                paste(label, ": is the update coupling different in women? (F-M)"))
  # The gender main effect is printed only so the reader can see the model is complete.
  # It is NOT the gender test: the DV here is a shrunk posterior mean that already
  # contains the unified model's own gender coefficient, so its interval is far too
  # narrow. The gender effect is reported from the unified fit (script 05), not here.
  print_effects(filter(eff, grepl("^gender \\(F-M\\)|^covariate", eff$effect)),
                paste(label, ": gender main effect (NOT a test -- see header) and covariates"))
  eff
}

eff <- bind_rows(
  run_one("logM", "logM",      ktag("revision/models/m25_logM")),
  run_one("md",   "metadelta", ktag("revision/models/m25_metadelta"))
)

write.csv(eff, ktag("revision/data/effects_local_global.csv"), row.names = FALSE)
cat("\nwrote ", ktag("revision/data/effects_local_global.csv"), "\n", sep = "")
