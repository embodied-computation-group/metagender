# Revision analysis 26 -- positive controls: task accuracy and RT by gender
# =============================================================================
# Replaces the JASP Welch t-tests reported in "Task accuracy & reaction time".
# The manuscript claims task difficulty was "robustly controlled between
# genders"; the editor requires that such null claims be supported by a Bayes
# factor or an equivalence statement rather than by p > .05. Same modelling
# convention as scripts 21-24 so the positive control reads as part of one
# system rather than as a leftover frequentist appendix.
#
# Two models, one row per subject x domain in each:
#   A  accuracy (percent correct) ~ 0 + domain + domain:fem_c + age_z + edu_z + (1|subj)
#   B  mean type-1 RT (s)         ~ 0 + domain + domain:fem_c + age_z + edu_z + (1|subj)
# Both carry `sigma ~ 0 + domain`: between-subject accuracy SD is 1.9 percentage
# points for vision but 8.8 for memory, so a pooled residual SD would misstate
# the precision of every per-domain contrast.
#
# Coding   fem_c = +0.5 Feminin / -0.5 Masculin; domain = cell means;
#          domain-general = draw-wise mean of the four per-domain effects.
# Priors   s = SD(DV). Cell means and the standardised covariates ~ normal(0, s);
#          CONTRAST coefficients (anything on fem_c) ~ normal(0, k*s) with
#          k = 0.5; sd ~ normal(0, s); sigma ~ student_t(3, 0, s);
#          sample_prior = "yes" so Savage-Dickey BF10 is available for every
#          effect. k is swept over 0.25 / 0.5 / 1 by
#          29_prior_sensitivity_behavioural.R -> prior_sensitivity_behavioural.csv.
# ROPE     +/- 0.1 * s (0.61 percentage points for accuracy, 0.046 s for RT).
#
# Input   revision/data/cells.rds
# Output  revision/models/m26_accuracy.rds, revision/models/m26_rt.rds (gitignored)
#         revision/data/effects_positive_controls.csv
#
# Run from anywhere:  Rscript revision/code/26_positive_controls.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

source("revision/code/utils_bayes.R")
suppressMessages({ library(dplyr) })

DOMAINS <- c("memory", "vision", "GDP", "Calories")
cells   <- readRDS("revision/data/cells.rds")
dir.create("revision/models", showWarnings = FALSE, recursive = TRUE)

# One routine, run twice: the two control models differ only in the DV.
run_control <- function(dv, label, file) {
  dat <- cells |> select(subj, gender, fem_c, domain, age_z, edu_z, y = all_of(dv))
  s    <- sd(dat$y)
  ROPE <- 0.1 * s
  cat(sprintf("\n---- %s: SD(DV) = %.3f, ROPE = +/-%.3f ----\n", label, s, ROPE))

  form <- bf(y ~ 0 + domain + domain:fem_c + age_z + edu_z + (1 | subj),
             sigma ~ 0 + domain)
  m <- fit_light_model(form, dat, s = s, file = file)
  check_convergence(m)

  dr <- posterior::as_draws_df(m)
  b  <- function(term) {
    nm <- paste0("b_", term)
    if (!nm %in% names(dr)) stop("no such coefficient: ", nm)
    dr[[nm]]
  }
  t_gender <- paste0("domain", DOMAINS, ":fem_c")
  eff <- bind_rows(
    bind_rows(lapply(seq_along(DOMAINS), function(i)
      eff_row(b(t_gender[i]), paste0("gender (F-M): ", DOMAINS[i]), ROPE, m, t_gender[i]))),
    eff_row(Reduce(`+`, lapply(t_gender, b)) / 4, "gender (F-M): DOMAIN-GENERAL", ROPE,
            m, paste0("(", paste(t_gender, collapse = " + "), ")/4"))
  ) |> mutate(model = label)

  print_effects(eff, paste0("gender contrast on ", label))
  eff
}

eff <- bind_rows(
  run_control("accuracy", "accuracy (percentage points)", ktag("revision/models/m26_accuracy")),
  run_control("rt",       "mean RT (seconds)",            ktag("revision/models/m26_rt"))
)

write.csv(eff, ktag("revision/data/effects_positive_controls.csv"), row.names = FALSE)
cat("\nwrote ", ktag("revision/data/effects_positive_controls.csv"), "\n", sep = "")
