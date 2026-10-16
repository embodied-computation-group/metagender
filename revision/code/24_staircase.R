# Revision analysis 24 -- staircase difficulty variability by gender
# =============================================================================
# Answers Reviewer 2: "M-Ratio is biased when the task involves multiple
# difficulty levels, as with a staircase (Rahnev & Fleming, 2019). Acknowledge
# this and evaluate whether staircase-related difficulty variation was larger
# for women than men." If one gender's staircase wandered more, their M-ratio
# would be biased relative to the other's and the headline contrast would be an
# artefact. This script tests that directly.
#
# DV       SD of the absolute stimulus level (|contrast|) across a participant's
#          trials in a domain -- how much the staircase moved them around.
#          Vision, GDP and Calories only: the memory task used three fixed
#          blockwise difficulty levels, not a staircase, so it has no such
#          quantity and is excluded.
#
#          THE SCALES ARE NOT COMPARABLE ACROSS TASKS. |contrast| is degrees of
#          orientation (vision, ~0-20), percent GDP (~0.1-5) and kilocalories
#          (~1-500). A model on the raw SD would be dominated by the Calories
#          units and its residual SD would be meaningless. The DV is therefore
#          the staircase SD Z-SCORED WITHIN DOMAIN, so a coefficient is read in
#          within-domain SD units and is comparable across the three tasks. The
#          raw SDs and the unitless coefficient of variation are printed below
#          for the record.
#
# Model    sc_sd_z ~ 0 + domain + domain:fem_c + age_z + edu_z + (1 | subj),
#          sigma ~ 0 + domain   (residual SD free per domain)
# Coding   fem_c = +0.5 Feminin / -0.5 Masculin; domain = cell means;
#          domain-general = draw-wise mean of the three.
# Priors   s = SD(DV). Cell means and the standardised covariates ~ normal(0, s);
#          CONTRAST coefficients (anything on fem_c) ~ normal(0, k*s) with
#          k = 0.5; sd ~ normal(0, s); sigma ~ student_t(3, 0, s);
#          sample_prior = "yes" so Savage-Dickey BF10 is available for every
#          effect. k is swept over 0.25 / 0.5 / 1 by
#          29_prior_sensitivity_behavioural.R -> prior_sensitivity_behavioural.csv.
# ROPE     +/- 0.1 * s (i.e. a tenth of a within-domain SD).
#
# Input   revision/data/cells.rds
# Output  revision/models/m24_staircase.rds   (gitignored)
#         revision/data/effects_staircase.csv
#
# Run from anywhere:  Rscript revision/code/24_staircase.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

source("revision/code/utils_bayes.R")
suppressMessages({ library(dplyr) })

DOMAINS <- c("vision", "GDP", "Calories")   # staircased tasks only

# ------------------------------------------------------------------ data ----
cells <- readRDS("revision/data/cells.rds")

dat <- cells |>
  filter(domain %in% DOMAINS) |>
  mutate(domain = factor(as.character(domain), levels = DOMAINS)) |>
  select(subj, gender, fem_c, domain, age_z, edu_z, sc_sd, sc_mean, sc_cv, sc_sd_z)
stopifnot(!anyNA(dat$sc_sd_z))

cat("\n=== raw staircase descriptives by gender x domain ===\n")
cat("sc_sd is in each task's own stimulus units and is NOT comparable across rows\n")
print(as.data.frame(dat |> group_by(domain, gender) |>
        summarise(n = n(), sd_mean = mean(sc_sd), sd_sd = sd(sc_sd),
                  level_mean = mean(sc_mean), cv = mean(sc_cv), .groups = "drop")),
      row.names = FALSE, digits = 3)

s    <- sd(dat$sc_sd_z)
ROPE <- 0.1 * s
cat(sprintf("\nN = %d subjects, %d rows; SD(DV) = %.3f; ROPE = +/-%.3f (within-domain SD units)\n",
            nlevels(droplevels(dat$subj)), nrow(dat), s, ROPE))

# ----------------------------------------------------------------- model ----
dir.create("revision/models", showWarnings = FALSE, recursive = TRUE)
form <- bf(sc_sd_z ~ 0 + domain + domain:fem_c + age_z + edu_z + (1 | subj),
           sigma ~ 0 + domain)

m <- fit_light_model(form, dat, s = s, file = ktag("revision/models/m24_staircase"))
check_convergence(m)

dr <- posterior::as_draws_df(m)
b  <- function(term) {
  nm <- paste0("b_", term)
  if (!nm %in% names(dr)) stop("no such coefficient: ", nm, "\nhave: ",
                              paste(grep("^b_", names(dr), value = TRUE), collapse = ", "))
  dr[[nm]]
}
avg <- function(terms) Reduce(`+`, lapply(terms, b)) / length(terms)
t_gender <- paste0("domain", DOMAINS, ":fem_c")

# ------------------------------------------------------------- effects ------
eff <- bind_rows(
  bind_rows(lapply(seq_along(DOMAINS), function(i)
    eff_row(b(t_gender[i]), paste0("gender (F-M): ", DOMAINS[i]), ROPE, m, t_gender[i]))),
  eff_row(avg(t_gender), "gender (F-M): DOMAIN-GENERAL", ROPE, m,
          paste0("(", paste(t_gender, collapse = " + "), ")/3")),
  bind_rows(lapply(combn(seq_along(DOMAINS), 2, simplify = FALSE), function(k)
    eff_row(b(t_gender[k[1]]) - b(t_gender[k[2]]),
            paste0("gender x domain: ", DOMAINS[k[1]], " - ", DOMAINS[k[2]]), ROPE,
            m, paste0(t_gender[k[1]], " - ", t_gender[k[2]])))),
  eff_row(b("age_z"), "covariate: age (z)", ROPE, m, "age_z"),
  eff_row(b("edu_z"), "covariate: education (z)", ROPE, m, "edu_z")
) |> mutate(model = "staircase_sd")

print_effects(filter(eff, grepl("^gender \\(F-M\\)", effect)),
              "gender effect on staircase variability (F-M, within-domain SD units)")
print_effects(filter(eff, grepl("^gender x domain|^covariate", effect)),
              "domain specificity and covariates")

# --------------------------------------------------------------- output -----
write.csv(eff, ktag("revision/data/effects_staircase.csv"), row.names = FALSE)
cat("\nwrote ", ktag("revision/data/effects_staircase.csv"), "\n", sep = "")
