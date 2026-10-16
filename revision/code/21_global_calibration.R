# Revision analysis 21 -- global metacognitive calibration (self-belief - accuracy)
# =============================================================================
# Answers: Reviewer 1 point 1 ("calculate the difference between judgement and
# actual task performance ... positive = overestimation, zero = no bias,
# negative = underestimation"), Reviewer 2 on self-beliefs (is the measure a
# bias at all?), and the editor's requirement that null claims carry a BF.
#
# DV       calibration = self-belief - accuracy, both in percent correct.
#          > 0 over-estimation, = 0 calibrated, < 0 under-estimation.
# Design   long over time (pre / post), one row per subject x domain x time.
# Model    calib ~ 0 + domain + domain:fem_c + domain:time_c
#                   + domain:fem_c:time_c + age_z + edu_z + (1 | subj),
#          sigma ~ 0 + domain:time   (residual SD free per domain x time cell)
# Coding   fem_c  = +0.5 Feminin / -0.5 Masculin
#          time_c = +0.5 post    / -0.5 pre
#          domain = cell means, so every effect is estimated inside a domain.
#          Domain-general effects are the draw-wise MEAN of the four
#          per-domain effects; domain specificity is their pairwise differences.
# Priors   s = SD(DV). Cell means and the standardised covariates ~ normal(0, s);
#          CONTRAST coefficients (anything on fem_c or time_c) ~ normal(0, k*s) with
#          k = 0.5; sd ~ normal(0, s); sigma ~ student_t(3, 0, s);
#          sample_prior = "yes" so Savage-Dickey BF10 is available for every
#          effect. k is swept over 0.25 / 0.5 / 1 by
#          29_prior_sensitivity_behavioural.R -> prior_sensitivity_behavioural.csv.
# ROPE     +/- 0.1 * s percentage points.
#
# Input   revision/data/cells.rds
# Output  revision/models/m21_calibration.rds   (gitignored)
#         revision/data/effects_calibration.csv
#
# Run from anywhere:  Rscript revision/code/21_global_calibration.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

source("revision/code/utils_bayes.R")
suppressMessages({ library(dplyr); library(tidyr) })

DOMAINS <- c("memory", "vision", "GDP", "Calories")

# ------------------------------------------------------------- long data ----
cells <- readRDS("revision/data/cells.rds")

dat <- cells |>
  select(subj, gender, fem_c, domain, age_z, edu_z, calib_pre, calib_post) |>
  pivot_longer(c(calib_pre, calib_post), names_to = "time",
               names_prefix = "calib_", values_to = "calib") |>
  mutate(time   = factor(time, levels = c("pre", "post")),
         time_c = ifelse(time == "post", 0.5, -0.5))

s    <- sd(dat$calib)
ROPE <- 0.1 * s
cat(sprintf("N = %d subjects, %d rows; SD(calibration) = %.2f pp; ROPE = +/-%.2f pp\n",
            nlevels(dat$subj), nrow(dat), s, ROPE))

# ----------------------------------------------------------------- model ----
dir.create("revision/models", showWarnings = FALSE, recursive = TRUE)
form <- bf(calib ~ 0 + domain + domain:fem_c + domain:time_c +
                   domain:fem_c:time_c + age_z + edu_z + (1 | subj),
           sigma ~ 0 + domain:time)

m <- fit_light_model(form, dat, s = s, file = ktag("revision/models/m21_calibration"))
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
t_time   <- paste0("domain", DOMAINS, ":time_c")
t_gxt    <- paste0("domain", DOMAINS, ":fem_c:time_c")

# ------------------------------------------------------------- effects ------
eff <- bind_rows(
  # gender: Feminin - Masculin, averaged over pre and post
  bind_rows(lapply(seq_along(DOMAINS), function(i)
    eff_row(b(t_gender[i]), paste0("gender (F-M): ", DOMAINS[i]), ROPE, m, t_gender[i]))),
  eff_row(avg(t_gender), "gender (F-M): DOMAIN-GENERAL", ROPE, m,
          paste0("(", paste(t_gender, collapse = " + "), ")/4")),

  # time: post - pre, averaged over genders
  bind_rows(lapply(seq_along(DOMAINS), function(i)
    eff_row(b(t_time[i]), paste0("time (post-pre): ", DOMAINS[i]), ROPE, m, t_time[i]))),
  eff_row(avg(t_time), "time (post-pre): DOMAIN-GENERAL", ROPE, m,
          paste0("(", paste(t_time, collapse = " + "), ")/4")),

  # gender x time
  bind_rows(lapply(seq_along(DOMAINS), function(i)
    eff_row(b(t_gxt[i]), paste0("gender x time: ", DOMAINS[i]), ROPE, m, t_gxt[i]))),
  eff_row(avg(t_gxt), "gender x time: DOMAIN-GENERAL", ROPE, m,
          paste0("(", paste(t_gxt, collapse = " + "), ")/4")),

  # domain specificity of the gender effect: all six pairwise differences
  bind_rows(lapply(combn(seq_along(DOMAINS), 2, simplify = FALSE), function(k)
    eff_row(b(t_gender[k[1]]) - b(t_gender[k[2]]),
            paste0("gender x domain: ", DOMAINS[k[1]], " - ", DOMAINS[k[2]]), ROPE,
            m, paste0(t_gender[k[1]], " - ", t_gender[k[2]]))))
) |> mutate(model = "calibration")

print_effects(filter(eff, grepl("^gender \\(F-M\\)", effect)),
              "gender effect on calibration (Feminin - Masculin, pp)")
print_effects(filter(eff, grepl("^time", effect)),
              "time effect on calibration (post - pre, pp)")
print_effects(filter(eff, grepl("^gender x time", effect)),
              "gender x time on calibration (pp)")
print_effects(filter(eff, grepl("^gender x domain", effect)),
              "domain specificity of the gender effect (pp)")

# --------------------------------------------- cell means: who is biased? ----
# Draw-wise calibration in each gender x time x domain cell, covariates at their
# means (age_z = edu_z = 0). Positive = over-estimation of own performance.
cell <- function(dom, g, t) {
  b(paste0("domain", dom)) +
    g * b(paste0("domain", dom, ":fem_c")) +
    t * b(paste0("domain", dom, ":time_c")) +
    g * t * b(paste0("domain", dom, ":fem_c:time_c"))
}
grid <- expand.grid(domain = c(DOMAINS, "DOMAIN-GENERAL"),
                    gender = c("Masculin", "Feminin"),
                    time   = c("pre", "post"), stringsAsFactors = FALSE)
cells_tab <- bind_rows(lapply(seq_len(nrow(grid)), function(i) {
  g <- ifelse(grid$gender[i] == "Feminin", 0.5, -0.5)
  t <- ifelse(grid$time[i]   == "post",    0.5, -0.5)
  x <- if (grid$domain[i] == "DOMAIN-GENERAL")
    Reduce(`+`, lapply(DOMAINS, cell, g = g, t = t)) / 4 else cell(grid$domain[i], g, t)
  eff_row(x, sprintf("calibration: %s, %s, %s", grid$domain[i],
                     ifelse(grid$gender[i] == "Feminin", "Women", "Men"), grid$time[i]),
          ROPE) |> mutate(model = "calibration_cellmean")
}))
print_effects(cells_tab, "calibration cell means (pp; P(>0) = P(over-estimation))")

# --------------------------------------------------------------- output -----
write.csv(bind_rows(eff, cells_tab), ktag("revision/data/effects_calibration.csv"),
          row.names = FALSE)
cat("\nwrote ", ktag("revision/data/effects_calibration.csv"), "\n", sep = "")
