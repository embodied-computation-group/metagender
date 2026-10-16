# Revision analysis 23 -- reaction time by gender and trial accuracy
# =============================================================================
# Answers Reviewer 3: "if men are more impulsive, fast easy-to-detect errors
# would artificially boost their metacognitive sensitivity". The test of that
# is the gender x accuracy interaction on RT: are men's errors faster relative
# to their correct responses than women's errors are relative to theirs?
#
# DV       mean type-1 RT in seconds, per subject x domain x trial accuracy.
#          Kept on the raw second scale: after the first edition's trial-level
#          RT trimming the cell means are close to symmetric (skewness -0.08),
#          and a log transform makes them MORE skewed (-0.76). Reported in the
#          script output so the choice is checkable.
# Model    rt ~ 0 + domain + domain:fem_c + domain:acc_c + domain:fem_c:acc_c
#                 + age_z + edu_z + (1 | subj),
#          sigma ~ 0 + domain:acc   (residual SD free per domain x accuracy cell)
# Coding   fem_c = +0.5 Feminin / -0.5 Masculin
#          acc_c = +0.5 correct / -0.5 incorrect
#          domain = cell means; domain-general = draw-wise mean of the four.
#          The RT slowing on errors is (incorrect - correct) = -b_acc.
# Priors   s = SD(DV). Cell means and the standardised covariates ~ normal(0, s);
#          CONTRAST coefficients (anything on fem_c or acc_c) ~ normal(0, k*s) with
#          k = 0.5; sd ~ normal(0, s); sigma ~ student_t(3, 0, s);
#          sample_prior = "yes" so Savage-Dickey BF10 is available for every
#          effect. k is swept over 0.25 / 0.5 / 1 by
#          29_prior_sensitivity_behavioural.R -> prior_sensitivity_behavioural.csv.
# ROPE     +/- 0.1 * s seconds.
#
# Input   revision/data/cells.rds
# Output  revision/models/m23_rt.rds   (gitignored)
#         revision/data/effects_rt.csv
#
# Run from anywhere:  Rscript revision/code/23_reaction_time.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

source("revision/code/utils_bayes.R")
suppressMessages({ library(dplyr); library(tidyr) })

DOMAINS <- c("memory", "vision", "GDP", "Calories")
skew <- function(x) mean((x - mean(x))^3) / stats::sd(x)^3

# ------------------------------------------------------------- long data ----
cells <- readRDS("revision/data/cells.rds")

dat <- cells |>
  select(subj, gender, fem_c, domain, age_z, edu_z, rt_cor, rt_inc) |>
  pivot_longer(c(rt_cor, rt_inc), names_to = "acc",
               names_prefix = "rt_", values_to = "rt") |>
  mutate(acc   = factor(acc, levels = c("inc", "cor")),
         acc_c = ifelse(acc == "cor", 0.5, -0.5))

cat(sprintf("skewness of cell-mean RT: raw = %.2f, log = %.2f -> modelling RAW seconds\n",
            skew(dat$rt), skew(log(dat$rt))))

s    <- sd(dat$rt)
ROPE <- 0.1 * s
cat(sprintf("N = %d subjects, %d rows; SD(mean RT) = %.3f s; ROPE = +/-%.3f s\n",
            nlevels(dat$subj), nrow(dat), s, ROPE))

# ----------------------------------------------------------------- model ----
dir.create("revision/models", showWarnings = FALSE, recursive = TRUE)
form <- bf(rt ~ 0 + domain + domain:fem_c + domain:acc_c + domain:fem_c:acc_c +
                age_z + edu_z + (1 | subj),
           sigma ~ 0 + domain:acc)

m <- fit_light_model(form, dat, s = s, file = ktag("revision/models/m23_rt"))
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
t_acc    <- paste0("domain", DOMAINS, ":acc_c")
t_gxa    <- paste0("domain", DOMAINS, ":fem_c:acc_c")

# ------------------------------------------------------------- effects ------
eff <- bind_rows(
  bind_rows(lapply(seq_along(DOMAINS), function(i)
    eff_row(b(t_gender[i]), paste0("gender (F-M): ", DOMAINS[i]), ROPE, m, t_gender[i]))),
  eff_row(avg(t_gender), "gender (F-M): DOMAIN-GENERAL", ROPE, m,
          paste0("(", paste(t_gender, collapse = " + "), ")/4")),

  # error slowing = incorrect - correct = -b_acc, positive means errors are slower
  bind_rows(lapply(seq_along(DOMAINS), function(i)
    eff_row(-b(t_acc[i]), paste0("error slowing (inc-cor): ", DOMAINS[i]), ROPE, m,
            paste0("-1 * ", t_acc[i])))),
  eff_row(-avg(t_acc), "error slowing (inc-cor): DOMAIN-GENERAL", ROPE, m,
          paste0("-1 * (", paste(t_acc, collapse = " + "), ")/4")),

  # gender x accuracy on RT. Reported as the gender difference in ERROR SLOWING,
  # i.e. (inc-cor)_women - (inc-cor)_men = -b_gxa.
  # NEGATIVE = women slow down LESS on errors than men, equivalently men's
  # errors are relatively faster only if this is POSITIVE. Reviewer 3's
  # impulsivity account predicts a POSITIVE value (men slow less on errors).
  bind_rows(lapply(seq_along(DOMAINS), function(i)
    eff_row(-b(t_gxa[i]), paste0("gender x accuracy (error slowing F-M): ", DOMAINS[i]),
            ROPE, m, paste0("-1 * ", t_gxa[i])))),
  eff_row(-avg(t_gxa), "gender x accuracy (error slowing F-M): DOMAIN-GENERAL", ROPE, m,
          paste0("-1 * (", paste(t_gxa, collapse = " + "), ")/4")),

  eff_row(b("age_z"), "covariate: age (z)", ROPE, m, "age_z"),
  eff_row(b("edu_z"), "covariate: education (z)", ROPE, m, "edu_z")
) |> mutate(model = "rt")

print_effects(filter(eff, grepl("^gender \\(F-M\\)", effect)),
              "gender effect on RT (Feminin - Masculin, seconds)")
print_effects(filter(eff, grepl("^error slowing", effect)),
              "error slowing (incorrect - correct RT, seconds)")
print_effects(filter(eff, grepl("^gender x accuracy", effect)),
              "gender difference in error slowing (positive = men slow LESS on errors)")
print_effects(filter(eff, grepl("^covariate", effect)), "covariates")

# --------------------------------------------------------------- output -----
write.csv(eff, ktag("revision/data/effects_rt.csv"), row.names = FALSE)
cat("\nwrote ", ktag("revision/data/effects_rt.csv"), "\n", sep = "")
