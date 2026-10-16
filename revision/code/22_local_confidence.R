# Revision analysis 22 -- local confidence, with task accuracy controlled
# =============================================================================
# Answers: Reviewer 1 point 2 (is the gender effect a general confidence shift
# or specifically greater error sensitivity in women?), Reviewer 1 minor 1 and
# Reviewer 2 (the Calories null may simply reflect women's higher accuracy
# there -- so accuracy must be controlled before the domain-specificity claim
# can be made), and Reviewer 2's request for a regression rather than an ANOVA.
#
# Mean confidence is reported here as a DESCRIPTIVE quantity. It is not a
# metacognitive bias in the normative sense (Reviewer 2's point); the bias
# proper is meta-Delta from the unified model (scripts 03/05).
#
# DV       mean confidence on the 1-7 scale, per subject x domain x accuracy.
# Model    conf ~ 0 + domain + domain:fem_c + domain:acc_c + domain:fem_c:acc_c
#                   + acc_z + age_z + edu_z + (1 | subj),
#          sigma ~ 0 + domain:acc   (residual SD free per domain x accuracy cell)
# Coding   fem_c = +0.5 Feminin / -0.5 Masculin
#          acc_c = +0.5 correct trials / -0.5 incorrect trials
#          acc_z = the subject's OVERALL accuracy in that domain, z-scored
#                  within domain -- this is the accuracy control Reviewer 2
#                  asked for, and it is a between-subject covariate, distinct
#                  from the within-subject trial-accuracy split acc_c.
#          domain = cell means. Domain-general = draw-wise mean of the four.
# Priors   s = SD(DV). Cell means and the standardised covariates ~ normal(0, s);
#          CONTRAST coefficients (anything on fem_c or acc_c) ~ normal(0, k*s) with
#          k = 0.5; sd ~ normal(0, s); sigma ~ student_t(3, 0, s);
#          sample_prior = "yes" so Savage-Dickey BF10 is available for every
#          effect. k is swept over 0.25 / 0.5 / 1 by
#          29_prior_sensitivity_behavioural.R -> prior_sensitivity_behavioural.csv.
# ROPE     +/- 0.1 * s confidence points.
#
# Input   revision/data/cells.rds
# Output  revision/models/m22_confidence.rds   (gitignored)
#         revision/data/effects_confidence.csv
#
# Run from anywhere:  Rscript revision/code/22_local_confidence.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

source("revision/code/utils_bayes.R")
suppressMessages({ library(dplyr); library(tidyr) })

DOMAINS <- c("memory", "vision", "GDP", "Calories")

# ------------------------------------------------------------- long data ----
cells <- readRDS("revision/data/cells.rds")

dat <- cells |>
  select(subj, gender, fem_c, domain, age_z, edu_z, acc_z, conf_cor, conf_inc) |>
  pivot_longer(c(conf_cor, conf_inc), names_to = "acc",
               names_prefix = "conf_", values_to = "conf") |>
  mutate(acc   = factor(acc, levels = c("inc", "cor")),
         acc_c = ifelse(acc == "cor", 0.5, -0.5))

s    <- sd(dat$conf)
ROPE <- 0.1 * s
cat(sprintf("N = %d subjects, %d rows; SD(mean confidence) = %.3f; ROPE = +/-%.3f\n",
            nlevels(dat$subj), nrow(dat), s, ROPE))

# ----------------------------------------------------------------- model ----
dir.create("revision/models", showWarnings = FALSE, recursive = TRUE)
form <- bf(conf ~ 0 + domain + domain:fem_c + domain:acc_c + domain:fem_c:acc_c +
                  acc_z + age_z + edu_z + (1 | subj),
           sigma ~ 0 + domain:acc)

m <- fit_light_model(form, dat, s = s, file = ktag("revision/models/m22_confidence"))
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
  # gender, averaged over correct and incorrect trials, accuracy controlled
  bind_rows(lapply(seq_along(DOMAINS), function(i)
    eff_row(b(t_gender[i]), paste0("gender (F-M): ", DOMAINS[i]), ROPE, m, t_gender[i]))),
  eff_row(avg(t_gender), "gender (F-M): DOMAIN-GENERAL", ROPE, m,
          paste0("(", paste(t_gender, collapse = " + "), ")/4")),

  # accuracy effect (correct - incorrect): the standard confidence-accuracy gap
  bind_rows(lapply(seq_along(DOMAINS), function(i)
    eff_row(b(t_acc[i]), paste0("accuracy (cor-inc): ", DOMAINS[i]), ROPE, m, t_acc[i]))),
  eff_row(avg(t_acc), "accuracy (cor-inc): DOMAIN-GENERAL", ROPE, m,
          paste0("(", paste(t_acc, collapse = " + "), ")/4")),

  # gender x accuracy = ERROR SENSITIVITY (Reviewer 1 point 2).
  # POSITIVE = women have a LARGER correct-incorrect gap, i.e. women drop their
  # confidence more on errors than men do (gap = b_acc + fem_c * b_gxa).
  bind_rows(lapply(seq_along(DOMAINS), function(i)
    eff_row(b(t_gxa[i]), paste0("gender x accuracy: ", DOMAINS[i]), ROPE, m, t_gxa[i]))),
  eff_row(avg(t_gxa), "gender x accuracy: DOMAIN-GENERAL", ROPE, m,
          paste0("(", paste(t_gxa, collapse = " + "), ")/4")),

  # domain specificity of the gender effect
  bind_rows(lapply(combn(seq_along(DOMAINS), 2, simplify = FALSE), function(k)
    eff_row(b(t_gender[k[1]]) - b(t_gender[k[2]]),
            paste0("gender x domain: ", DOMAINS[k[1]], " - ", DOMAINS[k[2]]), ROPE,
            m, paste0(t_gender[k[1]], " - ", t_gender[k[2]])))),

  # gender effect separately on correct and on incorrect trials, which is what
  # Reviewer 1 actually eyeballed off Figure 3A
  bind_rows(lapply(seq_along(DOMAINS), function(i) bind_rows(
    eff_row(b(t_gender[i]) + 0.5 * b(t_gxa[i]),
            paste0("gender on CORRECT trials: ", DOMAINS[i]), ROPE, m,
            paste0(t_gender[i], " + 0.5 * ", t_gxa[i])),
    eff_row(b(t_gender[i]) - 0.5 * b(t_gxa[i]),
            paste0("gender on INCORRECT trials: ", DOMAINS[i]), ROPE, m,
            paste0(t_gender[i], " - 0.5 * ", t_gxa[i]))))),
  eff_row(avg(t_gender) + 0.5 * avg(t_gxa), "gender on CORRECT trials: DOMAIN-GENERAL",
          ROPE, m, paste0("(", paste(t_gender, collapse = " + "), ")/4 + 0.5 * (",
                          paste(t_gxa, collapse = " + "), ")/4")),
  eff_row(avg(t_gender) - 0.5 * avg(t_gxa), "gender on INCORRECT trials: DOMAIN-GENERAL",
          ROPE, m, paste0("(", paste(t_gender, collapse = " + "), ")/4 - 0.5 * (",
                          paste(t_gxa, collapse = " + "), ")/4")),

  # the between-subject accuracy covariate itself
  eff_row(b("acc_z"), "covariate: accuracy (z, within domain)", ROPE, m, "acc_z"),
  eff_row(b("age_z"), "covariate: age (z)",  ROPE, m, "age_z"),
  eff_row(b("edu_z"), "covariate: education (z)", ROPE, m, "edu_z")
) |> mutate(model = "confidence")

print_effects(filter(eff, grepl("^gender \\(F-M\\)", effect)),
              "gender effect on mean confidence, ACCURACY CONTROLLED (F-M, conf points)")
print_effects(filter(eff, grepl("^gender x accuracy", effect)),
              "gender x accuracy = error sensitivity (positive = women drop more on errors)")
print_effects(filter(eff, grepl("^gender on", effect)),
              "gender effect split by trial accuracy")
print_effects(filter(eff, grepl("^accuracy \\(", effect)),
              "confidence gap: correct - incorrect")
print_effects(filter(eff, grepl("^gender x domain", effect)),
              "domain specificity of the gender effect")
print_effects(filter(eff, grepl("^covariate", effect)), "covariates")

# ------------------------------------------------ cell means (drives Fig 3B) ----
# Model-implied mean confidence in each domain x gender x trial-accuracy cell,
# with age, education and the between-subject accuracy covariate at their means.
cell <- function(dom, g, a) {
  b(paste0("domain", dom)) +
    g * b(paste0("domain", dom, ":fem_c")) +
    a * b(paste0("domain", dom, ":acc_c")) +
    g * a * b(paste0("domain", dom, ":fem_c:acc_c"))
}
grid <- expand.grid(domain = DOMAINS, gender = c("Men", "Women"),
                    acc = c("incorrect", "correct"), stringsAsFactors = FALSE)
cells_tab <- bind_rows(lapply(seq_len(nrow(grid)), function(i) {
  g <- ifelse(grid$gender[i] == "Women", 0.5, -0.5)
  a <- ifelse(grid$acc[i] == "correct",  0.5, -0.5)
  eff_row(cell(grid$domain[i], g, a),
          sprintf("confidence: %s, %s, %s", grid$domain[i], grid$gender[i], grid$acc[i]),
          ROPE) |> mutate(model = "confidence_cellmean")
}))
print_effects(cells_tab, "model-implied mean confidence per cell (1-7)")

# --------------------------------------------------------------- output -----
write.csv(bind_rows(eff, cells_tab), ktag("revision/data/effects_confidence.csv"),
          row.names = FALSE)
cat("\nwrote ", ktag("revision/data/effects_confidence.csv"), "\n", sep = "")
