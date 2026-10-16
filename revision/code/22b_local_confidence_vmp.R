# Revision analysis 22b -- local confidence in the VMP replication sample
# =============================================================================
# Model 22 refitted on 457 independent participants (Visceral Mind Project) who
# performed the SAME GDP and Calories trivia tasks on the SAME 7-point confidence
# scale. It is the behavioural half of the replication: script 05 replicates the
# meta-d' result, this replicates the confidence result.
#
# The model is model 22 with two forced changes and nothing else:
#   - two domains (GDP, Calories) instead of four -- the formula is cell-means
#     coded, so it takes whatever domains the data hold;
#   - NO education covariate: the VMP phenotype file has no years of education.
#     Age is kept. (Recorded here rather than silently dropped.)
#
# DV       mean confidence on the 1-7 scale, per subject x domain x accuracy.
# Model    conf ~ 0 + domain + domain:fem_c + domain:acc_c + domain:fem_c:acc_c
#                   + acc_z + age_z + (1 | subj),
#          sigma ~ 0 + domain:acc
# Coding   fem_c = +0.5 Feminin / -0.5 Masculin; acc_c = +0.5 correct / -0.5 incorrect;
#          acc_z = the subject's overall accuracy in that domain, z-scored within domain.
# Priors   s = SD(DV). Cell means and the standardised covariates ~ normal(0, s);
#          CONTRAST coefficients (anything on fem_c or acc_c) ~ normal(0, k*s) with
#          k = 0.5; sd ~ normal(0, s); sigma ~ student_t(3, 0, s);
#          sample_prior = "yes" so Savage-Dickey BF10 is available for every
#          effect. k is swept over 0.25 / 0.5 / 1 by
#          29_prior_sensitivity_behavioural.R -> prior_sensitivity_behavioural.csv.
# ROPE     +/- 0.1 * s confidence points, as everywhere else in the revision.
#
# Input   revision/data/cells_vmp.rds     (20b_prep_cells_vmp.R)
# Output  revision/models/m22b_confidence_vmp.rds   (gitignored)
#         revision/data/effects_confidence_vmp.csv
#
# Run from anywhere:  Rscript revision/code/22b_local_confidence_vmp.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

source("revision/code/utils_bayes.R")
suppressMessages({ library(dplyr); library(tidyr) })

DOMAINS <- c("GDP", "Calories")
TAG     <- "confidence_vmp"

# ------------------------------------------------------------- long data ----
cells <- readRDS("revision/data/cells_vmp.rds")

dat <- cells |>
  select(subj, gender, fem_c, domain, age_z, acc_z, conf_cor, conf_inc) |>
  pivot_longer(c(conf_cor, conf_inc), names_to = "acc",
               names_prefix = "conf_", values_to = "conf") |>
  mutate(acc   = factor(acc, levels = c("inc", "cor")),
         acc_c = ifelse(acc == "cor", 0.5, -0.5))

s    <- sd(dat$conf)
ROPE <- 0.1 * s
cat(sprintf("VMP: N = %d subjects, %d rows; SD(mean confidence) = %.3f; ROPE = +/-%.3f\n",
            nlevels(dat$subj), nrow(dat), s, ROPE))

# ----------------------------------------------------------------- model ----
dir.create("revision/models", showWarnings = FALSE, recursive = TRUE)
form <- bf(conf ~ 0 + domain + domain:fem_c + domain:acc_c + domain:fem_c:acc_c +
                  acc_z + age_z + (1 | subj),
           sigma ~ 0 + domain:acc)

m <- fit_light_model(form, dat, s = s, file = ktag("revision/models/m22b_confidence_vmp"))
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
mean_hyp <- function(tt) paste0("(", paste(tt, collapse = " + "), ")/", length(tt))

# ------------------------------------------------------------- effects ------
eff <- bind_rows(
  bind_rows(lapply(seq_along(DOMAINS), function(i)
    eff_row(b(t_gender[i]), paste0("gender (F-M): ", DOMAINS[i]), ROPE, m, t_gender[i]))),
  eff_row(avg(t_gender), "gender (F-M): DOMAIN-GENERAL", ROPE, m, mean_hyp(t_gender)),

  bind_rows(lapply(seq_along(DOMAINS), function(i)
    eff_row(b(t_acc[i]), paste0("accuracy (cor-inc): ", DOMAINS[i]), ROPE, m, t_acc[i]))),
  eff_row(avg(t_acc), "accuracy (cor-inc): DOMAIN-GENERAL", ROPE, m, mean_hyp(t_acc)),

  # gender x accuracy = ERROR SENSITIVITY. POSITIVE = women's correct-incorrect
  # confidence gap is LARGER, i.e. women drop confidence more on errors.
  bind_rows(lapply(seq_along(DOMAINS), function(i)
    eff_row(b(t_gxa[i]), paste0("gender x accuracy: ", DOMAINS[i]), ROPE, m, t_gxa[i]))),
  eff_row(avg(t_gxa), "gender x accuracy: DOMAIN-GENERAL", ROPE, m, mean_hyp(t_gxa)),

  # domain specificity of the gender effect (one pair here)
  bind_rows(lapply(combn(seq_along(DOMAINS), 2, simplify = FALSE), function(k)
    eff_row(b(t_gender[k[1]]) - b(t_gender[k[2]]),
            paste0("gender x domain: ", DOMAINS[k[1]], " - ", DOMAINS[k[2]]), ROPE,
            m, paste0(t_gender[k[1]], " - ", t_gender[k[2]])))),

  bind_rows(lapply(seq_along(DOMAINS), function(i) bind_rows(
    eff_row(b(t_gender[i]) + 0.5 * b(t_gxa[i]),
            paste0("gender on CORRECT trials: ", DOMAINS[i]), ROPE, m,
            paste0(t_gender[i], " + 0.5 * ", t_gxa[i])),
    eff_row(b(t_gender[i]) - 0.5 * b(t_gxa[i]),
            paste0("gender on INCORRECT trials: ", DOMAINS[i]), ROPE, m,
            paste0(t_gender[i], " - 0.5 * ", t_gxa[i]))))),
  eff_row(avg(t_gender) + 0.5 * avg(t_gxa), "gender on CORRECT trials: DOMAIN-GENERAL",
          ROPE, m, paste0(mean_hyp(t_gender), " + 0.5 * ", mean_hyp(t_gxa))),
  eff_row(avg(t_gender) - 0.5 * avg(t_gxa), "gender on INCORRECT trials: DOMAIN-GENERAL",
          ROPE, m, paste0(mean_hyp(t_gender), " - 0.5 * ", mean_hyp(t_gxa))),

  eff_row(b("acc_z"), "covariate: accuracy (z, within domain)", ROPE, m, "acc_z"),
  eff_row(b("age_z"), "covariate: age (z)", ROPE, m, "age_z")
) |> mutate(model = TAG)

print_effects(filter(eff, grepl("^gender \\(F-M\\)", eff$effect)),
              "VMP: gender effect on mean confidence, ACCURACY CONTROLLED (F-M, conf points)")
print_effects(filter(eff, grepl("^gender x accuracy", eff$effect)),
              "VMP: gender x accuracy = error sensitivity")
print_effects(filter(eff, grepl("^gender on", eff$effect)),
              "VMP: gender effect split by trial accuracy")
print_effects(filter(eff, grepl("^accuracy \\(", eff$effect)),
              "VMP: confidence gap correct - incorrect")
print_effects(filter(eff, grepl("^gender x domain", eff$effect)),
              "VMP: domain specificity of the gender effect")
print_effects(filter(eff, grepl("^covariate", eff$effect)), "VMP: covariates")

# ------------------------------------------------ cell means (drives Fig S3) ----
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
          ROPE) |> mutate(model = paste0(TAG, "_cellmean"))
}))
print_effects(cells_tab, "VMP: model-implied mean confidence per cell (1-7)")

# --------------------------------------------------------------- output -----
write.csv(bind_rows(eff, cells_tab),
          ktag(sprintf("revision/data/effects_%s.csv", TAG)), row.names = FALSE)
cat("\nwrote ", ktag(sprintf("revision/data/effects_%s.csv", TAG)), "\n", sep = "")
