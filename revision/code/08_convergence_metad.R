# Revision analysis 08 -- convergence of the REPORTED coefficients
# ===========================================================================
#   Rscript revision/code/08_convergence_metad.R
#
# A max-Rhat over the whole unified fit is a statistic about ~15,000 subject-level
# random effects, most of which nothing in the paper reports. What has to converge
# is what we quote: the population-level coefficients of every dpar (domain cell
# means and gender contrasts), the between-domain correlations of log M, and the
# random-effect SDs. This script summarises exactly those, per fit.
#
# Output  revision/data/convergence_metad.csv   one row per fit x variable
#         plus a per-fit and per-parameter-block summary on the console.
#
# Fits are read one at a time and dropped again: four of them do not fit in memory
# together.

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)
source("revision/code/helpers.R")

suppressMessages({ library(brms); library(posterior); library(dplyr) })

FITS <- c(fe_fixed     = "revision/models/fit_firstedition_fixed.rds",
          fe_relative  = "revision/models/fit_firstedition_relative.rds",
          vmp_fixed    = "revision/models/fit_vmp_fixed.rds",
          vmp_relative = "revision/models/fit_vmp_relative.rds")

one_fit <- function(tag, path) {
  cat("reading ", path, " ...\n", sep = "")
  m   <- readRDS(path)
  fam <- fitted_confidence_model(m)
  dr  <- as_draws_array(m)
  vars <- grep("^(b_|sd_subj|cor_subj)", variables(m), value = TRUE)
  vars <- vars[!grepl("^prior_", vars)]
  s <- summarise_draws(subset_draws(dr, variable = vars), "rhat", "ess_bulk", "ess_tail")
  rm(m, dr); gc()
  s |>
    mutate(tag = tag, confidence_model = fam$confidence_model,
           # Only the log M-ratio correlations are reported (effects_<tag>_rho.csv);
           # the criterion-dpar correlations are nuisance and are labelled as such, so a
           # bad Rhat among them is not mistaken for a bad Rhat on a reported quantity.
           block = case_when(grepl("^cor_subj__mod", variable) ~ "between-domain correlation (log M)",
                             grepl("^cor_subj",      variable) ~ "correlation, other dpars (not reported)",
                             grepl("^sd_subj",       variable) ~ "random-effect SD",
                             grepl(":fem_c$",        variable) ~ "gender contrast",
                             TRUE                              ~ "domain cell mean"),
           dpar = ifelse(grepl("^b_", variable),
                         ifelse(grepl("^b_mod", variable), "N",
                                sub("^b_([^_]+)_.*$", "\\1", variable)), NA_character_)) |>
    select(tag, confidence_model, block, dpar, variable, rhat, ess_bulk, ess_tail)
}

conv <- bind_rows(Map(one_fit, names(FITS), FITS))
write.csv(conv, "revision/data/convergence_metad.csv", row.names = FALSE)

cat("\n=== convergence of the reported coefficients, per fit ===\n")
print(as.data.frame(conv |> filter(block != "correlation, other dpars (not reported)") |>
        group_by(tag, confidence_model) |>
        summarise(n = n(), max_rhat = max(rhat), min_ess_bulk = min(ess_bulk),
                  min_ess_tail = min(ess_tail), n_rhat_gt_101 = sum(rhat > 1.01),
                  .groups = "drop")), row.names = FALSE, digits = 4)

cat("\n=== by parameter block ===\n")
print(as.data.frame(conv |> group_by(tag, block) |>
        summarise(n = n(), max_rhat = max(rhat), min_ess_bulk = round(min(ess_bulk)),
                  .groups = "drop")), row.names = FALSE, digits = 4)

cat("\n=== the log M-ratio coefficients specifically (what Figure 4 shows) ===\n")
print(as.data.frame(conv |> filter(dpar == "N") |>
        select(tag, variable, rhat, ess_bulk, ess_tail)),
      row.names = FALSE, digits = 4)

cat("\nwrote revision/data/convergence_metad.csv\n")
