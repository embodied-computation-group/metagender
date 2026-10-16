# Revision analysis 09 -- how much does the gender-contrast prior matter?
# ===========================================================================
#   Rscript revision/code/09_prior_sensitivity.R
#
# The reported gender effects are estimated under normal(0, 0.5) on every
# mod<X>:fem_c coefficient. 03_fit_unified.R's --contrast-sd refits the same model
# with that SD alone changed, so the same estimands can be read off three prior
# widths and compared. Collects, per sample x prior SD:
#
#   log M-ratio  the gender contrast, domain-general and per domain
#                (mean, 95% HDI, Savage-Dickey BF10, % inside the +/-0.05 ROPE)
#   meta-Delta   the domain-general gender contrast (no ROPE: the +/-0.05 band is
#                defined on log units of M and means nothing on the criterion scale)
#
# -> revision/data/prior_sensitivity.csv
#
# A prior-width sweep moves two things for different reasons and they should not be
# read the same way. The POSTERIOR MEAN and HDI move only through shrinkage, so a
# stable mean is evidence the data, not the prior, are doing the work. The BAYES
# FACTOR is EXPECTED to move and its movement is not instability: Savage-Dickey
# compares the posterior at zero against THAT prior's density at zero, so widening
# the prior spreads the alternative over effect sizes these data rule out and
# PENALISES it (Lindley/Bartlett) whenever the effect is small relative to the prior
# width -- which is the case for every log M contrast here. A BF that falls as the
# prior widens therefore says the effect is small, not that the analysis is fragile.
# What would matter is a BF crossing 1, or crossing 3 in the direction a claim in the
# paper depends on. (For meta-Delta, whose posterior sits many SDs from zero, the BF
# is a ratio of tail densities and its exact value is estimation noise; only its
# order of magnitude is meaningful.)
#
# Requires 05_effects_unified.R and 07_contrast_draws.R to have been run on each fit.
# ---------------------------------------------------------------------------

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)
source("revision/code/helpers.R")
suppressMessages({ library(dplyr) })

# tag -> sample x prior SD. 0.5 is the reported analysis; its tag carries no suffix.
grid <- expand.grid(prior_sd = c(0.2, 0.5, 1.0),
                    sample   = c("first edition", "VMP replication"),
                    stringsAsFactors = FALSE) |>
  mutate(base = ifelse(sample == "first edition", "fe_fixed", "vmp_fixed"),
         tag  = ifelse(prior_sd == 0.5, base, sprintf("%s_csd%.1f", base, prior_sd)))

read_one <- function(tag, sample, prior_sd) {
  gen <- sprintf("revision/data/effects_%s_gender.csv", tag)
  lab <- sprintf("revision/data/labels_%s.csv", tag)                 # meta-Delta's BF10
  for (f in c(gen, lab))
    if (!file.exists(f)) stop("missing ", f, " -- run 05 and 07 on tag ", tag)

  # log M: straight out of 05's table, so this cannot disagree with the main tables.
  lm <- read.csv(gen, stringsAsFactors = FALSE) |>
    filter(dpar == "N", !grepl("^gender x", effect)) |>
    transmute(quantity = "log M-ratio", effect, mean, hdi_lo, hdi_hi, bf10, pct_in_rope)

  # meta-Delta: 05 reports no BF for it (nonlinear in twelve criterion parameters);
  # 07 computes the Savage-Dickey ratio through hmetad's closed form. Cross-checked
  # against 05's own posterior summary so the two files cannot have drifted apart.
  md <- read.csv(lab, stringsAsFactors = FALSE) |>
    filter(quantity == "metadelta", domain == "domain-general") |>
    transmute(quantity = "meta-Delta", effect = "domain-general",
              mean, hdi_lo, hdi_hi, bf10, pct_in_rope = NA_real_)
  ref <- read.csv(sprintf("revision/data/effects_%s_metadelta.csv", tag),
                  stringsAsFactors = FALSE) |>
    filter(quantity == "contrast_Feminin_minus_Masculin",
           mod == "domain-general", response == "both")
  stopifnot(nrow(md) == 1L, nrow(ref) == 1L, abs(md$mean - ref$mean) < 1e-6)

  bind_rows(lm, md) |> mutate(sample = sample, prior_sd = prior_sd, tag = tag) |>
    select(sample, prior_sd, tag, quantity, effect, everything())
}

out <- bind_rows(Map(read_one, grid$tag, grid$sample, grid$prior_sd))
write.csv(out, "revision/data/prior_sensitivity.csv", row.names = FALSE)

# ----------------------------------------------------------------- print ----
fmt <- function(d) as.data.frame(d |> transmute(
  effect, prior_sd,
  mean    = sprintf("%+.3f", mean),
  `95% HDI` = sprintf("[%+.3f, %+.3f]", hdi_lo, hdi_hi),
  BF10    = ifelse(bf10 >= 1000 | bf10 < 0.01, sprintf("%.1e", bf10), sprintf("%.2f", bf10)),
  `% ROPE` = ifelse(is.na(pct_in_rope), "--", sprintf("%.1f", pct_in_rope))))

for (s in unique(out$sample)) {
  cat(sprintf("\n=== %s: gender contrast (Feminin - Masculin) vs the SD of its prior ===\n", s))
  x <- out |> filter(sample == s)
  for (q in unique(x$quantity)) {
    cat(sprintf("\n-- %s --\n", q))
    print(fmt(x |> filter(quantity == q) |>
                arrange(effect != "domain-general", effect, prior_sd)), row.names = FALSE)
  }
}
cat("\nThe mean and HDI are what should stay put. BF10 falling as the prior widens is the",
    "\nexpected Lindley penalty on a small effect, not instability; for meta-Delta the BF is a",
    "\nratio of far-tail densities, so only its order of magnitude means anything.",
    "\n\nwrote revision/data/prior_sensitivity.csv\n")
