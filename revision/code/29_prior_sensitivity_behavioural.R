# Revision analysis 29 -- how much does the CONTRAST prior matter (scripts 21-26)?
# =============================================================================
# The behavioural models put normal(0, s) on cell means and covariates and
# normal(0, k*s) on every contrast coefficient, s = SD(DV), k = 0.5 reported.
# This script refits all of 21-26 at k = 0.25 and k = 1 (utils_bayes.R reads
# CONTRAST_SCALE from the environment and ktag()s the outputs) and collects the
# headline contrasts at the three widths.
#
# As in 09_prior_sensitivity.R: the MEAN and HDI move only through shrinkage, so
# their stability is the evidence that the data and not the prior are doing the
# work; the Savage-Dickey BF10 must move, because it divides by the prior density
# at zero (Lindley). What would matter is a BF crossing 1, or crossing 3 in the
# direction a claim depends on.
#
# -> revision/data/prior_sensitivity_behavioural.csv
#
# Run from anywhere:  Rscript revision/code/29_prior_sensitivity_behavioural.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)
suppressMessages({ library(dplyr) })

KS      <- c(0.25, 0.5, 1)          # 0.5 is the reported fit, already on disk
SCRIPTS <- c("21_global_calibration", "22_local_confidence", "22b_local_confidence_vmp",
             "23_reaction_time", "24_staircase", "25_local_global_link",
             "26_positive_controls")
FILES   <- c(calibration       = "effects_calibration.csv",
             confidence        = "effects_confidence.csv",
             confidence_vmp    = "effects_confidence_vmp.csv",
             rt                = "effects_rt.csv",
             staircase         = "effects_staircase.csv",
             local_global      = "effects_local_global.csv",
             positive_controls = "effects_positive_controls.csv")
# domain-general and per-domain gender, and the gender interactions
HEAD <- paste("^gender \\(F-M\\)", "^gender x time: DOMAIN-GENERAL",
              "^gender x accuracy.*DOMAIN-GENERAL", "^coupling x gender.*DOMAIN-GENERAL",
              sep = "|")

# ------------------------------------------------------------------ refit ----
for (k in setdiff(KS, 0.5)) for (sc in SCRIPTS) {
  cat(sprintf("\n===== %s at k = %s =====\n", sc, k)); flush(stdout())
  st <- system2("Rscript", sprintf("revision/code/%s.R", sc),
                env = sprintf("CONTRAST_SCALE=%s", k))
  if (st != 0L) stop(sc, " failed at k = ", k)
}

# ---------------------------------------------------------------- collect ----
read_k <- function(analysis, file, k) {
  p <- file.path("revision/data",
                 if (k == 0.5) file else sub("\\.csv$", sprintf("_k%s.csv", k), file))
  read.csv(p, stringsAsFactors = FALSE) |>
    filter(grepl(HEAD, effect)) |>
    transmute(analysis, model, effect, k, mean, hdi_lo, hdi_hi, BF10, pct_rope)
}
out <- bind_rows(lapply(KS, function(k)
  bind_rows(Map(read_k, names(FILES), FILES, k)))) |>
  arrange(analysis, model, effect, k)
write.csv(out, "revision/data/prior_sensitivity_behavioural.csv", row.names = FALSE)

# ------------------------------------------------------------------ print ----
fmt <- function(x) as.data.frame(x |> transmute(
  effect, k, mean = sprintf("%+.3f", mean),
  `95% HDI` = sprintf("[%+.3f, %+.3f]", hdi_lo, hdi_hi),
  BF10 = ifelse(BF10 >= 1000 | BF10 < 0.01, sprintf("%.1e", BF10), sprintf("%.2f", BF10)),
  `% ROPE` = sprintf("%.1f", pct_rope)))
op <- options(width = 200); on.exit(options(op))
for (a in unique(out$analysis)) {
  cat(sprintf("\n=== %s: headline contrasts vs the contrast-prior scale k ===\n", a))
  print(fmt(filter(out, analysis == a)), row.names = FALSE)
}
cat("\nwrote revision/data/prior_sensitivity_behavioural.csv\n")
