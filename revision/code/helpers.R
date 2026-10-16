# Revision analysis -- small shared helpers
# ===========================================================================
# Sourced by 03/04/05 AFTER they have resolved the repo root:
#
#   source("revision/code/helpers.R")
#
# Deliberately tiny. Anything that is part of the analysis lives in the
# numbered scripts, not here.

# --- command-line arguments ------------------------------------------------
getarg <- function(key, default, args = commandArgs(trailingOnly = TRUE)) {
  i <- which(args == key)
  if (length(i)) args[i[1] + 1] else default
}
hasflag <- function(key, args = commandArgs(trailingOnly = TRUE)) key %in% args

# --- posterior summary of a draws vector -----------------------------------
# One row: mean, 95% HDI, P(>0), whether the HDI excludes zero.
# mean_hdci (continuous HDI), not mean_hdi: on a multimodal posterior mean_hdi returns
# SEVERAL disjoint intervals, i.e. several rows, which silently multiplies rows in every
# table built from it and breaks any column assigned alongside.
hdi_row <- function(x, ...) {
  h <- ggdist::mean_hdci(x, .width = .95)
  stopifnot(nrow(h) == 1L)
  data.frame(..., mean = mean(x), hdi_lo = h$ymin, hdi_hi = h$ymax,
             p_gt0 = mean(x > 0), excludes0 = (h$ymin > 0) | (h$ymax < 0),
             stringsAsFactors = FALSE)
}

# --- stratified subsample of subjects (pilots) -----------------------------
# A pilot must contain both genders or it will fit, converge, and estimate the
# estimand from nothing.
subsample_subjects <- function(d, n_sub, seed = 1) {
  if (n_sub <= 0 || n_sub >= nlevels(d$subj)) return(d)
  set.seed(seed)
  g <- unique(d[, c("subj", "gender")])
  keep <- unlist(lapply(levels(d$gender), function(lv) {
    s <- as.character(g$subj[g$gender == lv])
    sample(s, min(length(s), max(1L, round(n_sub * length(s) / nrow(g)))))
  }))
  d <- d[d$subj %in% keep, ]
  d$subj <- droplevels(d$subj)
  d
}

# --- which confidence model was actually fitted ----------------------------
# Read from the fit object, never from the command line: a flag that does
# nothing does not error.
fitted_confidence_model <- function(m) {
  nm <- m$family$name                       # e.g. metad__7__normal__relative__multinomial
  parts <- strsplit(nm, "__", fixed = TRUE)[[1]]
  # hmetad's own word is "absolute"; the paper calls that model FIXED.
  list(name = nm, K = as.integer(parts[2]),
       confidence_model = if (parts[4] == "absolute") "fixed" else parts[4],
       metac_absolute = parts[4] == "absolute")
}
