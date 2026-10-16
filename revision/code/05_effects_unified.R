# Revision analysis 05 -- read the estimands off a unified fit
# ===========================================================================
#   Rscript revision/code/05_effects_unified.R --fit revision/models/fit_firstedition_fixed.rds \
#                                              --tag firstedition_fixed
#
# Writes, all computed DRAW-WISE (never by differencing two summaries -- that
# discards the correlation between them and gives the wrong interval):
#
#   revision/data/effects_<tag>_gender.csv     gender contrast on every dpar:
#                                              per domain, domain-general, gender x domain,
#                                              + Savage-Dickey BF and ROPE on log M
#   revision/data/effects_<tag>_domain.csv     main effect of domain on log M (cell-mean diffs)
#   revision/data/effects_<tag>_rho.csv        between-domain correlations of log M
#   revision/data/effects_<tag>_metadelta.csv  meta-Delta levels + Feminin - Masculin contrast
#
# ---------------------------------------------------------------------------
# WHAT THE COEFFICIENTS ARE (03_fit_unified.R, `~ 0 + mod + mod:fem_c`):
#   b_mod<X>          domain X's cell mean, averaged over genders
#   b_mod<X>:fem_c    the gender effect (Feminin - Masculin) WITHIN domain X
#   domain-general    = mean of the per-domain contrasts  <- computed here, not a coefficient
#   gender x domain   = the differences between them
#
# SIGN CONVENTION: every contrast is Feminin - Masculin. Positive = women higher.
#
# meta-Delta (O'Neill, Zhan & Fleming, 2026) is the principled measure of metacognitive
# BIAS: the distance between the average confidence criterion and meta-c, defined net of
# d', c and meta-d'. Mean confidence -- what the first edition leaned on -- is confounded
# by all three (Sherman et al., 2018). meta-Delta runs BACKWARDS relative to confidence:
# SMALLER meta-Delta = criteria packed nearer meta-c = higher confidence = more biased.
# So a POSITIVE Feminin - Masculin contrast means women are LESS biased / more conservative.
# ---------------------------------------------------------------------------

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)
source("revision/code/helpers.R")

suppressMessages({
  library(hmetad); library(brms); library(posterior); library(ggdist)
  library(dplyr); library(tidyr); library(tidybayes)
})

FIT  <- getarg("--fit", "revision/models/fit_firstedition_fixed.rds")
TAG  <- getarg("--tag", sub("\\.rds$", "", basename(FIT)))
ROPE <- as.numeric(getarg("--rope", "0.05"))     # +/- log units on log M

m  <- readRDS(FIT)
fam <- fitted_confidence_model(m)
K   <- fam$K                                      # read K off the fit, never hardcode
dr  <- as_draws_df(m)

mods   <- levels(m$data$mod)
cells  <- paste0("mod", mods)
contrs <- paste0("mod", mods, ":fem_c")
metac2 <- metac2_parameters(K = K)
DPARS  <- c(N = "log M-ratio", dprime = "d'", c = "c", setNames(metac2, metac2))

cat("fit  : ", FIT, "\n", sep = "")
cat("tag  : ", TAG, "\n", sep = "")
cat(sprintf("model: K = %d, confidence model = %s [%s]\n", K,
            toupper(fam$confidence_model), fam$name))
cat("domains: ", paste(mods, collapse = ", "), "\n", sep = "")

# A mistyped coefficient name returns NULL and silently poisons everything downstream.
co <- function(dpar, term) {
  nm <- paste0("b_", if (dpar == "N") "" else paste0(dpar, "_"), term)
  if (!nm %in% names(dr)) stop("no such coefficient: ", nm)
  dr[[nm]]
}
# brms coefficient name -> the string brms::hypothesis() wants (no b_ prefix).
hy_name <- function(dpar, term) paste0(if (dpar == "N") "" else paste0(dpar, "_"), term)

# --- Savage-Dickey BF at the point null, from the analysis prior -----------
# Needs sample_prior = "yes" at fit time. brms returns Evid.Ratio = posterior density
# at 0 / prior density at 0 = BF01 (evidence FOR the null); BF10 is its reciprocal.
sd_bf <- function(expr) {
  h <- try(brms::hypothesis(m, paste(expr, "= 0"), class = "b"), silent = TRUE)
  if (inherits(h, "try-error") || !is.finite(h$hypothesis$Evid.Ratio))
    return(c(bf01 = NA_real_, bf10 = NA_real_))
  c(bf01 = h$hypothesis$Evid.Ratio, bf10 = 1 / h$hypothesis$Evid.Ratio)
}
has_prior <- any(grepl("^prior_b", names(dr)))
if (!has_prior) warning("fit has no prior draws (sample_prior != 'yes'): BFs will be NA")

# ================================================== gender, every dpar ====
gen <- bind_rows(lapply(names(DPARS), function(p) {
  per  <- sapply(contrs, function(tm) co(p, tm))          # draws x domains
  gm   <- rowMeans(per)                                   # domain-general
  rows <- bind_rows(
    hdi_row(gm, param = unname(DPARS[p]), dpar = p,
            effect = "domain-general", term = "mean of domains"),
    bind_rows(lapply(seq_along(mods), function(i)
      hdi_row(per[, i], param = unname(DPARS[p]), dpar = p,
              effect = mods[i], term = contrs[i]))),
    # gender x domain: every pairwise difference between the per-domain effects
    if (length(mods) > 1) bind_rows(apply(combn(length(mods), 2), 2, function(ij)
      hdi_row(per[, ij[1]] - per[, ij[2]],
              param = unname(DPARS[p]), dpar = p,
              effect = paste0("gender x domain: ", mods[ij[1]], " - ", mods[ij[2]]),
              term = paste0(contrs[ij[1]], " - ", contrs[ij[2]]))))
  )
  # Bayes factors: per-domain coefficients and the domain-general linear combination.
  # Keyed by `term` so the join cannot silently misalign.
  hy <- setNames(c(paste0("(", paste(sapply(contrs, function(tm) hy_name(p, tm)),
                                     collapse = " + "), ")/", length(contrs)),
                   sapply(contrs, function(tm) hy_name(p, tm))),
                 c("mean of domains", contrs))
  bf <- t(sapply(hy, sd_bf))
  rows$bf01 <- bf[match(rows$term, rownames(bf)), "bf01"]
  rows$bf10 <- bf[match(rows$term, rownames(bf)), "bf10"]
  # ROPE and the multiplicative effect are only meaningful on the log M rows.
  rope <- c(mean(abs(gm) < ROPE),
            colMeans(abs(per) < ROPE),
            if (length(mods) > 1) apply(combn(length(mods), 2), 2,
              function(ij) mean(abs(per[, ij[1]] - per[, ij[2]]) < ROPE)))
  rows$pct_in_rope <- if (p == "N") 100 * rope else NA_real_
  rows$M_mult      <- if (p == "N") exp(rows$mean)   else NA_real_
  rows$M_mult_lo   <- if (p == "N") exp(rows$hdi_lo) else NA_real_
  rows$M_mult_hi   <- if (p == "N") exp(rows$hdi_hi) else NA_real_
  rows
}))
gen$rope_width <- ROPE
write.csv(gen, sprintf("revision/data/effects_%s_gender.csv", TAG), row.names = FALSE)

cat("\n=== gender contrast (Feminin - Masculin) on log M-ratio ===\n")
print(as.data.frame(gen |> filter(dpar == "N") |>
        select(effect, mean, hdi_lo, hdi_hi, p_gt0, excludes0, bf10, pct_in_rope, M_mult)),
      row.names = FALSE, digits = 3)

cat("\n=== gender contrast on type-1 (d', c) ===\n")
print(as.data.frame(gen |> filter(dpar %in% c("dprime", "c"), !grepl("^gender x", effect)) |>
        select(param, effect, mean, hdi_lo, hdi_hi, excludes0, bf10)),
      row.names = FALSE, digits = 3)

cat("\n=== confidence criteria whose gender contrast excludes zero ===\n")
crit <- gen |> filter(dpar %in% metac2, excludes0, !grepl("^gender x", effect))
if (nrow(crit)) print(as.data.frame(crit |> select(param, effect, mean, hdi_lo, hdi_hi)),
                      row.names = FALSE, digits = 3) else cat("  none\n")

# =================================== main effect of domain on log M =======
if (length(mods) > 1) {
  cm  <- sapply(cells, function(tm) co("N", tm))
  dom <- bind_rows(apply(combn(length(mods), 2), 2, function(ij)
    hdi_row(cm[, ij[1]] - cm[, ij[2]], param = "log M-ratio",
            effect = paste0(mods[ij[1]], " - ", mods[ij[2]]),
            term = paste0(cells[ij[1]], " - ", cells[ij[2]]))))
  lev <- bind_rows(lapply(seq_along(mods), function(i)
    hdi_row(cm[, i], param = "log M-ratio", effect = paste0("cell mean: ", mods[i]),
            term = cells[i])))
  dom <- bind_rows(lev, dom)
  write.csv(dom, sprintf("revision/data/effects_%s_domain.csv", TAG), row.names = FALSE)
  cat("\n=== main effect of domain on log M-ratio (gender-averaged cell means) ===\n")
  print(as.data.frame(dom |> select(effect, mean, hdi_lo, hdi_hi, excludes0)),
        row.names = FALSE, digits = 3)
}

# ============================= between-domain correlations of log M =======
# Cell-means random effects make these model parameters, not correlations of shrunk
# point estimates (which are upward-biased). The mu block carries no dpar prefix.
rho_nm <- grep(paste0("^cor_subj__mod(", paste(mods, collapse = "|"), ")__mod"),
               names(dr), value = TRUE)
if (length(rho_nm)) {
  rho <- bind_rows(lapply(rho_nm, function(nm)
    hdi_row(dr[[nm]], param = "rho(log M)", effect = nm, term = nm)))
  write.csv(rho, sprintf("revision/data/effects_%s_rho.csv", TAG), row.names = FALSE)
  cat("\n=== between-domain correlation of log M (domain generality) ===\n")
  print(as.data.frame(rho |> select(effect, mean, hdi_lo, hdi_hi, p_gt0, excludes0)),
        row.names = FALSE, digits = 3)
}

# ========================================================== meta-Delta ====
nd <- expand_grid(mod = factor(mods, levels = mods), fem_c = c(-0.5, 0.5))
md <- add_metacognitive_bias_draws(nd, m, re_formula = NA) |>
  ungroup() |>          # RETURNS A GROUPED TIBBLE. Without ungroup(), select() silently
  # re-adds the grouping columns, splitting the two genders onto different rows so that
  # `Feminin - Masculin` comes out NA for every cell -- with no error at all.
  mutate(gender = ifelse(fem_c > 0, "Feminin", "Masculin"))

lvl <- md |> group_by(mod, gender, response) |>
  median_qi(metacognitive_bias, .width = .95) |> ungroup() |>
  select(mod, gender, response, meta_delta = metacognitive_bias, .lower, .upper)
cat("\n=== meta-Delta levels (median, 95% QI) ===\n")
print(as.data.frame(lvl), row.names = FALSE, digits = 3)

ctr <- md |> select(mod, response, gender, .draw, metacognitive_bias) |>
  pivot_wider(names_from = gender, values_from = metacognitive_bias) |>
  mutate(diff = Feminin - Masculin)
stopifnot(!anyNA(ctr$diff))

ctr_s <- bind_rows(
  # per domain x response side (the response-specific distortion under a misspecified
  # confidence model is exactly what makes this split worth reporting)
  bind_rows(lapply(split(ctr, list(ctr$mod, ctr$response), drop = TRUE), function(x)
    hdi_row(x$diff, mod = as.character(x$mod[1]), response = as.character(x$response[1])))),
  # averaged over response sides, per domain
  bind_rows(lapply(split(ctr, ctr$mod, drop = TRUE), function(x)
    hdi_row(tapply(x$diff, x$.draw, mean), mod = as.character(x$mod[1]), response = "both"))),
  # averaged over response sides AND domains
  hdi_row(tapply(ctr$diff, ctr$.draw, mean), mod = "domain-general", response = "both")
)
cat("\n=== meta-Delta gender contrast (Feminin - Masculin) ===\n")
cat("    POSITIVE = women's criteria sit FURTHER from meta-c = women use LOWER confidence\n")
cat("               = women LESS metacognitively biased / more conservative\n")
print(as.data.frame(ctr_s), row.names = FALSE, digits = 3)

write.csv(bind_rows(
  lvl   |> mutate(mod = as.character(mod), response = as.character(response),
                  quantity = "level"),
  ctr_s |> mutate(quantity = "contrast_Feminin_minus_Masculin")
), sprintf("revision/data/effects_%s_metadelta.csv", TAG), row.names = FALSE)

cat(sprintf("\nwrote revision/data/effects_%s_{gender,domain,rho,metadelta}.csv\n", TAG))
