# Revision helpers -- Bayesian effect summaries for the light gaussian models
# ===========================================================================
# Sourced by revision/code/2*.R and the figure scripts. Deliberately tiny:
# a handful of functions, no classes, no side effects.
#
#   fit_light_model()  fit a gaussian brms model with the revision's standard
#                      priors (normal(0, s) on cell means, normal(0, k*s) on
#                      contrasts, normal(0, s) on sd, student_t(3, 0, s) on
#                      sigma, sample_prior = "yes")
#   ktag()             filename suffix for a non-default contrast scale
#
# PRIOR SCALING. s = SD of the DV. A cell mean can be anywhere the DV can be, so
# it keeps normal(0, s); the same goes for the standardised covariates age_z,
# edu_z and acc_z, whose slopes are per-SD shifts of the DV. A CONTRAST -- any
# coefficient carrying a sum-coded predictor (fem_c, time_c, acc_c, or an
# interaction of them) -- cannot plausibly be as large as the whole spread of
# the DV, so it gets normal(0, k * s) with k = `contrast_scale` (default 0.5;
# revision/code/29_prior_sensitivity_behavioural.R sweeps k = 0.25 / 0.5 / 1).
# This matters for the Savage-Dickey BF10, which divides by the prior density at
# zero: an over-wide contrast prior mechanically drives BF10 towards 0 for small
# but real effects (Lindley). Posterior means and HDIs are barely touched by it.
# Every coefficient is given its prior BY NAME (the coef column of get_prior()),
# and the fitted model is checked against prior_summary() so that none can have
# quietly fallen back on a brms default.
#
# HETEROSCEDASTICITY. Every model below lets the residual SD vary over the
# within-subject design cells (`sigma ~ 0 + domain`, or `0 + domain:acc` where
# there is an accuracy split). The four tasks differ enormously in spread --
# between-subject SD of accuracy is 1.9 percentage points for vision and 8.8
# for memory -- so a single pooled sigma would make the memory contrast look
# far more precise than it is and the vision contrast far less. brms uses a log
# link on sigma, so its coefficients get a normal(log(s), 1) prior.
#   eff_row()          posterior mean, 95% HDI, P(>0), % in ROPE for a vector
#                      of draws, optionally with a Savage-Dickey BF10
#   bf10()             Savage-Dickey BF10 from brms::hypothesis()
#   print_effects()    compact console table
#
# CONVENTIONS USED THROUGHOUT
#   - sum coding of two-level factors to +/-0.5, so a coefficient is the plain
#     difference between the two levels and its "main effect" is the average
#     over the other sum-coded factors;
#   - cell-means coding for domain (`0 + domain`), so the gender effect is
#     estimated inside each domain as `domain<X>:fem_c`;
#   - the DOMAIN-GENERAL effect is the draw-wise mean of the per-domain
#     effects, never a separate coefficient;
#   - DOMAIN SPECIFICITY is the set of pairwise differences between them.
#
# BAYES FACTORS. brms::hypothesis() returns Evid.Ratio = the Savage-Dickey
# density ratio posterior(0)/prior(0), i.e. BF01. We report BF10 = 1/BF01.
# For this to be correct the prior must be sampled coefficient-by-coefficient,
# which is why fit_light_model() calls set_prior(..., coef = ) once per
# coefficient rather than setting one prior at the class level.
#
# ROPE. Region of practical equivalence = +/-0.1 x SD(DV), reported as the
# percentage of the 95% HDI that falls inside it (bayestestR convention).

suppressMessages({
  library(brms); library(posterior); library(ggdist)
  library(bayestestR); library(dplyr); library(tibble)
})

# --- fitting ---------------------------------------------------------------
# The contrast scale k. Default 0.5 = the reported analysis; the sensitivity
# sweep sets CONTRAST_SCALE in the environment instead of editing any script.
CONTRAST_SCALE <- as.numeric(Sys.getenv("CONTRAST_SCALE", "0.5"))

# A coefficient is a CONTRAST if its name carries a sum-coded predictor; cell
# means (`domain<X>`) and the standardised covariates are not.
is_contrast_coef <- function(coef) grepl("fem_c|time_c|acc_c", coef)

# Non-default k writes to its own model and effects files, so the reported
# k = 0.5 results are never overwritten by the sweep.
ktag <- function(path) {
  if (isTRUE(all.equal(CONTRAST_SCALE, 0.5))) return(path)
  ext <- tools::file_ext(path)
  paste0(tools::file_path_sans_ext(path), "_k", CONTRAST_SCALE,
         if (nzchar(ext)) paste0(".", ext) else "")
}

# s = the SD of the dependent variable; every prior is scaled by it, and
# contrast coefficients additionally by contrast_scale.
fit_light_model <- function(formula, data, s, file, seed = 2026,
                            chains = 4, cores = 4, iter = 4000, warmup = 1000,
                            adapt_delta = 0.95, contrast_scale = CONTRAST_SCALE) {
  fs  <- format(s, digits = 4)
  fks <- format(contrast_scale * s, digits = 4)
  gp  <- brms::get_prior(formula, data)
  gp  <- gp[gp$class != "Intercept", , drop = FALSE]  # cell-means: no Intercept term

  # Every mu coefficient by name: cell means and covariates at normal(0, s),
  # contrasts at normal(0, k * s).
  mu <- gp$class == "b" & gp$dpar == "" & nzchar(gp$coef)
  pr <- do.call(c, lapply(gp$coef[mu], function(cf)
    brms::set_prior(sprintf("normal(0, %s)", if (is_contrast_coef(cf)) fks else fs),
                    class = "b", coef = cf)))
  pr <- pr + brms::set_prior(sprintf("normal(0, %s)", fs), class = "sd")
  # a pooled sigma only exists when the model does not predict sigma
  if (any(gp$class == "sigma"))
    pr <- pr + brms::set_prior(sprintf("student_t(3, 0, %s)", fs), class = "sigma")
  # cell-wise residual SD (`sigma ~ 0 + ...`): brms uses a log link here, so the
  # prior is centred on log(SD of the DV) rather than on 0.
  if (any(gp$class == "b" & gp$dpar == "sigma"))
    pr <- pr + brms::set_prior(sprintf("normal(%s, 1)", format(log(s), digits = 4)),
                               class = "b", dpar = "sigma")

  cat(sprintf("priors: normal(0, %s) on %d cell means/covariates, normal(0, %s) on %d contrasts (k = %s)\n",
              fs, sum(mu & !is_contrast_coef(gp$coef)), fks,
              sum(mu & is_contrast_coef(gp$coef)), format(contrast_scale)))

  backend <- tryCatch({ cmdstanr::cmdstan_path(); "cmdstanr" }, error = function(e) "rstan")
  message("brms backend: ", backend)

  fit <- brms::brm(
    formula, data = data, family = gaussian(), prior = pr,
    sample_prior = "yes", chains = chains, cores = cores,
    iter = iter, warmup = warmup, seed = seed,
    control = list(adapt_delta = adapt_delta),
    backend = backend, file = file, file_refit = "on_change", refresh = 500
  )

  # No mu coefficient may be on a brms default: Savage-Dickey needs a proper
  # prior for every one of them, and a flat default would silently give none.
  # (sigma's coefficients inherit the dpar-level normal(log s, 1), also ours.)
  ps  <- brms::prior_summary(fit)
  bad <- ps$class == "b" & ps$dpar == "" & nzchar(ps$coef) &
         !(nzchar(ps$prior) & ps$source == "user")
  if (any(bad)) stop("coefficients left on a brms default prior: ",
                     paste(ps$coef[bad], collapse = ", "))
  fit
}

# --- Savage-Dickey BF10 ----------------------------------------------------
# `hyp` is a brms hypothesis string WITHOUT the "= 0", e.g.
#   "domainmemory:fem_c"  or  "(domainmemory:fem_c + domainvision:fem_c)/2"
bf10 <- function(fit, hyp) {
  out <- tryCatch(
    brms::hypothesis(fit, paste0(hyp, " = 0"))$hypothesis$Evid.Ratio,
    error = function(e) { warning("BF failed for: ", hyp, " (", conditionMessage(e), ")"); NA_real_ })
  if (is.na(out)) return(NA_real_)
  if (out <= 0) return(Inf)   # posterior density at 0 numerically zero
  1 / out                     # Evid.Ratio is BF01; we report BF10
}

# --- one row of the results table -----------------------------------------
# x    : vector of posterior draws of the effect
# rope : half-width of the ROPE (already on the DV scale)
eff_row <- function(x, effect, rope, fit = NULL, hyp = NULL, model = NA_character_) {
  h <- ggdist::mean_hdi(x, .width = .95)
  r <- suppressWarnings(bayestestR::rope(x, range = c(-rope, rope), ci = 0.95))
  tibble::tibble(
    model    = model,
    effect   = effect,
    mean     = mean(x),
    hdi_lo   = h$ymin,
    hdi_hi   = h$ymax,
    p_gt0    = mean(x > 0),
    pct_rope = as.numeric(r$ROPE_Percentage) * 100,
    BF10     = if (!is.null(fit) && !is.null(hyp)) bf10(fit, hyp) else NA_real_,
    excl0    = (h$ymin > 0) | (h$ymax < 0)
  )
}

# --- console table ---------------------------------------------------------
print_effects <- function(tab, title = NULL, digits = 3) {
  op <- options(width = 200); on.exit(options(op))
  if (!is.null(title)) cat("\n===", title, "===\n")
  out <- as.data.frame(tab[, intersect(
    c("effect", "mean", "hdi_lo", "hdi_hi", "p_gt0", "BF10", "pct_rope", "excl0"),
    names(tab)), drop = FALSE])
  print(out, row.names = FALSE, digits = digits)
  invisible(tab)
}

# --- convergence of the reported coefficients ------------------------------
check_convergence <- function(fit) {
  s <- posterior::summarise_draws(
    posterior::subset_draws(posterior::as_draws_array(fit),
                            variable = "^b_", regex = TRUE),
    "rhat", "ess_bulk", "ess_tail")
  cat(sprintf("convergence over %d population-level coefficients: max Rhat = %.4f, min ESS bulk = %.0f, min ESS tail = %.0f\n",
              nrow(s), max(s$rhat, na.rm = TRUE),
              min(s$ess_bulk, na.rm = TRUE), min(s$ess_tail, na.rm = TRUE)))
  invisible(s)
}
