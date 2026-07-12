# Revision analysis 03b -- the VMP replication fit
# ===========================================================================
# Fits the trivia gender contrast in the INDEPENDENT VMP sample (456 subjects), on the
# same 7-point confidence scale and with the same priors as the first-edition model, so
# the two sets of coefficients are directly comparable.
#
#   Rscript revision/code/03b_fit_vmp.R                      # full fit (long; use the sbatch)
#   Rscript revision/code/03b_fit_vmp.R --nsub 30 --iter 800 # pilot
#
# Output: revision/models/fit_vmp.rds
#
# ---------------------------------------------------------------------------
# THE FIXED EFFECTS ARE SUM-CODED, AND THIS FIXES A REAL BUG IN 03_fit_model.R.
#
# The first-edition revision model uses `gender * mod` with R's default TREATMENT coding
# and mod's reference level set to memory. Its README describes the resulting `genderFeminin`
# coefficient as the "domain-general gender difference in efficiency". It is not. Under
# treatment coding that coefficient is the gender effect AT THE REFERENCE MODALITY ONLY --
# a simple effect in the memory task. Relabelling the modalities would change its value.
# A domain-general effect is the AVERAGE over modalities, and treatment coding does not
# give you that.
#
# Here both factors are sum-coded to +/- 0.5:
#
#   fem_c = +0.5 Feminin, -0.5 Masculin
#   gdp_c = +0.5 GDP,     -0.5 Calories
#
# so that, writing the linear predictor as  Int + g*fem_c + m*gdp_c + gm*fem_c*gdp_c,
#
#   gender effect at GDP       =  g + 0.5*gm
#   gender effect at Calories  =  g - 0.5*gm
#   AVERAGE over modalities    =  g          <- `fem_c` IS the domain-general effect
#   domain-specificity         =  gm
#
# The sign convention is Feminin - Masculin, matching the published Figure 4 contrast
# direction, so the replication can be read against the first edition without flipping signs.
#
# Because the model keeps an intercept (`1 +`), the `Intercept` prior class survives and the
# first edition's priors map across UNCHANGED -- the replication is prior-matched, not merely
# structure-matched.
#
# THE RANDOM EFFECTS ARE CELL-MEANS (`0 + mod | subj`), UNLIKE THE FIXED EFFECTS.
# Fixed and random structures are independent, and each is coded for what we want to read
# off it. Cell-means random effects make cor_subj__modGDP__modCalories the between-domain
# correlation of log M -- a model parameter with a posterior, rather than something
# reconstructed from a covariance matrix of an intercept and a difference score. The sibling
# project found this correlation to be ~0 within trivia, which is worth testing here.
#
# Separate grouping ids (|m|, |dp|, |cc|) keep mu's correlation matrix a clean 2x2 rather
# than entangling log M with dprime and c in one 6x6.
#
# GENDER ENTERS EVERY DISTRIBUTIONAL PARAMETER, not just log M. M = meta-d'/d' is a ratio,
# so a gender difference in raw sensitivity should divide out -- but only if d' is FREE to
# differ by gender. The dangerous parameters are the twelve type-2 criteria: men and women
# demonstrably use the confidence scale differently here (mean confidence 4.42 vs 3.95 on
# GDP at identical accuracy), and if the criteria cannot express that, the difference has
# nowhere to go but log M, manufacturing a spurious efficiency effect. The sibling project
# has been bitten by exactly this. Any gender effect on log M reported here is therefore
# estimated CONDITIONAL ON gender effects in sensitivity, bias and confidence-scale usage.
# ---------------------------------------------------------------------------

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

suppressMessages({ library(hmetad); library(brms); library(posterior); library(dplyr) })

args   <- commandArgs(trailingOnly = TRUE)
getarg <- function(k, d) { i <- which(args == k); if (length(i)) args[i + 1] else d }
N_SUB   <- as.integer(getarg("--nsub", "0"))      # 0 = all
ITER    <- as.integer(getarg("--iter",  Sys.getenv("HMETAD_ITER",   "2000")))
CHAINS  <- as.integer(getarg("--chains", Sys.getenv("HMETAD_CHAINS", "4")))
SEED    <- as.integer(getarg("--seed", "1"))
REFRESH <- as.integer(getarg("--refresh", "50"))
BACKEND <- getarg("--backend", "rstan")
ADAPT_DELTA   <- as.numeric(getarg("--adapt-delta", "0.99"))
MAX_TREEDEPTH <- as.integer(getarg("--max-treedepth", "12"))

# THE FIXED vs RELATIVE CONFIDENCE MODEL -- fit BOTH.
#   --relative   metac_absolute = FALSE   type-2 criteria equated RELATIVELY
#   (default)    metac_absolute = TRUE    type-2 criterion fixed equal to the type-1 criterion
#
# hmetad defaults to FIXED, and 03_fit_model.R uses that default too. But O'Neill, Zhan &
# Fleming (2026) show that meta-Delta -- the bias measure 05b_effects_vmp.R reports, and the
# measure that carries this revision's headline -- is biased in a NON-LINEAR, RESPONSE-SPECIFIC
# way under the FIXED model when the data are generated by the RELATIVE model, UNLESS c = 0 or
# meta-d' = d'. Neither escape clause holds in the sibling trivia data (a credible gender
# difference in c; M != 1 in both domains), so it almost certainly does not hold here either.
#
# The bias claim is only worth making if it survives BOTH assumptions. Fit both and compare the
# meta-Delta contrast. The signature of misspecification is response-specific distortion, which
# is why 05b_effects_vmp.R reports meta-Delta separately per response side.
RELATIVE       <- "--relative" %in% args
METAC_ABSOLUTE <- !RELATIVE
TAG            <- if (RELATIVE) "relative" else "fixed"
OUT     <- getarg("--out", sprintf("revision/models/fit_vmp_%s.rds", TAG))

K <- 7L
dir.create("revision/models", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------- data ----
d <- readRDS("revision/data/hmetad_trial_vmp.rds")
stopifnot(all(d$confidence %in% 1:K), nlevels(d$gender) == 2, nlevels(d$mod) == 2)

if (N_SUB > 0 && N_SUB < nlevels(d$subj)) {
  # A pilot must contain BOTH genders or it will fit, converge, and estimate the estimand
  # from nothing. Stratify.
  set.seed(SEED)
  g   <- distinct(d, subj, gender)
  n_m <- max(1L, round(N_SUB * mean(g$gender == "Masculin")))
  keep <- c(sample(as.character(g$subj[g$gender == "Masculin"]), min(n_m, sum(g$gender == "Masculin"))),
            sample(as.character(g$subj[g$gender == "Feminin"]),  min(N_SUB - n_m, sum(g$gender == "Feminin"))))
  d <- d[d$subj %in% keep, ]
  d$subj <- droplevels(d$subj)
}

# Sum coding. See header -- this is what makes `fem_c` the domain-general effect.
d$fem_c <- ifelse(d$gender == "Feminin", 0.5, -0.5)
d$gdp_c <- ifelse(d$mod    == "GDP",     0.5, -0.5)

message(sprintf("VMP: subjects=%d (Masculin=%d Feminin=%d)  trials=%d  cells=%d  K=%d",
                nlevels(d$subj),
                n_distinct(d$subj[d$gender == "Masculin"]), n_distinct(d$subj[d$gender == "Feminin"]),
                nrow(d), nrow(distinct(d, subj, mod)), K))

# --------------------------------------------------------------- model ----
metac2 <- metac2_parameters(K = K)          # K = 7 -> 12 criterion log-spacings
rhs    <- "~ 1 + fem_c * gdp_c"
f <- bf(as.formula(paste("N",      rhs, "+ (0 + mod |m|  subj)")),
        as.formula(paste("dprime", rhs, "+ (0 + mod |dp| subj)")),
        as.formula(paste("c",      rhs, "+ (0 + mod |cc| subj)"))) +
     lf(as.formula(paste(paste(metac2, collapse = " + "), rhs, "+ (0 + mod | subj)")))

# The first edition's priors, unchanged (03_fit_model.R). They map across because the
# intercept survives sum coding.
pr <- set_prior("normal(0, 0.5)", class = "Intercept") +                        # log M
      set_prior("normal(1, 0.5)", class = "Intercept", dpar = "dprime") +
      set_prior("normal(0, 0.5)", class = "Intercept", dpar = "c") +
      set_prior("normal(-0.5, 1)", class = "Intercept", dpar = metac2) +        # log criterion spacing
      set_prior("normal(0, 0.5)", class = "b") +                                # gender / mod / interaction
      set_prior("normal(0, 0.5)", class = "b", dpar = "dprime") +
      set_prior("normal(0, 0.5)", class = "b", dpar = "c") +
      set_prior("normal(0, 0.5)", class = "b", dpar = metac2) +
      set_prior("normal(0, 1)", class = "sd") +
      set_prior("normal(0, 1)", class = "sd", dpar = c("dprime", "c", metac2)) +
      set_prior("lkj(2)", class = "cor")

message(sprintf("fitting: %d chains x %d iter, backend=%s, adapt_delta=%.2f",
                CHAINS, ITER, BACKEND, ADAPT_DELTA))
t0 <- Sys.time()
message(sprintf("CONFIDENCE MODEL: %s  (metac_absolute = %s)", toupper(TAG), METAC_ABSOLUTE))
m <- fit_metad(f, data = d, K = K, init = 0,
               metac_absolute = METAC_ABSOLUTE,      # see header: fit BOTH
               prior = pr, chains = CHAINS, iter = ITER, warmup = ITER %/% 2,
               cores = CHAINS, seed = SEED,
               refresh = REFRESH, silent = if (REFRESH > 0) 0 else 2,
               backend = BACKEND,
               control = list(adapt_delta = ADAPT_DELTA, max_treedepth = MAX_TREEDEPTH))
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

np   <- nuts_params(m)
ndiv <- sum(np$Parameter == "divergent__" & np$Value == 1)
message(sprintf("elapsed %.0fs (%.2f h)  divergences %d", elapsed, elapsed / 3600, ndiv))

# Save BEFORE summarising: hours of compute, and summary() on a fresh object can blow memory.
saveRDS(m, OUT); message("saved ", OUT)

sm <- posterior::summarise_draws(posterior::as_draws_array(m))
rh <- suppressWarnings(as.numeric(sm$rhat))
message(sprintf("R-hat: max %.4f | %d params > 1.01 | ESS_bulk min %.0f | ESS_tail min %.0f",
                max(rh, na.rm = TRUE), sum(rh > 1.01, na.rm = TRUE),
                min(sm$ess_bulk, na.rm = TRUE), min(sm$ess_tail, na.rm = TRUE)))
write.csv(sm, sprintf("revision/data/vmp_convergence_%s.csv", TAG), row.names = FALSE)

cat("\n=== population-level ===\n"); print(summary(m)$fixed, digits = 3)
cat(sprintf("\nNext: Rscript revision/code/05b_effects_vmp.R --fit %s\n", OUT))
