# Revision analysis 03 -- fit the unified hierarchical meta-d' model -------
# =========================================================================
# Single hierarchical model fit to ALL data with gender as a predictor,
# replacing the deprecated MATLAB HMeta-d workflow (separate male/female
# group fits per domain, subtracting mu_logMratio posteriors).
#
#   Fixed effects : gender * mod  on every parameter
#   Random effects: (1 + mod | subj) correlated slopes on every parameter  [MAXIMAL]
#
# Parameter of interest: gender (and gender:mod) on the log M-ratio (mu):
#   * gender main effect      -> domain-general gender difference in efficiency
#   * gender:mod interactions -> domain-specific gender differences
#
# K = 7 confidence levels -> 15 distributional parameters
#   (mu = log M, dprime, c, metac2zero1-6diff, metac2one1-6diff).
#
# NOTE: heavy fit (15 correlated 4x4 random-effect matrices). Intended for a
#       cluster / long run. Set chains/cores to the machine below.
#
# Input  : revision/data/hmetad_trial.rds   (run 01_prep_data.R first)
# Output : revision/models/fit_full_maximal.rds
#          revision/logs/ (redirect stdout here when running detached)
#
# Run from anywhere:  Rscript revision/code/03_fit_model.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

suppressMessages({ library(dplyr); library(brms); library(hmetad) })

# ---- sampler resources (override via env vars on the cluster) --------------
CHAINS <- as.integer(Sys.getenv("HMETAD_CHAINS", "4"))
CORES  <- as.integer(Sys.getenv("HMETAD_CORES",  as.character(CHAINS)))
ITER   <- as.integer(Sys.getenv("HMETAD_ITER",   "2000"))
WARMUP <- as.integer(Sys.getenv("HMETAD_WARMUP", "1000"))

stopifnot(file.exists("revision/data/hmetad_trial.rds"))
d <- readRDS("revision/data/hmetad_trial.rds")
cat("Full data:", nrow(d), "trials,", nlevels(d$subj), "subjects,",
    nlevels(d$mod), "modalities\n")
cat("gender ref:", levels(d$gender)[1], "| mod ref:", levels(d$mod)[1], "\n")
cat("sampler:", CHAINS, "chains,", CORES, "cores,", WARMUP, "warmup /", ITER, "iter\n")

K    <- 7L
crit <- c(paste0("metac2zero", 1:6, "diff"), paste0("metac2one", 1:6, "diff"))

# --- formula: gender*mod fixed + correlated mod random slopes on ALL params --
rhs  <- "gender * mod + (1 + mod | subj)"
form <- bf(as.formula(paste("N ~", rhs)))
for (p in c("dprime", "c", crit)) form <- form + lf(as.formula(paste(p, "~", rhs)))

# --- weakly-informative priors (latent scale; see revision/reference/) ------
pr <- c(
  set_prior("normal(0, 0.5)", class = "Intercept"),                 # log M-ratio ~ 0
  set_prior("normal(1, 0.5)", class = "Intercept", dpar = "dprime"),# d' ~ 1
  set_prior("normal(0, 0.5)", class = "Intercept", dpar = "c"),     # c ~ 0
  set_prior("normal(0, 0.5)", class = "b"),                         # gender/mod on log M
  set_prior("normal(0, 0.5)", class = "b", dpar = "dprime"),
  set_prior("normal(0, 0.5)", class = "b", dpar = "c"),
  set_prior("normal(0, 1)",   class = "sd"),                        # between-subject SDs
  set_prior("normal(0, 1)",   class = "sd", dpar = "dprime"),
  set_prior("normal(0, 1)",   class = "sd", dpar = "c"),
  set_prior("lkj(2)",         class = "cor")                        # random-effect correlations
)
for (p in crit) pr <- pr +
  set_prior("normal(-0.5, 1)", class = "Intercept", dpar = p) +     # log criterion spacing
  set_prior("normal(0, 0.5)",  class = "b",         dpar = p) +
  set_prior("normal(0, 1)",    class = "sd",        dpar = p)

dir.create("revision/models", showWarnings = FALSE, recursive = TRUE)

cat("\nLaunching sampler...\n")
t0 <- proc.time()
m <- fit_metad(
  form, data = d, K = K, prior = pr,
  init    = 0,
  chains  = CHAINS, cores = CORES,
  iter    = ITER, warmup = WARMUP,
  seed    = 2024,
  backend = "cmdstanr",
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  file    = "revision/models/fit_full_maximal",
  file_refit = "on_change"
)
cat("\n=== elapsed (min) ===\n"); print(round((proc.time() - t0)["elapsed"] / 60, 1))

# --- quick convergence + effect summary -------------------------------------
cat("\n=== sampler diagnostics ===\n")
np   <- brms::nuts_params(m)
cat("divergent transitions:", sum(np$Parameter == "divergent__" & np$Value == 1), "\n")
cat("max Rhat:", round(max(brms::rhat(m), na.rm = TRUE), 4), "\n")

cat("\n=== gender effects on log M-ratio (fixed) ===\n")
fx <- fixef(m)
print(round(fx[grep("gender", rownames(fx)), , drop = FALSE], 4))

cat("\nSaved model to revision/models/fit_full_maximal.rds\n")
