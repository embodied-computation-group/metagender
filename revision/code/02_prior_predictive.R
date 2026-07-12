# Revision analysis 02 -- prior predictive check --------------------------
# =========================================================================
# Draws parameters from the population-level priors and simulates data with
# sim_metad(), to confirm the priors imply realistic type-1 accuracy,
# M-ratio, and confidence distributions BEFORE launching the full fit.
# (Population/intercept-level check; random-effect SDs add spread on top.)
#
# Run from anywhere:  Rscript revision/code/02_prior_predictive.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

suppressMessages({ library(hmetad); library(dplyr) })
set.seed(1)
n <- 4000

# draw population intercepts from the priors (latent scale)
logM   <- rnorm(n, 0, 0.5)                 # log M-ratio
dprime <- rnorm(n, 1, 0.5)                 # d'
cc     <- rnorm(n, 0, 0.5)                 # type-1 criterion c
z0 <- matrix(rnorm(n * 6, -0.5, 1), n, 6)  # 6 criterion log-spacings, "0" responses
z1 <- matrix(rnorm(n * 6, -0.5, 1), n, 6)  # 6 criterion log-spacings, "1" responses

Mratio <- exp(logM)
acc <- 0.5 * (pnorm(dprime/2 - cc) + pnorm(dprime/2 + cc))   # implied 2AFC accuracy

qtab <- function(x) round(quantile(x, c(.025, .25, .5, .75, .975)), 3)
cat("=== implied M-ratio ===\n");         print(qtab(Mratio))
cat("\n=== implied d' ===\n");            print(qtab(dprime))
cat("\n=== implied type-1 accuracy ===\n"); print(qtab(acc))
cat("   share accuracy in 0.55-0.95:", round(mean(acc > .55 & acc < .95), 3), "\n")
cat("   share M-ratio  in 0.3-2.0 :", round(mean(Mratio > .3 & Mratio < 2), 3), "\n")

cat("\n=== implied confidence distribution (5 prior draws) ===\n")
for (i in sample(n, 5)) {
  sd <- sim_metad(N_trials = 4000, dprime = dprime[i], c = cc[i], log_M = logM[i],
                  c2_0_diff = exp(z0[i, ]), c2_1_diff = exp(z1[i, ]))
  ct <- table(factor(sd$confidence, levels = 1:7))
  cat(sprintf("  d'=%.2f M=%.2f acc=%.2f | conf 1-7: %s\n",
              dprime[i], Mratio[i], mean(sd$response == sd$stimulus),
              paste(sprintf("%.2f", ct / sum(ct)), collapse = " ")))
}
cat("\nPrior predictive check done.\n")
