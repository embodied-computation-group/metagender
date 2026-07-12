# Revision analysis 00 -- toolchain smoke test (OPTIONAL) ------------------
# =========================================================================
# Run this FIRST on a new machine (e.g. the cluster) to confirm hmetad +
# brms + cmdstanr can compile and sample the meta-d' family before launching
# the full multi-hour fit. Fits ONE modality with a short chain. NOT for
# inference -- purely a pipeline / environment check.
#
# Run from anywhere:  Rscript revision/code/00_smoke_test.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

suppressMessages({ library(dplyr); library(brms); library(hmetad) })

stopifnot(file.exists("revision/data/hmetad_trial.rds"))
d  <- readRDS("revision/data/hmetad_trial.rds")
dm <- d |> filter(mod == "memory") |> droplevels()
cat("smoke test on memory subset:", nrow(dm), "trials,", nlevels(dm$subj), "subjects\n")

K    <- 7L
crit <- c(paste0("metac2zero", 1:6, "diff"), paste0("metac2one", 1:6, "diff"))

form <- bf(N ~ gender + (1 | subj))
for (p in c("dprime", "c", crit)) form <- form + lf(as.formula(paste(p, "~ gender + (1 | subj)")))

pr <- c(
  set_prior("normal(0, 0.5)", class = "Intercept"),
  set_prior("normal(1, 0.5)", class = "Intercept", dpar = "dprime"),
  set_prior("normal(0, 0.5)", class = "Intercept", dpar = "c"),
  set_prior("normal(0, 0.5)", class = "b"),
  set_prior("normal(0, 0.5)", class = "b", dpar = "dprime"),
  set_prior("normal(0, 0.5)", class = "b", dpar = "c"),
  set_prior("normal(0, 1)",   class = "sd"),
  set_prior("normal(0, 1)",   class = "sd", dpar = "dprime"),
  set_prior("normal(0, 1)",   class = "sd", dpar = "c")
)
for (p in crit) pr <- pr +
  set_prior("normal(-0.5, 1)", class = "Intercept", dpar = p) +
  set_prior("normal(0, 0.5)",  class = "b",         dpar = p) +
  set_prior("normal(0, 1)",    class = "sd",        dpar = p)

dir.create("revision/models", showWarnings = FALSE, recursive = TRUE)
t0 <- proc.time()
m <- fit_metad(
  form, data = dm, K = K, prior = pr,
  init = 0, chains = 2, cores = 2, iter = 400, warmup = 200, seed = 1,
  backend = "cmdstanr", control = list(adapt_delta = 0.9),
  file = "revision/models/smoke_test_memory"
)
cat("\nelapsed (s):", round((proc.time() - t0)["elapsed"], 1), "\n")
cat("SMOKE TEST PASSED -- hmetad/brms/cmdstanr compile and sample OK.\n")
cat("gender effect on log M (memory):\n")
print(round(fixef(m)["genderFeminin", ], 4))
