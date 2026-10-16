# Revision analysis 07 -- posterior draws of the two Figure 4 contrasts
# ===========================================================================
#   Rscript revision/code/07_contrast_draws.R --fit revision/models/fit_vmp_fixed.rds \
#                                             --tag vmp_fixed
#
# Figure 4 and Figure S4 draw densities, so they need DRAWS, not the summaries
# 05_effects_unified.R writes. Extracting them means loading a ~0.5 GB fit, which
# a figure script has no business doing four times over. This script does it once
# per fit and leaves two small files behind:
#
#   revision/data/draws_<tag>.csv    quantity x domain x draw  (the densities)
#   revision/data/labels_<tag>.csv   quantity x domain: mean, 95% HDI, BF10,
#                                    % in ROPE   (the text label on each density)
#
# THE TWO QUANTITIES, both Feminin - Masculin, both per domain and domain-general
# (the draw-wise mean over domains):
#   logM       the gender contrast on log M-ratio, i.e. b_mod<X>:fem_c
#   metadelta  the gender contrast on meta-Delta, averaged over the two response
#              sides. POSITIVE = women's criteria sit further from meta-c = women
#              use lower confidence = women less metacognitively biased.
#
# WHERE THE NUMBERS COME FROM
#   log M: mean, HDI, BF10 and % in ROPE are read from effects_<tag>_gender.csv so
#   that the figure cannot disagree with the tables. Run 05 first.
#
#   meta-Delta: 05 reports no Bayes factor for it, because meta-Delta is a
#   nonlinear function of twelve criterion parameters and brms::hypothesis() only
#   handles linear combinations of coefficients. It is, however, a CLOSED FORM:
#   hmetad defines meta-Delta = sum_j w_j * exp(eta_j), w_j = (7-j)/6, over the six
#   criterion log-spacings of a response side. So the same map applied to the
#   fit's PRIOR draws (sample_prior = "yes") gives the prior of the contrast, and
#   the Savage-Dickey ratio at zero gives BF10. The closed form is checked against
#   hmetad's own add_metacognitive_bias_draws() below and the script stops if they
#   disagree -- it is a shortcut into the prior, not a reimplementation.
#   No ROPE is reported for meta-Delta: the +/-0.05 band is defined on log units of
#   M-ratio and has no meaning on the criterion scale.
# ---------------------------------------------------------------------------

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)
source("revision/code/helpers.R")

suppressMessages({
  library(hmetad); library(brms); library(posterior); library(bayestestR)
  library(dplyr); library(tidyr); library(tidybayes)
})

FIT <- getarg("--fit", "revision/models/fit_firstedition_fixed.rds")
TAG <- getarg("--tag", sub("^fit_|\\.rds$", "", basename(FIT)))

m    <- readRDS(FIT)
fam  <- fitted_confidence_model(m)
mods <- levels(m$data$mod)
dr   <- as_draws_df(m)
cat(sprintf("fit: %s\ntag: %s\nmodel: K = %d, confidence model = %s\ndomains: %s\n",
            FIT, TAG, fam$K, toupper(fam$confidence_model), paste(mods, collapse = ", ")))

# ======================================================= log M contrasts ====
logM <- sapply(mods, function(x) dr[[paste0("b_mod", x, ":fem_c")]])
logM <- cbind(logM, `domain-general` = rowMeans(logM))

# =================================================== meta-Delta contrasts ====
# meta-Delta of one response side = sum_j ((7-j)/6) * exp(eta_j), eta on the log link.
W <- (6:1) / 6
md_side <- function(pre, side, x, fem) {
  eta <- sapply(1:6, function(j)
    dr[[sprintf("%smetac2%s%ddiff_mod%s", pre, side, j, x)]] +
      fem * dr[[sprintf("%smetac2%s%ddiff_mod%s:fem_c", pre, side, j, x)]])
  as.vector(exp(eta) %*% W)
}
# averaged over the two response sides, then Feminin - Masculin
md_contrast <- function(pre, x)
  (md_side(pre, "zero", x, 0.5)  + md_side(pre, "one", x, 0.5)) / 2 -
  (md_side(pre, "zero", x, -0.5) + md_side(pre, "one", x, -0.5)) / 2

md <- sapply(mods, function(x) md_contrast("b_", x))
md <- cbind(md, `domain-general` = rowMeans(md))

# -- check the closed form against hmetad itself ----------------------------
nd  <- expand_grid(mod = factor(mods, levels = mods), fem_c = c(-0.5, 0.5))
ref <- add_metacognitive_bias_draws(nd, m, re_formula = NA) |> ungroup() |>
  group_by(mod, fem_c, .draw) |>
  summarise(mb = mean(metacognitive_bias), .groups = "drop") |>
  pivot_wider(names_from = fem_c, values_from = mb) |>
  mutate(diff = `0.5` - `-0.5`) |>
  group_by(mod) |> summarise(m = mean(diff), .groups = "drop")
chk <- max(abs(ref$m[match(mods, as.character(ref$mod))] - colMeans(md)[mods]))
cat(sprintf("closed-form meta-Delta vs hmetad: max |difference of means| = %.3g\n", chk))
stopifnot(chk < 1e-6)

# --------------------------------------------------------- draws on disk ----
long <- function(mat, quantity)
  as.data.frame(mat) |>
    mutate(.draw = row_number()) |>
    pivot_longer(-.draw, names_to = "domain", values_to = "value") |>
    mutate(tag = TAG, quantity = quantity)

draws <- bind_rows(long(logM, "logM"), long(md, "metadelta")) |>
  select(tag, quantity, domain, .draw, value)
write.csv(draws, sprintf("revision/data/draws_%s.csv", TAG), row.names = FALSE)

# ================================================================ labels ====
# log M: taken from 05's table, so the figure and the tables cannot drift apart.
gen_csv <- sprintf("revision/data/effects_%s_gender.csv", TAG)
if (!file.exists(gen_csv))
  stop("run 05_effects_unified.R --tag ", TAG, " first: ", gen_csv, " is missing")
lab_logM <- read.csv(gen_csv, stringsAsFactors = FALSE) |>
  filter(dpar == "N", effect %in% c(mods, "domain-general")) |>
  transmute(quantity = "logM", domain = effect, mean, hdi_lo, hdi_hi,
            bf10, pct_in_rope)

# meta-Delta: Savage-Dickey at zero, prior obtained through the same closed form.
md_prior <- sapply(mods, function(x) md_contrast("prior_b_", x))
md_prior <- cbind(md_prior, `domain-general` = rowMeans(md_prior))
sd_bf10 <- function(post, prior) {
  b <- try(bayestestR::bayesfactor_parameters(
    data.frame(x = post), prior = data.frame(x = prior), null = 0, verbose = FALSE),
    silent = TRUE)
  if (inherits(b, "try-error")) NA_real_ else exp(b$log_BF[1])
}
cols <- colnames(md)
lab_md <- bind_rows(lapply(cols, function(k) {
  h <- ggdist::mean_hdci(md[, k], .width = .95)
  data.frame(quantity = "metadelta", domain = k, mean = mean(md[, k]),
             hdi_lo = h$ymin, hdi_hi = h$ymax,
             bf10 = sd_bf10(md[, k], md_prior[, k]), pct_in_rope = NA_real_)
}))

labels <- bind_rows(lab_logM, lab_md) |> mutate(tag = TAG) |> relocate(tag)
write.csv(labels, sprintf("revision/data/labels_%s.csv", TAG), row.names = FALSE)

cat("\n=== Figure 4 inputs ===\n")
print(as.data.frame(labels), row.names = FALSE, digits = 3)
cat(sprintf("\nwrote revision/data/{draws_%s.csv, labels_%s.csv}\n", TAG, TAG))
