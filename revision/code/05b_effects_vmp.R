# Revision analysis 05b -- read the estimands off the VMP replication fit
# ===========================================================================
#   Rscript revision/code/05b_effects_vmp.R                    # revision/models/fit_vmp.rds
#   Rscript revision/code/05b_effects_vmp.R --fit <path>
#
# Writes:
#   revision/data/vmp_gender_effects.csv    gender contrast on every dpar
#   revision/data/vmp_meta_delta.csv        meta-Delta levels + gender contrast
#   revision/data/vmp_rho.csv               between-domain correlation of log M
#
# ---------------------------------------------------------------------------
# SIGN CONVENTION: every contrast below is Feminin - Masculin, matching the published
# Figure 4 direction. Positive = women higher.
#
# WHAT `b_fem_c` IS, AND WHY IT IS NOT WHAT 03_fit_model.R's `genderFeminin` IS.
# Because 03b_fit_vmp.R sum-codes both factors, the linear predictor is
#     Int + g*fem_c + m*gdp_c + gm*fem_c*gdp_c,     fem_c, gdp_c in {-0.5, +0.5}
# so  gender effect at GDP      = g + 0.5*gm
#     gender effect at Calories = g - 0.5*gm
#     average over modalities   = g                  <- THE DOMAIN-GENERAL EFFECT
# Under the first-edition model's TREATMENT coding, its `genderFeminin` is instead the
# gender effect at the reference modality (memory) alone, despite the README calling it
# domain-general. The two are different quantities. Do not compare them directly without
# first recomputing the first edition's domain-general effect as the average of its four
# modality-specific simple effects (which can be done from its existing draws, no refit).
#
# meta-Delta (O'Neill, Zhan & Fleming, 2026) is the principled measure of metacognitive
# BIAS: the distance between the average confidence criterion and meta-c, defined net of
# d', c and meta-d'. Mean confidence -- the measure the first edition leans on -- is
# confounded by all three (Sherman et al., 2018). It runs BACKWARDS from confidence:
# SMALLER meta-Delta = criteria packed nearer meta-c = uses higher confidence = more biased.
# ---------------------------------------------------------------------------

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

suppressMessages({
  library(hmetad); library(brms); library(posterior); library(ggdist)
  library(dplyr); library(tidyr); library(tidybayes)
})

args   <- commandArgs(trailingOnly = TRUE)
getarg <- function(k, d) { i <- which(args == k); if (length(i)) args[i + 1] else d }
FIT <- getarg("--fit", "revision/models/fit_vmp.rds")
K   <- 7L

m  <- readRDS(FIT)
dr <- as_draws_df(m)
message("fit: ", FIT)

metac2 <- metac2_parameters(K = K)
DPARS  <- c(N = "log M-ratio", dprime = "d'", c = "c",
            setNames(metac2, metac2))

# A mistyped coefficient name returns NULL and silently poisons everything downstream.
co <- function(dpar, term) {
  pre <- if (dpar == "N") "" else paste0(dpar, "_")
  nm  <- paste0("b_", pre, term)
  if (!nm %in% names(dr))
    stop("no such coefficient: ", nm, "\navailable: ",
         paste(grep("^b_", names(dr), value = TRUE)[1:12], collapse = ", "), " ...")
  dr[[nm]]
}

summ <- function(x, dpar, label) {
  h <- ggdist::mean_hdi(x, .width = .95)
  tibble(param = unname(DPARS[dpar]), dpar = dpar, effect = label,
         mean = mean(x), hdi_lo = h$ymin, hdi_hi = h$ymax,
         p_gt0 = mean(x > 0), excludes0 = (h$ymin > 0) | (h$ymax < 0))
}

# --------------------------------------------------- gender, every dpar ----
eff <- bind_rows(lapply(names(DPARS), function(p) {
  g  <- co(p, "fem_c")            # domain-general (the average over modalities)
  gm <- co(p, "fem_c:gdp_c")      # domain-specificity
  bind_rows(
    summ(g,            p, "domain-general (mean of domains)"),
    summ(g + 0.5 * gm, p, "GDP"),
    summ(g - 0.5 * gm, p, "Calories"),
    summ(gm,           p, "gender x domain interaction")
  )
}))
# exp(b) is the multiplicative effect on M-ratio; only meaningful on the log-M rows.
eff$M_ratio_x <- ifelse(eff$dpar == "N", exp(eff$mean), NA_real_)

cat("\n=== gender contrast (Feminin - Masculin), log M-ratio ===\n")
print(as.data.frame(eff |> filter(dpar == "N") |>
        select(effect, mean, hdi_lo, hdi_hi, p_gt0, excludes0, M_ratio_x)),
      row.names = FALSE, digits = 3)

cat("\n=== gender contrast on type-1 (d', c) ===\n")
print(as.data.frame(eff |> filter(dpar %in% c("dprime", "c"), effect != "gender x domain interaction") |>
        select(param, effect, mean, hdi_lo, hdi_hi, excludes0)),
      row.names = FALSE, digits = 3)

cat("\n=== criteria whose gender contrast excludes zero ===\n")
crit <- eff |> filter(dpar %in% metac2, excludes0, effect != "gender x domain interaction")
if (nrow(crit)) print(as.data.frame(crit |> select(param, effect, mean, hdi_lo, hdi_hi)),
                      row.names = FALSE, digits = 3) else cat("  none\n")

write.csv(eff, "revision/data/vmp_gender_effects.csv", row.names = FALSE)

# ------------------------------------ between-domain correlation of log M ----
# Cell-means random effects make this a model parameter rather than a reconstruction.
rho_nm <- grep("^cor_subj__mod.*__mod", names(dr), value = TRUE)
rho_nm <- rho_nm[!grepl("dprime|_c_|metac2", rho_nm)][1]
if (!is.na(rho_nm)) {
  rho <- dr[[rho_nm]]; h <- ggdist::mean_hdi(rho, .width = .95)
  cat(sprintf("\n=== rho(log M: GDP, Calories) ===\n  %.3f  95%% HDI [%.3f, %.3f]  P(rho>0)=%.3f\n",
              mean(rho), h$ymin, h$ymax, mean(rho > 0)))
  write.csv(tibble(param = rho_nm, mean = mean(rho), hdi_lo = h$ymin, hdi_hi = h$ymax,
                   p_gt0 = mean(rho > 0)),
            "revision/data/vmp_rho.csv", row.names = FALSE)
}

# ------------------------------------------------------------ meta-Delta ----
nd <- expand_grid(fem_c = c(-0.5, 0.5), gdp_c = c(-0.5, 0.5))
md <- add_metacognitive_bias_draws(nd, m, re_formula = NA) |>
  ungroup() |>                       # RETURNS A GROUPED TIBBLE: select() would silently
  # re-add .row/fem_c/gdp_c as grouping columns, splitting the two genders onto different
  # rows so that `Feminin - Masculin` comes out NA for every cell -- with no error at all.
  mutate(gender = ifelse(fem_c > 0, "Feminin", "Masculin"),
         mod    = ifelse(gdp_c > 0, "GDP", "Calories"))

lvl <- md |> group_by(mod, gender, response) |>
  median_qi(metacognitive_bias, .width = .95) |> ungroup() |>
  select(mod, gender, response, meta_delta = metacognitive_bias, .lower, .upper)
cat("\n=== meta-Delta levels (median, 95% QI) ===\n")
print(as.data.frame(lvl), row.names = FALSE, digits = 3)

# Contrast computed DRAW-WISE, never by differencing the two summaries above.
ctr <- md |>
  select(mod, response, gender, .draw, metacognitive_bias) |>
  pivot_wider(names_from = gender, values_from = metacognitive_bias) |>
  mutate(diff = Feminin - Masculin)
stopifnot(!anyNA(ctr$diff))

ctr_s <- bind_rows(
  ctr |> group_by(mod, response) |> summarise(.groups = "drop",
           mean = mean(diff), hdi_lo = ggdist::mean_hdi(diff, .width = .95)$ymin,
           hdi_hi = ggdist::mean_hdi(diff, .width = .95)$ymax, p_gt0 = mean(diff > 0)) |>
    mutate(response = as.character(response)),
  ctr |> group_by(mod, .draw) |> summarise(diff = mean(diff), .groups = "drop") |>
    group_by(mod) |> summarise(.groups = "drop",
           mean = mean(diff), hdi_lo = ggdist::mean_hdi(diff, .width = .95)$ymin,
           hdi_hi = ggdist::mean_hdi(diff, .width = .95)$ymax, p_gt0 = mean(diff > 0)) |>
    mutate(response = "both")
) |> mutate(excludes0 = (hdi_lo > 0) | (hdi_hi < 0))

cat("\n=== meta-Delta gender contrast (Feminin - Masculin) ===\n")
cat("    POSITIVE = women's criteria sit FURTHER from meta-c = women use LOWER confidence\n")
cat("               = women less metacognitively biased / more conservative\n")
print(as.data.frame(ctr_s), row.names = FALSE, digits = 3)

# response is integer in `lvl` and character ("both") in `ctr_s`; bind_rows will not combine
# them silently, so make the type explicit rather than letting the write fail at the last line.
write.csv(bind_rows(
  lvl   |> mutate(response = as.character(response), quantity = "level"),
  ctr_s |> mutate(response = as.character(response), quantity = "contrast_Feminin_minus_Masculin")
), "revision/data/vmp_meta_delta.csv", row.names = FALSE)

cat("\nwrote revision/data/vmp_{gender_effects,meta_delta,rho}.csv\n")
