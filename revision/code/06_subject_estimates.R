# Revision analysis 06 -- subject-level log M-ratio and meta-Delta
# ===========================================================================
#   Rscript revision/code/06_subject_estimates.R \
#     --fit revision/models/fit_firstedition_fixed.rds [--ndraws 1000]
#
# The unified fit's per-subject posteriors, written out so that the local <->
# global link model (25_local_global_link.R) can regress them on the global
# calibration measure. Nothing is modelled here.
#
#   revision/data/subject_logM.csv       subj x domain: mean, sd, 95% quantile
#   revision/data/subject_metadelta.csv  subj x domain: ditto, averaged over
#                                        the two response sides, plus each side
#
# WHAT IS COMPUTED
#   log M for subject s in domain X = b_mod<X>  +  fem_c(s) * b_mod<X>:fem_c
#                                     + r_subj[s, mod<X>]
#   i.e. the domain cell mean, the subject's gender offset, and the subject's
#   own random effect -- the full posterior of that subject's log M-ratio, not a
#   deviation score.
#
#   meta-Delta comes from add_metacognitive_bias_draws() with re_formula = NULL,
#   so every dpar's random effect is included; it is a nonlinear function of d',
#   c, meta-d' and the twelve criteria, which is why it cannot be assembled by
#   hand the way log M can. It is returned per response side; we write both sides
#   and their average (the quantity the link model uses).
#
# SHRINKAGE. These are hierarchical estimates: they are pulled towards the group
# and towards each gender's own cell mean. That attenuates any regression slope
# fitted on them and can in principle manufacture a gender moderation out of the
# differential pull. Script 25 states this; so must the manuscript.
# ---------------------------------------------------------------------------

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)
source("revision/code/helpers.R")

suppressMessages({
  library(hmetad); library(brms); library(posterior); library(dplyr)
  library(tidyr); library(tidybayes)
})

FIT    <- getarg("--fit", "revision/models/fit_firstedition_fixed.rds")
NDRAWS <- as.integer(getarg("--ndraws", "1000"))

m   <- readRDS(FIT)
fam <- fitted_confidence_model(m)
cat(sprintf("fit  : %s\nmodel: K = %d, confidence model = %s\n",
            FIT, fam$K, toupper(fam$confidence_model)))

mods <- levels(m$data$mod)
nd   <- m$data |> distinct(subj, mod, fem_c) |> arrange(subj, mod)
nd$gender <- ifelse(nd$fem_c > 0, "Feminin", "Masculin")
cat(sprintf("subjects: %d, domains: %s, cells: %d\n",
            nlevels(droplevels(factor(nd$subj))), paste(mods, collapse = ", "), nrow(nd)))

# ============================================================ log M-ratio ====
# The mu block of the hmetad family is log M and carries no dpar prefix, so its
# coefficients are `b_mod<X>` and its random effects `r_subj[<subj>,mod<X>]`.
dr <- as_draws_df(m)

logM <- bind_rows(lapply(seq_len(nrow(nd)), function(i) {
  x <- nd[i, ]
  b  <- dr[[paste0("b_mod", x$mod)]] + x$fem_c * dr[[paste0("b_mod", x$mod, ":fem_c")]]
  re <- dr[[sprintf("r_subj[%s,mod%s]", x$subj, x$mod)]]
  v  <- b + re
  data.frame(subj = as.character(x$subj), gender = x$gender, fem_c = x$fem_c,
             domain = as.character(x$mod),
             logM_mean = mean(v), logM_sd = sd(v),
             logM_lo = unname(quantile(v, .025)), logM_hi = unname(quantile(v, .975)),
             stringsAsFactors = FALSE)
}))
write.csv(logM, "revision/data/subject_logM.csv", row.names = FALSE)

cat("\n=== subject-level log M: spread by gender x domain ===\n")
print(as.data.frame(logM |> group_by(gender, domain) |>
        summarise(n = n(), mean = mean(logM_mean), sd = sd(logM_mean),
                  post_sd = mean(logM_sd), .groups = "drop")),
      row.names = FALSE, digits = 3)

# ============================================================== meta-Delta ====
# re_formula = NULL keeps every random effect, i.e. this is subject-specific.
cat(sprintf("\ndrawing meta-Delta for %d cells x %d draws ...\n", nrow(nd), NDRAWS))
md <- add_metacognitive_bias_draws(
        nd |> select(subj, mod, fem_c), m,
        re_formula = NULL, ndraws = NDRAWS, seed = 2026) |> ungroup()

per_side <- md |>
  group_by(subj, mod, response) |>
  summarise(mean = mean(metacognitive_bias), sd = sd(metacognitive_bias),
            .groups = "drop") |>
  mutate(side = paste0("resp", response)) |>
  select(-response) |>
  pivot_wider(names_from = side, values_from = c(mean, sd),
              names_glue = "md_{side}_{.value}")

both <- md |>
  group_by(subj, mod, .draw) |>
  summarise(mb = mean(metacognitive_bias), .groups = "drop") |>   # average over sides
  group_by(subj, mod) |>
  summarise(md_mean = mean(mb), md_sd = sd(mb),
            md_lo = quantile(mb, .025), md_hi = quantile(mb, .975), .groups = "drop")

metad <- nd |>
  transmute(subj = as.character(subj), gender, fem_c, domain = as.character(mod),
            mod = mod) |>
  left_join(both,     by = c("subj", "mod")) |>
  left_join(per_side, by = c("subj", "mod")) |>
  select(-mod)
stopifnot(!anyNA(metad$md_mean))
write.csv(metad, "revision/data/subject_metadelta.csv", row.names = FALSE)

cat("\n=== subject-level meta-Delta (averaged over response sides) ===\n")
print(as.data.frame(metad |> group_by(gender, domain) |>
        summarise(n = n(), mean = mean(md_mean), sd = sd(md_mean),
                  post_sd = mean(md_sd), .groups = "drop")),
      row.names = FALSE, digits = 3)

cat("\n=== correlation of the two subject-level estimates (within domain) ===\n")
print(as.data.frame(logM |> left_join(metad, by = c("subj", "gender", "fem_c", "domain")) |>
        group_by(domain) |>
        summarise(r = cor(logM_mean, md_mean), .groups = "drop")),
      row.names = FALSE, digits = 3)

cat("\nwrote revision/data/{subject_logM.csv, subject_metadelta.csv}\n")
