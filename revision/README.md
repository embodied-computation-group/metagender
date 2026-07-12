# Revision analysis — Bayesian meta-d′ refit (`hmetad` / brms / Stan)

This directory contains the **revision** re-analysis of the metagender study. It is
**self-contained and isolated** from the first-edition code (`../code`, `../metad`,
`../data`, `../figures`) — nothing here overwrites or depends on published analysis files,
other than *reading* the raw first-edition data `../data/Hmetad_data.csv` (unchanged).

## Why

The first edition modelled metacognitive efficiency with the now-deprecated **MATLAB HMeta-d**
(JAGS) toolbox: it fit **separate hierarchical models for females and males within each domain**
(`fitF*.mat`, `fitM*.mat`) and then subtracted the group-level `mu_logMratio` posteriors to get a
gender difference (Figure 4). Reviewers objected to this design.

The revision replaces it with a **single hierarchical model** fit to all data at once, with
**gender as a predictor**. The gender coefficient on the log M-ratio *is* the contrast — with
proper partial pooling, no subtraction of independently-fit models.

## Model

Custom brms multinomial meta-d′ family (`hmetad::metad`, K = 7). Fifteen distributional
parameters: `mu` = log M-ratio (= log meta-d′/d′), `dprime`, `c`, and twelve confidence-criterion
log-spacings (`metac2zero1-6diff`, `metac2one1-6diff`).

**Structure (maximal):**
```
<each parameter> ~ gender * mod + (1 + mod | subj)
```
- Fixed `gender * mod`: `genderFeminin` on `mu` = **domain-general** gender difference in
  efficiency; `genderFeminin:mod` = **domain-specific** differences (the Figure-4 contrasts).
- Reference levels: gender = **Masculin**, mod = **memory** → `genderFeminin` reads
  Feminin − Masculin (same direction as the old Female − Male contrast).
- Correlated `(1 + mod | subj)` random slopes on every parameter.

Priors are weakly-informative on the latent scale — see `reference/hmetad_reference.md` §2 and
the prior table in `code/03_fit_model.R`. Validated (0 flat priors) and passed a prior predictive
check.

> **Compute note.** The maximal structure has fifteen correlated 4×4 random-effect matrices and is
> heavy — warmup ran > 25 s/iteration on an 8-core laptop (projected 12–24 h+). It is intended for
> a cluster. If it under-converges (watch R̂/ESS on the `cor`/criterion parameters), drop the random
> slopes from the twelve confidence-criterion parameters to `(1 | subj)` — the "Balanced" fallback
> — which gives identical inference on the gender × log-M question far more cheaply.

## Files

```
revision/
  code/
    00_smoke_test.R          # OPTIONAL: verify hmetad/brms/cmdstanr compile+sample on this machine
    01_prep_data.R           # ../data/Hmetad_data.csv -> data/hmetad_trial.rds
    02_prior_predictive.R    # prior predictive check (no model needed)
    03_fit_model.R           # fit the model -> models/fit_full_maximal.rds   [the long job]
    04_posterior_predictive.R# PPCs -> figures/ppc/*.png, data/ppc_summary.csv
  reference/
    hmetad_reference.md       # generated reference on hmetad data format / parameterization / brms
    hmetad_tutorial_utils.R   # hmetad workshop plotting helpers
  data/                       # derived data (hmetad_trial.rds, ppc_summary.csv) — committed
  models/                     # fitted model .rds — gitignored (large; regenerated)
  figures/                    # generated figures — .png gitignored
  logs/                       # run logs — gitignored
```

Every script resolves the repo root itself, so it runs from any working directory.

## Running on the cluster

Dependencies (R ≥ 4.4): `hmetad`, `brms`, `cmdstanr` (+ a working CmdStan install via
`cmdstanr::install_cmdstan()`), `tidybayes`, `dplyr`, `tidyr`, `purrr`, `ggplot2`.

```bash
cd <repo root>

# 0. (optional) confirm the toolchain works before the long run
Rscript revision/code/00_smoke_test.R

# 1. prepare data (fast)
Rscript revision/code/01_prep_data.R

# 2. prior predictive check (fast)
Rscript revision/code/02_prior_predictive.R

# 3. fit the model (the long job — run under nohup / a job scheduler)
#    resources are overridable via env vars:
HMETAD_CHAINS=4 HMETAD_CORES=4 HMETAD_ITER=2000 HMETAD_WARMUP=1000 \
  nohup Rscript revision/code/03_fit_model.R > revision/logs/fit.log 2>&1 &

# 4. posterior predictive checks (after the fit lands)
Rscript revision/code/04_posterior_predictive.R          # 300 draws (default)
```

`03_fit_model.R` uses `file_refit = "on_change"`, so re-running it reuses
`models/fit_full_maximal.rds` if it already exists and the model is unchanged.
