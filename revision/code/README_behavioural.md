# Behavioural analyses (revision) — scripts 20–29 and Figures 2, 3, S3, S5

Everything here is plain R, run with `Rscript` from anywhere in the repo; each script
resolves the repo root itself. Set the library path first:

    export R_LIBS_USER=/mnt/penguino/micah/Rlib_hmetad_dev:/mnt/penguino/micah/Rlib_meta_intero

Common convention: two-level factors sum-coded to ±0.5 (`fem_c` = +0.5 Feminin / −0.5 Masculin,
`time_c` = +0.5 post, `acc_c` = +0.5 correct); domain cell-means coded (`0 + domain`) so every
effect is estimated inside a domain; the domain-general effect is the draw-wise mean of the
per-domain effects and domain specificity is their pairwise differences. All models are gaussian
`brms` with `(1 | subj)`, `age_z + edu_z` covariates, a residual SD free to vary over the
within-subject design cells, priors scaled to the SD of the DV, and `sample_prior = "yes"` so
Savage–Dickey BF10 is available. ROPE = ±0.1 × SD(DV).

**Prior convention.** With `s = SD(DV)`, the prior width depends on what the coefficient is.
A *cell mean* (`domain<X>`) can sit anywhere the DV can, and the standardised covariates
(`age_z`, `edu_z`, `acc_z`) shift the DV by a per-SD amount, so both keep `normal(0, s)`.
A *contrast* — any coefficient carrying a sum-coded predictor (`fem_c`, `time_c`, `acc_c`,
or an interaction of them, up to `domain:fem_c:time_c`) — cannot plausibly be as large as the
whole spread of the DV, so it gets `normal(0, k × s)` with **k = 0.5**. The distinction matters
for the Bayes factors and almost not at all for the estimates: Savage–Dickey divides by the
prior density at zero, so an over-wide contrast prior mechanically drives BF10 towards 0 for a
small but real effect (Lindley), while the posterior mean and HDI move only through shrinkage.
`fit_light_model()` sets every coefficient's prior by name (`set_prior(..., coef = )`) and then
checks `prior_summary()` on the fitted model, so no coefficient can quietly fall back on a brms
default. `29_prior_sensitivity_behavioural.R` refits everything at k = 0.25 and k = 1 (via the
`CONTRAST_SCALE` environment variable, which also suffixes the model and effects files with
`_k<k>`) and tabulates the headline contrasts at all three widths.

| Script | Does | In | Out |
|---|---|---|---|
| `utils_bayes.R` | helper: `fit_light_model()`, `eff_row()` (mean, 95% HDI, P(>0), % in ROPE), `bf10()`, `print_effects()`, `check_convergence()` | — | — |
| `20_prep_cells.R` | builds the subject × domain cell table; reproduces the paper's N = 317 (172 F / 145 M); prints raw means by gender × domain | `data/metacognition_TrialData_master.csv`, `data/self_belief_{pre,post}_labels.csv` | `revision/data/cells.rds`, `subjects.rds`, `raw_cell_means.csv` |
| `21_global_calibration.R` | calibration (self-belief − accuracy) by gender × time × domain; per-domain and domain-general gender, time and gender × time effects, plus the cell means that say who is over- or under-confident | `cells.rds` | `effects_calibration.csv`, `models/m21_calibration.rds` |
| `22_local_confidence.R` | mean confidence by gender × trial accuracy × domain, controlling each participant's task accuracy; error sensitivity = gender × accuracy | `cells.rds` | `effects_confidence.csv`, `models/m22_confidence.rds` |
| `23_reaction_time.R` | type-1 RT by gender × trial accuracy × domain; tests whether men slow down less on errors (Reviewer 3's impulsivity account) | `cells.rds` | `effects_rt.csv`, `models/m23_rt.rds` |
| `24_staircase.R` | staircase difficulty variability (SD of \|stimulus level\|, z-scored within domain) by gender; vision, GDP, Calories only | `cells.rds` | `effects_staircase.csv`, `models/m24_staircase.rds` |
| `25_local_global_link.R` | local ↔ global link: subject-level log M-ratio and meta-Δ from the unified fit regressed on global calibration (pre) and the pre→post update, moderated by gender; two models, one per outcome | `cells.rds`, `subject_logM.csv`, `subject_metadelta.csv` (from `06_subject_estimates.R`) | `effects_local_global.csv`, `models/m25_{logM,metadelta}.rds` |
| `26_positive_controls.R` | Bayesian replacement for the JASP t-tests: gender contrasts on accuracy and on mean RT per domain, with BF10 and ROPE | `cells.rds` | `effects_positive_controls.csv`, `models/m26_{accuracy,rt}.rds` |
| `fig2_calibration.R` | Figure 2: A) calibration pre vs post, grey individual lines, no colour map; B) per-domain calibration by gender from model 21 | `cells.rds`, `effects_calibration.csv` | `revision/figures/Figure2.{png,pdf}` |
| `fig3_local_confidence.R` | Figure 3: A) confidence correct vs incorrect, grey individual lines, no colour map; B) per-domain confidence by gender × accuracy from model 22 | `cells.rds`, `effects_confidence.csv` | `revision/figures/Figure3.{png,pdf}`, `Figure3_caption.txt` |
| `20b_prep_cells_vmp.R` | the VMP mirror of `20`: subject × domain cells for 457 participants (289 F / 168 M) on the same GDP and Calories tasks. No self-belief, no education, no staircase — so no VMP counterpart of models 21 or 24 | `revision/data/vmp/{trivia_trials,demographics}.tsv` | `revision/data/cells_vmp.rds`, `raw_cell_means_vmp.csv` |
| `22b_local_confidence_vmp.R` | model 22 on the VMP cells: two domains, age as the only covariate. Everything else identical | `cells_vmp.rds` | `effects_confidence_vmp.csv`, `models/m22b_confidence_vmp.rds` |
| `figS3_local_confidence_vmp.R` | Figure S3: the two-domain mirror of Figure 3 in the VMP sample | `cells_vmp.rds`, `effects_confidence_vmp.csv` | `revision/figures/FigureS3_vmp_confidence.{png,pdf}` + caption |
| `figS5_rt_staircase.R` | Figure S5: A) raw RT on correct vs error trials by gender × domain; B) model 23's error-slowing gender interaction; C) raw staircase SD by gender × domain; D) model 24's gender contrast | `cells.rds`, `effects_rt.csv`, `effects_staircase.csv` | `revision/figures/FigureS5_rt_staircase.{png,pdf}` + caption |
| `29_prior_sensitivity_behavioural.R` | refits 21–26 at k = 0.25 and k = 1 and tabulates the headline gender contrasts (mean, 95% HDI, BF10, % ROPE) against the reported k = 0.5 | all `effects_*.csv` | `revision/data/prior_sensitivity_behavioural.csv` |

Order: `20` → `21`–`24`, `26` (any order) → `fig2_*`, `fig3_*`; `20b` → `22b` → `figS3_*`;
`figS5_*` after `23` and `24`; `29` last, since it refits everything twice more. `25` needs the unified fit's subject-level estimates and so
runs after `revision/code/06_subject_estimates.R` (see `../README.md`). Models are cached in
`revision/models/` (gitignored) and reloaded unless the formula or data change.

Every figure script writes its caption, including the model formula, to
`revision/figures/<name>_caption.txt`. Formulas are deliberately kept out of panel
subtitles (2026-09-05 figure review).

### What the VMP replication says about the behavioural claims

| Claim | First edition (model 22) | VMP (model 22b) |
|---|---|---|
| women report lower confidence, domain-general | −0.27 [−0.41, −0.14], BF10 = 1.6e3 | −0.31 [−0.44, −0.18], BF10 = 9.6e6 |
| the effect is carried by GDP | −0.62 [−0.79, −0.45], BF10 = 2.8e18 | −0.48 [−0.63, −0.33], BF10 = 1.5e16 |
| the effect is absent in Calories | −0.04 [−0.18, 0.12], BF10 = 0.14 | −0.14 [−0.27, −0.01], BF10 = 1.04 |
| women are more sensitive to errors (gender × accuracy) | +0.12 [0.02, 0.22], BF10 = 1.94 | +0.04 [−0.08, 0.16], BF10 = 0.18 |

The confidence effect and its domain specificity replicate; the error-sensitivity claim does
not — the replication puts 94% of that posterior inside the ROPE.
