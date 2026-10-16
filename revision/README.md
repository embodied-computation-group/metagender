# Revision analysis — unified hierarchical meta-d′ model

Re-analysis for the Communications Psychology major revision. Self-contained: nothing here
overwrites first-edition code or data, it only *reads* `../data/Hmetad_data.csv`.

## What changed

The first edition fit **separate MATLAB HMeta-d models per gender per domain** and subtracted
the group-level `mu_logMratio` posteriors. Reviewers objected. The revision fits **one
hierarchical model** (`hmetad` custom brms family, Stan) to all the data with gender as a
predictor on every parameter, and reads the contrast straight off the posterior.

The same model, same priors and same QC are fitted to two samples:

| | subjects | domains | K |
|---|---|---|---|
| first edition | 318 (146 M / 172 F) | memory, vision, GDP, Calories | 7 |
| VMP replication | 456 (168 M / 288 F) | GDP, Calories | 7 |

## The model

```
<every dpar> ~ 0 + mod + mod:fem_c + (0 + mod | subj)
```

on all fifteen distributional parameters: `N` = log M-ratio, `dprime`, `c`, and the twelve
type-2 criterion log-spacings (`metac2_parameters(K = 7)`).

- `fem_c` = **+0.5 Feminin / −0.5 Masculin**; every contrast reads **Feminin − Masculin**.
- `b_mod<X>` is domain X's cell mean; `b_mod<X>:fem_c` is the gender effect **within** domain X.
- The **domain-general** effect is the draw-wise **mean** of the per-domain contrasts — it is
  not a coefficient. (The old `gender * mod` treatment coding made `genderFeminin` the gender
  effect in the *memory* task alone while the README called it domain-general. That is fixed.)
- Random effects are cell-means and independent per dpar, so `cor_subj__mod<X>__mod<Y>` is the
  between-domain correlation of log M — a parameter with a posterior, not a correlation of
  shrunk point estimates.
- Gender enters **all fifteen** dpars. If the type-2 criteria could not express a gender
  difference in confidence-scale usage, that difference would have nowhere to go but log M and
  would manufacture a spurious efficiency effect.
- Priors, all explicit (`03_fit_unified.R` refuses to run if any coefficient is left on a brms
  default): cell means `normal(0, 0.5)` on log M and c, `normal(1, 0.5)` on d′,
  `normal(-0.5, 1)` on the criteria; **every** `mod<X>:fem_c` contrast `normal(0, 0.5)`;
  `sd normal(0, 1)` on every dpar; `cor lkj(2)`. `sample_prior = "yes"` for Savage–Dickey BFs.
- **Prior sensitivity**: `--contrast-sd` refits with only the SD of the `mod<X>:fem_c` prior
  changed; both fixed fits were repeated at 0.2 and 1.0 and `09_prior_sensitivity.R` collates
  the three widths into `data/prior_sensitivity.csv`.
- Two variants: `metac_absolute = TRUE` (**fixed**, hmetad default, matches the old HMeta-d)
  and `--relative`. Efficiency is insensitive to the choice; meta-Δ is not. Fixed is primary,
  relative is the robustness check.

## Files

```
revision/
  code/
    01_prep_data.R           data/Hmetad_data.csv -> data/hmetad_trial.rds + data/cell_qc.csv
    01b_prep_vmp.R           VMP trials + demographics -> data/hmetad_trial_vmp.rds
    02_prior_predictive.R    prior predictive check (no fit needed)
    03_fit_unified.R         THE fit. --data / --relative / --contrast-sd / --nsub / --iter /
                             --chains / --threads / --seed / --adapt-delta / --max-treedepth / --out
    04_posterior_predictive.R PPC split by gender x domain. --fit / --data / --tag / --ndraws
    05_effects_unified.R     every estimand -> data/effects_<tag>_*.csv. --fit / --tag
    06_subject_estimates.R   per-subject log M and meta-Delta -> data/subject_*.csv. --fit
    07_contrast_draws.R      the two Figure 4 contrasts as DRAWS + labels. --fit / --tag
    08_convergence_metad.R   Rhat / ESS of the reported coefficients -> convergence_metad.csv
    09_prior_sensitivity.R   the gender contrast at prior SD 0.2 / 0.5 / 1.0, both samples
                             -> data/prior_sensitivity.csv
    fig4_metad_posteriors.R  Figure 4 (and, via --tags/--out, Figure S4)
    figS4_metad_relative.R   the relative-criterion entry point into fig4_metad_posteriors.R
    helpers.R                arg parsing, HDI summary, subject subsampling (small on purpose)
    20*-26*, fig2/3/S3/S5    the light behavioural models -- see code/README_behavioural.md
    slurm/                   genome_env.sh, fit.sbatch, ship.sh, fetch_models.sh
  data/                      derived data (committed)
  models/                    fitted .rds (gitignored, regenerated)
  figures/, logs/            generated
```

Every script resolves the repo root itself and runs from any working directory.

## Running

```bash
export R_LIBS_USER=/mnt/penguino/micah/Rlib_hmetad_dev:/mnt/penguino/micah/Rlib_meta_intero

Rscript revision/code/01_prep_data.R
Rscript revision/code/01b_prep_vmp.R
Rscript revision/code/03_fit_unified.R --nsub 8 --iter 200 --chains 1 --out /tmp/pilot.rds
```

The full fits go to **GenomeDK** (account `ecg_general`, partition `short`, 64 cpus, 120 G;
never on the frontend):

```bash
bash revision/code/slurm/ship.sh                      # code + prepared data -> cluster

ssh genome
cd /faststorage/project/ecg_general/metagender
sbatch --account=ecg_general --job-name=fe_fixed revision/code/slurm/fit.sbatch \
  --data revision/data/hmetad_trial.rds --nsub 0 --iter 2000 --chains 4 --threads 16 \
  --out revision/models/fit_firstedition_fixed.rds
# ... + --relative, and --data revision/data/hmetad_trial_vmp.rds, for the other three

bash revision/code/slurm/fetch_models.sh              # .rds back to revision/models/
```

Job ids for the current round are in `revision/logs/genome_jobs.txt`.

## After the fits land

Tags used throughout: `fe_fixed`, `fe_relative`, `vmp_fixed`, `vmp_relative`.

```bash
# 1. every estimand, per fit
for t in fe_fixed fe_relative vmp_fixed vmp_relative; do ... ; done   # 05, then 07
Rscript revision/code/05_effects_unified.R --fit revision/models/fit_firstedition_fixed.rds --tag fe_fixed
Rscript revision/code/07_contrast_draws.R  --fit revision/models/fit_firstedition_fixed.rds --tag fe_fixed

# 2. posterior predictive, the two FIXED fits (both samples)
Rscript revision/code/04_posterior_predictive.R --fit revision/models/fit_firstedition_fixed.rds \
        --data revision/data/hmetad_trial.rds --tag fe_fixed --ndraws 200
Rscript revision/code/04_posterior_predictive.R --fit revision/models/fit_vmp_fixed.rds \
        --data revision/data/hmetad_trial_vmp.rds --tag vmp_fixed --ndraws 200

# 3. convergence of the reported coefficients (all four fits, one file)
Rscript revision/code/08_convergence_metad.R

# 4. subject-level estimates, then the local <-> global link model
Rscript revision/code/06_subject_estimates.R --fit revision/models/fit_firstedition_fixed.rds
Rscript revision/code/25_local_global_link.R

# 5. figures
Rscript revision/code/fig4_metad_posteriors.R      # Figure 4  (fixed-criterion fits)
Rscript revision/code/figS4_metad_relative.R       # Figure S4 (relative-criterion fits)
```

`04` writes `revision/figures/ppc/<tag>/*.png`, `ppc_summary_<tag>.csv` (accuracy and mean
confidence per gender x domain, observed vs predicted) and `ppc_bins_<tag>.csv` (the
observed - predicted residual per confidence bin). The last one carries the check that
matters for a gender claim: whether the model misses the confident bins in OPPOSITE
directions for men and women. It does not. In the extreme-confidence bin (7) the residual
has the SAME sign in every one of the 8 first-edition and 4 VMP domain x accuracy cells;
the few opposite-signed cells are all in bin 6 with |residual| <= 0.006. The largest misfit
anywhere is 0.024 in probability, in the memory task's error trials, where the model
slightly over-predicts high-confidence errors for both genders (a touch more for women,
which if anything shrinks the log M gender contrast rather than inflating it).

`07` writes `draws_<tag>.csv` and `labels_<tag>.csv`, the density and the BF10/ROPE label
for each Figure 4 panel. log M's BF10 and ROPE are copied from `05` so the figure cannot
disagree with the tables; meta-Delta's BF10 is a Savage-Dickey ratio computed there, because
meta-Delta is a nonlinear function of twelve criterion parameters and `brms::hypothesis()`
only handles linear combinations. hmetad defines it in closed form
(`sum_j ((7-j)/6) * exp(eta_j)` over a response side's criterion log-spacings), so the same
map applied to the fit's prior draws gives the prior; `07` checks the closed form against
`add_metacognitive_bias_draws()` and refuses to run if they disagree.

`05` writes `effects_<tag>_{gender,domain,rho,metadelta}.csv`: per-domain and domain-general
gender effects on every dpar (mean, 95% HDI, P(>0), multiplicative effect on M, Savage–Dickey
BF10, % of posterior inside the ±0.05 log-unit ROPE), all pairwise gender × domain differences,
the main effect of domain, the between-domain correlations of log M, and meta-Δ levels and
contrasts per response side and averaged.

Convergence is reported on the **coefficients we report**, not max-Rhat over ~15,000 random
effects: `08_convergence_metad.R` writes `revision/data/convergence_metad.csv`, one row per
fit x variable over the domain cell means, the gender contrasts, the random-effect SDs and
the between-domain correlations of log M, with the criterion-dpar correlations labelled
"not reported" so a bad Rhat among them is not mistaken for a bad Rhat on a reported
quantity. All four fits: max Rhat 1.017, min ESS bulk 268 over 186 (first edition) / 91
(VMP) reported coefficients.
