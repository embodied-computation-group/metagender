# VMP replication layer — state, decisions, and what to do next

**Status: scaffolding complete; the fit is now driven by the shared `03_fit_unified.R`.**
Written 2026-07-12, updated for the unified model. Start here.

---

## Why this exists

The Visceral Mind Project (Aarhus) ran the **same two semantic-trivia domains as the first
edition — GDP and Calories — in an independent sample**, on the **same 7-point confidence
scale**. It is a direct replication sample for the trivia half of this study.

| | first edition | VMP |
|---|---|---|
| Subjects | 318 (146 M / 172 F) | **456 (168 M / 288 F)** |
| Trials | 189,484 | 89,694 |
| Domains | memory, vision, **GDP, Calories** | **GDP, Calories** |
| Confidence | 7-point | 7-point (native — *not* binned) |

**The raw signal, before any modelling** (VMP):

| gender | domain | accuracy | mean confidence (1–7) |
|---|---|---|---|
| Masculin | GDP | 0.702 | **4.42** |
| Feminin | GDP | 0.699 | **3.95** |
| Masculin | Calories | 0.709 | 5.33 |
| Feminin | Calories | 0.710 | 5.18 |

Type-1 accuracy is identical by gender to three decimals in **both** domains, while women
rate confidence ~0.47 points lower on GDP. **The direction matches the published first-edition
finding** (women more conservative / lower confidence). The whole analytic question is where
the model puts that difference — into metacognitive *efficiency*, or into metacognitive *bias*.

---

## The treatment-coding bug — fixed

The first-edition fit used `gender * mod` with treatment coding and mod's reference level at
`memory`, so its `genderFeminin` coefficient was the gender effect **in the memory task alone**,
not the domain-general effect the README claimed. **Fixed**: both samples are now fitted by one
script (`03_fit_unified.R`) with cell-means coding on domain and a sum-coded gender contrast, so
the per-domain effects are `b_mod<X>:fem_c` and the domain-general effect is their draw-wise
mean. The two samples' coefficients are the same quantity and can go in the same table.

## What was built

```
revision/
  code/
    01b_prep_vmp.R        VMP trials + demographics -> data/hmetad_trial_vmp.rds
    03_fit_unified.R      the fit, shared with the first edition (--data ...vmp.rds)
    05_effects_unified.R  gender effects, rho, meta-Delta -> data/effects_<tag>_*.csv
  data/
    vmp/                  source data (trials, demographics + BIDS sidecar)
    hmetad_trial_vmp.rds  prepared, 456 subjects, 89,694 trials, K=7
    vmp_cell_qc.csv       7 of 914 cells dropped
```

### Design decisions baked in

- **K = 7, native.** The sibling `meta_intero_brain` project collapsed trivia confidence to 4
  bins *only* to make it commensurate with its cardiac/respiratory tasks. Nothing here needs
  that, and K=7 is what makes the two fits comparable. **Do not bin.**
- **Cell-means domain coding with a sum-coded gender contrast** (`0 + mod + mod:fem_c`,
  `fem_c` ∈ {−0.5, +0.5}), so `b_mod<X>:fem_c` is the gender effect within domain X and the
  domain-general effect is their mean. Sign convention **Feminin − Masculin**, matching the
  published Figure 4 direction.
- **Priors identical to the first-edition fit** — the same script fits both samples, so the
  replication is prior-matched, not merely structure-matched.
- **Random effects are cell-means** (`0 + mod | subj`), unlike the fixed effects — so
  `cor_subj__modGDP__modCalories` is the between-domain correlation of log M as a *parameter*.
  (The sibling project found this ≈ 0 within trivia, which is worth testing here.)
- **Gender enters all 15 dpars**, not just log M. Deny the twelve type-2 criteria the freedom
  to vary by gender and the confidence difference above has nowhere to go but log M —
  manufacturing a spurious efficiency effect. The sibling project has been bitten by exactly
  this (fixed criteria reversed a headline). Any gender effect on log M is therefore
  **conditional on** gender effects in sensitivity, bias, and confidence-scale usage.
- **Stimulus is reconstructed, not recorded**, in VMP (`response` on correct trials, its
  complement on errors). For 2AFC this is *exact*, not approximate. The first edition records
  `signal` directly. Both are valid; recorded so nobody mistakes it for a bug.
- **Gender = "Other" (n=1) dropped.** One subject cannot identify a coefficient.
- **QC:** cells with d′ ≤ 0 (the log link on M cannot represent them) or < 30 trials. Drops
  7 of 914 cells. Pass `--no-qc` to match the first edition's filtering exactly.

---

## Running it

```bash
Rscript revision/code/01b_prep_vmp.R          # fast; idempotent
# then the cluster, via revision/code/slurm/fit.sbatch -- see revision/README.md
Rscript revision/code/05_effects_unified.R --fit revision/models/fit_vmp_fixed.rds --tag vmp_fixed
```

**Cost.** Measured pilot: 30 subjects × 600 iter = 5m05s at `adapt_delta = 0.99`. The full fit
is 15× the random effects and 3.3× the draws; honest band **3–6 h**, and the unified fits run at
`adapt_delta = 0.95` with within-chain threading. Do **not** extrapolate from early warmup — on
the sibling fit the slow chains sat at ~2 iterations/min for 25 minutes (implying 16 h) and then
accelerated 25× once adaptation settled, finishing in 1h11m.

### Then, in priority order

1. **PPC.** `04_posterior_predictive.R` takes `--fit` and `--data`, so it runs on either
   sample, split by gender × domain. The check that matters is not overall fit but
   whether misfit is *gender-specific*: if scale usage leaked into log M, the model will miss
   the confident bins in **opposite directions** for men and women. On the sibling K=4 fit that
   gap was 0.0003 — no leak.
2. **meta-Δ is wired** (`05_effects_unified.R`). It is the principled bias measure
   (O'Neill, Zhan & Fleming, 2026), defined net of d′, c and meta-d′ — unlike mean confidence,
   which is confounded by all three (Sherman et al., 2018) and is what the first edition leans on.
   **Caveat that must be reported:** meta-Δ estimated under the *fixed* model is biased in a
   non-linear, *response-specific* way when the data are generated by the *relative* model,
   unless c = 0 or meta-d′ = d′. Neither escape clause is likely to hold. Refit under the
   relative model and confirm the contrast survives.

### The sibling result this is replicating

The same analysis on VMP trivia at K=4 (in `meta_intero_brain`, now removed and superseded by
this directory) found a clean **dissociation**:

- **M-ratio: no gender effect.** Male − female −0.060 log units, 95% HDI [−0.130, 0.015].
- **meta-Δ: a reliable bias effect.** Male − female −0.168 [−0.213, −0.124] (GDP) and
  −0.036 [−0.063, −0.010] (Calories) — i.e. **women's criteria sit further from meta-c: women
  use lower confidence, are more conservative, are less metacognitively biased.**
- **d′: nothing** in either domain.
- **ρ(log M, GDP↔Calories) = −0.07** [−0.30, 0.15] — efficiency does not correlate across two
  domains *of the same task*.

Those numbers came from a **K=4** fit and will shift under K=7. They are the prior expectation,
not the result. The direction — women more conservative — matches the first edition's published
finding.
