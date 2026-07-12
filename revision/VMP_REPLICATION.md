# VMP replication layer — state, decisions, and what to do next

**Status: scaffolding complete and tested on a pilot. The full fit has NOT been run.**
Written 2026-07-12. Start here.

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

## ⚠ A bug in `03_fit_model.R` that must be fixed before the revision is submitted

`revision/README.md` says:

> Fixed `gender * mod`: `genderFeminin` on `mu` = **domain-general** gender difference in efficiency

**It is not.** `gender * mod` uses R's default **treatment coding**, and `mod`'s reference level
is `memory`. Under treatment coding, `genderFeminin` is the gender effect **at the reference
modality only** — a simple effect in the *memory* task. It is not an average over domains, and
relabelling the modalities would change its value.

The domain-general effect is the **average of the four modality-specific simple effects**.

**This does not require a refit.** It can be computed from the existing draws:

```r
# domain-general gender effect = mean of the four simple effects
g   <- dr$b_genderFeminin                         # effect at the reference level (memory)
gm  <- dr[, grep("^b_genderFeminin:mod", names(dr))]   # deviations for the other 3 domains
domain_general <- rowMeans(cbind(g, g + gm))      # memory + the three others
```

Alternatively sum-code `mod` (`contr.sum`) and refit, after which `genderFeminin` *is* the
domain-general effect by construction. **`03b_fit_vmp.R` takes the sum-coding route** — see
its header — so the VMP numbers are domain-general as labelled.

**Consequence for comparing the two fits:** the first edition's `genderFeminin` and VMP's
`b_fem_c` are *different quantities* until the first edition's is recomputed as above. Do not
put them in the same table before then.

---

## What was built

```
revision/
  code/
    01b_prep_vmp.R        VMP trials + demographics -> data/hmetad_trial_vmp.rds   [TESTED]
    03b_fit_vmp.R         the replication fit -> models/fit_vmp.rds                [PILOTED]
    03b_fit_vmp.sbatch    cluster runner                                           [NOT RUN]
    05b_effects_vmp.R     gender effects, rho, meta-Delta -> data/vmp_*.csv        [TESTED]
  data/
    vmp/                  source data (trials, demographics + BIDS sidecar)
    hmetad_trial_vmp.rds  prepared, 456 subjects, 89,694 trials, K=7
    vmp_cell_qc.csv       7 of 914 cells dropped
```

### Design decisions baked in

- **K = 7, native.** The sibling `meta_intero_brain` project collapsed trivia confidence to 4
  bins *only* to make it commensurate with its cardiac/respiratory tasks. Nothing here needs
  that, and K=7 is what makes the two fits comparable. **Do not bin.**
- **Fixed effects sum-coded** (`fem_c`, `gdp_c` ∈ {−0.5, +0.5}), so `b_fem_c` *is* the
  domain-general gender effect and the sign convention is **Feminin − Masculin**, matching the
  published Figure 4 direction.
- **Priors are the first edition's, unchanged.** They map across because sum coding keeps the
  intercept, so the `Intercept` prior class survives. The replication is prior-matched, not
  merely structure-matched.
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

## What to do next

```bash
export R_LIBS_USER=/mnt/penguino/micah/Rlib_meta_intero
cd /mnt/penguino/micah/metagender

Rscript revision/code/01b_prep_vmp.R        # fast; already run, rerun is idempotent
sbatch  revision/code/03b_fit_vmp.sbatch    # THE LONG JOB — not yet submitted
Rscript revision/code/05b_effects_vmp.R     # after it lands
```

**Cost.** Measured pilot: 30 subjects × 600 iter = **5m05s**, 4 divergences at
`adapt_delta = 0.99`. The full fit is 15× the random effects and 3.3× the draws. Honest band:
**3–6 h**. Do **not** extrapolate from early warmup — on the sibling fit the slow chains sat at
~2 iterations/min for 25 minutes (implying 16 h) then accelerated 25× once adaptation settled,
finishing in 1h11m.

### Then, in priority order

1. **Fix the domain-general coefficient in the first-edition fit** (above). It is a headline
   claim in a paper under revision, and it needs no refit.
2. **Port the PPC.** `04_posterior_predictive.R` exists for the first-edition fit; the VMP fit
   needs the same, split by gender × domain. The check that matters is not overall fit but
   whether misfit is *gender-specific*: if scale usage leaked into log M, the model will miss
   the confident bins in **opposite directions** for men and women. On the sibling K=4 fit that
   gap was 0.0003 — no leak.
3. **meta-Δ is already wired** (`05b_effects_vmp.R`). It is the principled bias measure
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
