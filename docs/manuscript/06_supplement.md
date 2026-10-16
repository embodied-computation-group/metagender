# Supplementary material

**Women set a higher bar for confidence: a domain-graded gender difference in local and global metacognitive bias**

Hoogervorst, Banellis, Fardo, Xue, Tyrer, Rahnev & Allen

## Supplementary Note 1. Model specifications

All models were fitted with brms (Bürkner, 2017). The formulas below are as fitted. `domain` is a factor with cell-means coding (`0 + domain`), so every effect is estimated within domain and the domain-general effect is the draw-wise mean of the per-domain coefficients. `gender` is sum-coded (+0.5 women, −0.5 men), `time` (+0.5 post, −0.5 pre) and `accuracy` (+0.5 correct, −0.5 incorrect) likewise. `age` and `education` are z-scored across participants; `task_accuracy`, `calibration_pre` and `update` are z-scored within domain. `sigma ~ ...` gives the residual standard deviation its own linear predictor (log link) per design cell.

**Behavioural models.** Family gaussian, identity link for the mean. One observation per participant × domain cell, split by time or trial accuracy where stated; the outcome is the cell mean.

```
# Task accuracy (per cent correct) and mean reaction time (s)
# participant x domain
accuracy ~ 0 + domain + domain:gender + age + education + (1 | participant),
sigma    ~ 0 + domain

# Global calibration (self-estimated minus actual per cent correct)
# participant x domain x time
calibration ~ 0 + domain + domain:gender + domain:time + domain:gender:time
              + age + education + (1 | participant),
sigma       ~ 0 + domain:time

# Local confidence (mean rating, 1-7); participant x domain x accuracy
confidence ~ 0 + domain + domain:gender + domain:accuracy + domain:gender:accuracy
             + task_accuracy + age + education + (1 | participant),
sigma      ~ 0 + domain:accuracy
# Replication sample: the same model with two domains and without education.

# Reaction time (mean decision time, s); participant x domain x accuracy
rt    ~ 0 + domain + domain:gender + domain:accuracy + domain:gender:accuracy
        + age + education + (1 | participant),
sigma ~ 0 + domain:accuracy

# Staircase variability (within-participant SD of stimulus level, z within domain);
# participant x domain, three staircased domains
staircase_sd ~ 0 + domain + domain:gender + age + education + (1 | participant),
sigma        ~ 0 + domain

# Local-global link; participant x domain, outcome = posterior-mean log M-ratio
# or meta-Delta from the hierarchical model (one fit per outcome)
logM ~ 0 + domain + domain:calibration_pre + domain:update + domain:gender
       + domain:calibration_pre:gender + domain:update:gender
       + age + education + (1 | participant),
sigma ~ 0 + domain
```

Priors, with *s* the standard deviation of the outcome: domain means and covariate slopes normal(0, *s*); every coefficient carrying `gender`, `time` or `accuracy` normal(0, 0.5 *s*), with 0.25 *s* and *s* in the sensitivity analysis; random-effect SD normal(0, *s*) truncated at zero; `sigma` coefficients normal(log *s*, 1). ROPE ±0.1 *s*. Sampler: 4 chains × 4,000 iterations, 1,000 warm-up, adapt_delta 0.95, `sample_prior = "yes"`.

**Hierarchical meta-d′ model.** Family `metad(K = 7, metac_absolute = TRUE)` from the hmetad package (O'Neill et al., 2026): a multinomial likelihood over the 14 response × confidence categories per stimulus class in each participant × domain cell, with response probabilities from the meta-d′ signal-detection model. The relative-criterion variant (Table S10, Figure S4) sets `metac_absolute = FALSE`. The same right-hand side is applied to all fifteen distributional parameters: log M-ratio (`N`, log link), `dprime` and `c` (identity link), and the twelve confidence-criterion log-spacings `metac2zero1diff` to `metac2one6diff` (log link), six per response side, each the log distance from the previous criterion (the first from meta-c).

```
bf(N      ~ 0 + domain + domain:gender + (0 + domain | participant),
   dprime ~ 0 + domain + domain:gender + (0 + domain | participant),
   c      ~ 0 + domain + domain:gender + (0 + domain | participant)) +
lf(metac2zero1diff + ... + metac2one6diff
          ~ 0 + domain + domain:gender + (0 + domain | participant))
```

`(0 + domain | participant)` gives each parameter a participant random effect per domain, correlated across domains within a parameter and independent between parameters; the log M-ratio correlation matrix is the between-domain correlation reported in Table S6. Priors: domain means normal(0, 0.5) for log M-ratio and `c`, normal(1, 0.5) for `dprime`, normal(−0.5, 1) for the log-spacings; every `domain:gender` contrast normal(0, 0.5), with 0.2 and 1.0 in the sensitivity refits; random-effect SDs normal(0, 1) truncated at zero; correlations LKJ(2). ROPE ±0.05 on log M-ratio. Sampler: 4 chains × 2,000 iterations, 1,000 warm-up, adapt_delta 0.95, max_treedepth 12, `sample_prior = "yes"`. meta-Δ for one response side is the mean distance of its six criteria from meta-c, computed from the log-spacings, and is averaged over the two sides; the gender contrast is taken draw-wise at gender = ±0.5.

## Supplementary figures

![**Figure S1. Task accuracy by gender and domain.** Per cent correct per participant in each domain (discovery sample, N = 317). Half boxplots show the median, quartiles and 1.5 IQR whiskers; points are participants. The model-based gender contrasts are in Supplementary Table S12.](revision/figures/FigureS1_accuracy.png)

![**Figure S2. Mean reaction time by gender and domain.** Mean type-1 reaction time per participant in each domain (discovery sample, N = 317). Conventions as in Figure S1.](revision/figures/FigureS2_rt.png)

![**Figure S3. Local confidence in the replication sample.** The replication counterpart of Figure 3 (457 participants; 289 women, 168 men) on the same GDP and Calories general knowledge task and the same seven-point scale. (A) Mean confidence on correct and on incorrect trials, averaged over the two domains; each grey line is one participant, half boxplots show the median, quartiles and 1.5 IQR whiskers. (B) Model-implied mean confidence per domain, gender and trial accuracy, posterior mean with 95% HDI, from the same multilevel model as Figure 3B with two domains and age as the only covariate.](revision/figures/FigureS3_vmp_confidence.png)

![**Figure S4. Relative-criterion robustness fit.** As Figure 4, from the variant of the hierarchical model in which the confidence criteria are placed relative to the type-1 criterion rather than at fixed positions. Estimates are in Table S10.](revision/figures/FigureS4_metad_relative.png)

![**Figure S5. Reaction-time and staircase checks.** (A) Raw mean type-1 reaction time per domain and gender on correct and on error trials (mean ± 95% CI over participants; N = 317). (B) Posterior women − men difference in error slowing (the gender × accuracy interaction) per domain and domain-general; a positive value means women slow down more after an error than men. (C) Raw within-participant SD of the absolute stimulus level, z-scored within domain, per domain and gender; memory used three fixed block-wise difficulty levels and is not shown. (D) Posterior gender contrast on that quantity. Points and intervals are posterior means with 95% HDI; labels give BF10 against a point null.](revision/figures/FigureS5_rt_staircase.png)

![**Figure S6. Posterior predictive checks of the hierarchical model, discovery sample.** Bars show the observed probability of each confidence rating (1–7, x-axis); black points with 95% credible intervals show the corresponding posterior predictive probability. Within each panel, bars are paired by response accuracy (incorrect vs. correct). Columns are task domains (memory, calories, GDP, vision) and rows are gender (masculine, feminine). Summary statistics are in Table S9.](revision/figures/ppc/fe_fixed/fe_fixed_ppc_confidence_by_accuracy.png)

![**Figure S7. Posterior predictive checks of the hierarchical model, replication sample.** Conventions as in Figure S6; the replication battery contains only the two general knowledge domains, so the panel columns are GDP and calories.](revision/figures/ppc/vmp_fixed/vmp_fixed_ppc_confidence_by_accuracy.png)

## Supplementary tables

```{=latex}
\needspace{30\baselineskip}
```

**Table S1.** *Self-belief questionnaire prompts.*

| Domain | Prospective | Retrospective |
|---|---|---|
| Memory | "During the memory test, you will be asked to memorize lists of words. Later, you will see each of these words again, together with a new distractor word. You will have to choose which of the two words you have seen before. There will be about 200 trials in total. Please estimate the total percent of trials you expect to answer correctly on the memory test (0-100%):" | "During the memory test, you were asked to memorize lists of words. Please estimate the total percent of trials you answered correctly on the memory test (0-100%):" |
| Vision | "During the visual perception test, on each trial, you will be asked to view two clouds of dots. You will then decide which cloud has more dots. There will be 200 trials in total. Please estimate the total percent of trials you expect to answer correctly on the vision test (0-100%):" | "During the visual perception test, you viewed two clouds of dots. You then decided which cloud had more dots. Please estimate the total percent of trials you answered correctly on the vision test (0-100%):" |
| General knowledge: GDP | "During the 'trivia' test, you will make decisions about different facts. For example, you will have to decide which of two countries had a higher average gross domestic product over the previous ten years. There will be 100 GDP-decision trials in total. Please estimate the total percent of trials you expect to answer correctly on the GDP-trivia test (0-100%):" | "During the 'trivia' test, you also decided which of two countries had a higher average gross domestic product over the previous ten years. Please estimate the total percent of trials you answered correctly on the GDP-trivia test (0-100%):" |
| General knowledge: Calories | "During the 'trivia' test, you will make decisions about different facts. For example, you will have to decide which of two plates of food has more calories in it. There will be 100 food-decision trials in total. Please estimate the total percent of trials you expect to answer correctly on the food-trivia test (0-100%):" | "During the 'trivia' test, you made decisions about different facts: you decided which of two plates of food had more calories in it. Please estimate the total percent of trials you answered correctly on the food-trivia test (0-100%):" |

::: legend
*Note.* Full text prompts shown to the participant before and after completion of the testing battery. The baseline prompts included a short explanation of each task and the request for a prospective accuracy estimate between 0 and 100 per cent. The post-testing prompt included a request for a retrospective accuracy estimate. Each prompt was tailored to the task it concerned.
:::

{{include: tables/S2.md}}

{{include: tables/S3.md}}

{{include: tables/S4.md}}

{{include: tables/S5.md}}

{{include: tables/S6.md}}

{{include: tables/S7.md}}

{{include: tables/S8.md}}

{{include: tables/S9.md}}

{{include: tables/S10.md}}

{{include: tables/versions.md}}

{{include: tables/S12.md}}
