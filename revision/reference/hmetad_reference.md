# hmetad Reference (generated)

`hmetad` is an R package for Bayesian meta-d′ (metacognition) modeling built on top
of **brms** / **Stan**. It extends the original HMeta-d MATLAB toolbox (Fleming, 2017).

**This file is a generated reference** compiled from the official documentation. Source URLs:

- <https://metacoglab.github.io/hmetad/articles/hmetad.html> (Getting-started vignette)
- <https://metacoglab.github.io/hmetad/articles/parameterization.html> (Parameterization vignette)
- <https://metacoglab.github.io/hmetad_workshop/tutorial/> (Workshop tutorial; source read from
  `tutorial/index.qmd` in `github.com/metacoglab/hmetad_workshop`, "Hierarchical models for
  repeated measures" section and surrounding brms content)

Code is reproduced verbatim from the sources wherever possible. Where two sources show slightly
different call styles, both are shown and the difference is flagged rather than reconciled.

Typical package setup used throughout the docs:

```r
library(tidyverse)         # for plotting and data wrangling
library(hmetad)            # for confidence modeling
library(tidybayes)         # for wrangling posteriors
library(bayestestR)        # for significance testing

options(mc.cores=parallel::detectCores())   # fit models in parallel
```

---

## 1. How the model expects data to be formatted

### Trial-level (long) input columns

`hmetad` expects **trial-level (long) data**, one row per trial, with these core columns:

| Column       | Meaning                              | Coding / type                                         |
|--------------|--------------------------------------|-------------------------------------------------------|
| `trial`      | trial identifier                     | integer (optional; present in simulated data)         |
| `stimulus`   | which stimulus was presented         | `0` or `1`                                            |
| `response`   | the type-1 (decision) response       | `0` or `1`                                            |
| `confidence` | the type-2 confidence rating         | integer `1 : K`, where `K` = number of confidence levels |

The workshop describes the simulated data as: "one row for each ... trial, the `stimulus` that was
presented on that trial (`0` or `1`), the simulated type 1 `response` (`0` or `1`), and the
simulated `confidence` level (`1`, `2`, `3`, or `4`)."

Verified against the installed package's `example_data` (1,000 trials), whose columns are:

```
trial, stimulus, response, correct, confidence,          # observed data
dprime, c, meta_dprime, M, meta_c2_0, meta_c2_1          # ground-truth parameters (simulated only)
```

Only `stimulus`, `response`, and `confidence` are required for fitting; `correct` is convenience,
and the remaining columns are the known ground-truth parameters that ship with the simulated
example (real datasets won't have them).

Confidence is coded on a **1..K** scale (e.g. `1:4` for four levels, `1:3` for three). `K` is
inferred from the data (`n_distinct(data$confidence)` in helper code). The number of confidence
**thresholds** the model estimates is `K - 1` per type-1 response.

Example simulated dataset:

```r
d <- sim_metad(N_trials=10000, dprime=1.5)
d
```

### Grouping columns (participants, conditions)

Extra columns such as `participant` or `condition` are added as ordinary columns and are used as
predictors / grouping factors in the model formula. In the workshop these are made factors, e.g.
`mutate(participant=factor(participant))` and `mutate(condition=factor(condition))`.

Per the hmetad vignette: "If you have other columns in your dataset (e.g., `participant` or
`condition` columns) that you would like to be aggregated separately, you can simply add them to
the function call."

### Aggregation: trial-level vs. aggregated

- `fit_metad()` accepts **trial-level** data and aggregates it automatically.
- You can pre-aggregate with `aggregate_metad()`. Grouping columns are passed as extra
  arguments, e.g. `aggregate_metad(d)` or `aggregate_metad(d, participant)` or
  `aggregate_metad(d2, participant)`.
- After aggregation, the model's outcome is a **count** referred to as `N` (this is the left-hand
  side of the model formula, e.g. `N ~ 1`). Under the default multinomial likelihood the counts
  of each joint response category are modeled.

```r
d.summary <- aggregate_metad(d)          # optional; fit_metad() does this internally
```

### Alternative / convertible response codings

The hmetad vignette documents helpers for other data layouts:

- **Joint response** — a single value combining decision + confidence on a `1:2K` scale.
  Build one from separate columns, or split one apart (signatures verified against installed v0.1.0):
  ```r
  joint_response(response, confidence, K)   # combine decision + rating -> 1:2K value
  type1_response(joint_response, K)         # extract the type-1 decision back out
  type2_response(joint_response, K)         # extract the confidence rating back out
  ```
- **Signed vs. unsigned** — convert between signed coding (`-1, 1`) and unsigned (`0, 1`):
  ```r
  to_unsigned(x)   # -1/1  -> 0/1
  to_signed(x)     #  0/1  -> -1/1
  ```

### Simulating data (used throughout the docs)

Single "participant":

```r
d <- sim_metad(
  N_trials=50, log_M=-0.5,
  dprime=1.5, c=0,
  c2_0_diff=c(0.5, 0.5), c2_1_diff=c(0.5, 0.5)
)
```

The installed CRAN v0.1.0 `sim_metad()` signature is:

```r
sim_metad(N_trials = 100, dprime = 1, c = 0, log_M = 0,
          c2_0_diff = rep(0.5, 3), c2_1_diff = rep(0.5, 3),
          metac_absolute = TRUE, summarize = FALSE,
          lcdf = normal_lcdf, lccdf = normal_lccdf)
```

So the criteria are supplied as **successive log-distance** vectors `c2_0_diff` / `c2_1_diff`
(defaults `rep(0.5, 3)`, i.e. K = 4), matching the model's `...diff` parameterization (Section 2).
Some published vignette examples pass absolute criteria as `c2_0` / `c2_1` instead — those
correspond to a different package version than the installed CRAN build, whose formals are only
`c2_0_diff` / `c2_1_diff`.

Multiple participants (draws each participant's parameters from a normal population):

```r
d <- sim_metad_participant(
  N_participants=10, N_trials=50,
  mu_log_M=-0.5, sd_log_M=0.1,
  mu_dprime=1, sd_dprime=0.1,
  mu_c=0, sd_c=0.1,
  mu_z_c2_0=c(-0.5, -0.5), sd_z_c2_0=c(0.25, 0.25), r_z_c2_0=diag(2),
  mu_z_c2_1=c(-0.5, -0.5), sd_z_c2_1=c(0.25, 0.25), r_z_c2_1=diag(2)
) |>
  mutate(participant=factor(participant))
```

Multiple participants with within-participant condition:

```r
d <- sim_metad_participant_condition(
  N_participants=20, N_trials=200,
  mu_log_M=c(0, -1)
) |>
  mutate(condition=factor(condition))
```

`resimulate(d)` regenerates data from the same ground-truth parameters (used to demonstrate
sampling variability).

---

## 2. Key parameterization issues

The parameterization vignette explains that `hmetad` reparameterizes the meta-d′ model so that
every fitted coefficient is **unconstrained** (range −∞ to ∞), which makes Hamiltonian Monte
Carlo sampling efficient. brms formulas therefore predict these unconstrained latent parameters,
not the natural-scale quantities directly.

### Type-1 parameters

- **d′** (type-1 sensitivity) and **c** (type-1 criterion) are estimated directly as
  unconstrained variables — no transformation.
- In SDT terms: d′ is the distance between the S=0 and S=1 evidence distributions; c is the
  decision criterion (respond R=0 below c, R=1 above c).

### Type-2 sensitivity: meta-d′ via the M-ratio (log scale)

Rather than fitting meta-d′ directly, the package fits the **M-ratio** `M = meta-d′ / d′` on the
**log scale**, because M is bounded at zero:

```
log M = log(meta-d' / d')
meta-d' = e^(log M) * d'
```

Consequences called out by the docs:

- The **main / default latent parameter** of the model is `log(M)`. In `tidybayes`-style
  extraction, the `.linpred` column contains samples of `log(M)` by default; set
  `transform=TRUE` to get `M` on its natural scale (see Section 3 helpers).
- By default brms places a **wide t-distribution prior on `log(M)`** and (in the base model)
  **flat/no priors on the other parameters** — the docs strongly recommend setting priors (see
  pitfalls below).

### Type-2 criteria (confidence thresholds) — ordering constraints and `diff` parameters

There are `K - 1` confidence criteria for each type-1 response: `meta-c_{2,1:K}^0` (for "0"
responses) and `meta-c_{2,1:K}^1` (for "1" responses), plus the type-1-for-type-2 threshold
`meta-c`. They carry strict ordering constraints:

- `meta-c_{2,1:K}^0` must be **strictly decreasing** and **less than `meta-c`**.
- `meta-c_{2,1:K}^1` must be **strictly increasing** and **greater than `meta-c`**.

To enforce this while keeping parameters unconstrained, the package estimates the **log of the
distance between successive criteria** (hence the `diff` suffix in parameter names):

```
dmeta-c_2,k^0 = meta-c            - meta-c_2,1^0     (if k = 1)
                meta-c_2,k-1^0    - meta-c_2,k^0     (if k >= 2)

dmeta-c_2,k^1 = meta-c_2,1^1      - meta-c           (if k = 1)
                meta-c_2,k^1      - meta-c_2,k-1^1   (if k >= 2)
```

Reconstruction of the natural-scale criteria:

```
meta-c_2^0 = meta-c - cumulative_sum(e^(dmeta-c_2^0))
meta-c_2^1 = meta-c + cumulative_sum(e^(dmeta-c_2^1))
```

**Parameter (coefficient) names** used in brms formulas for these differences are
`metac2zero<k>diff` and `metac2one<k>diff`, e.g. for `K = 3` (two thresholds per response):
`metac2zero1diff`, `metac2zero2diff`, `metac2one1diff`, `metac2one2diff`. For `K = 4` there are
three each: `metac2zero1diff`, `metac2zero2diff`, `metac2zero3diff`, `metac2one1diff`,
`metac2one2diff`, `metac2one3diff`.

Use `metac2_parameters(K = ...)` to list the confidence-threshold parameter names for a given
number of confidence levels; it is also usable inside prior specifications:

```r
metac2_parameters(K=3)
```

### meta-c: absolute vs. relative parameterization

Where `meta-c` (the type-1 threshold used by the type-2 process) is fixed is controlled by the
`metac_absolute` argument to the model:

- **Absolute (default):** `meta-c = c`. Matches previous HMeta-d toolbox versions.
- **Relative:** `meta-c / meta-d' = c / d'`, implemented as `meta-c = M * c`. This is the
  original Maniscalco & Lau formulation. Enable with `metac_absolute = FALSE`.

### Likelihood: multinomial (default) vs. categorical

- **Multinomial (default):** aggregates the data and evaluates the likelihood twice (once per
  stimulus) — more efficient.
- **Categorical:** trial-by-trial evaluation. Use `categorical = TRUE` when you need trial-level
  effects or crossed random effects.

### Metacognitive bias: meta-Δ (recommended derived measure)

Because there are several confidence criteria, the docs recommend summarizing metacognitive
*bias* (overall confidence level) with a derived measure called **meta-Δ** rather than
interpreting each criterion. It is computed per type-1 response, or averaged
(`metacognitive_bias_draws(..., by_response=FALSE)`). See Section 3.

### Common pitfalls the docs call out

- **Flat default priors are dangerous.** Without priors, `d′` (and other parameters) can take
  "literally any value"; brms also cannot sample from its default flat priors for a prior
  predictive check. The docs urge setting weakly-informative priors and doing prior predictive
  checks *before* seeing data.
- **Reasonable prior ranges** suggested for a typical psychology task: M-ratios and d′ roughly
  `0.5`–`2`; c roughly `−1`–`1`; distances between confidence thresholds roughly `0.1`–`5`.
- **Everything is on a latent scale.** Coefficients/priors act on `log(M)`, `d′`, `c`, and the
  log-distance `...diff` parameters — not on natural-scale meta-d′ or the raw criteria. Remember
  to back-transform (`transform=TRUE`, or the `*_metad` helpers) when interpreting.
- **Small per-participant datasets are noisy** — motivating hierarchical partial pooling
  (Section 3).

---

## 3. Specifying the hierarchical model and random effects with brms

### The fitting function and formula style

Models are fit with **`fit_metad()`**, which wraps `brms::brm()`. The formula is a brms
**multivariate `bf()`** with **one sub-formula per model parameter**. The outcome `N` (the
aggregated count) gets the M-ratio (`log M`) linear predictor; the remaining parameters
(`dprime`, `c`, and the `metac2...diff` confidence-threshold parameters) each get their own
sub-formula:

```r
m <- fit_metad(
  bf(
    N ~ 1,
    dprime ~ 1,
    c ~ 1,
    metac2zero1diff ~ 1,
    metac2zero2diff ~ 1,
    metac2one1diff ~ 1,
    metac2one2diff ~ 1
  ),
  data=d
)
```

(The example above is for `K = 3`, i.e. two thresholds per response. Use `metac2_parameters(K)`
to get the right set of `...diff` parameters for your `K`.)

**Priors** are set with `brms::set_prior()` / `prior()`. Priors on the confidence-threshold
parameters are conveniently targeted with `dpar = metac2_parameters(K = ...)`. The `Intercept`
class with no `dpar` targets the main parameter (`log M`); `dpar='dprime'`, `dpar='c'`, etc.
target the other distributional parameters:

```r
m_prior <- update(m, sample_prior='only',
                  prior=set_prior('normal(0, 0.5)', class='Intercept') +
                    set_prior('normal(1, 0.5)', class='Intercept', dpar='dprime') +
                    set_prior('normal(0, 0.5)', class='Intercept', dpar='c') +
                    set_prior('normal(-0.5, 1)', class='Intercept',
                              dpar=metac2_parameters(K=3)))
```

> Note on call styles: the **hmetad getting-started vignette** shows a more compact form where
> `fit_metad()` is given a single formula `N ~ 1` and priors are set with parameter **classes**
> (`class = dprime`, `class = c`, `class = metac2_parameters(K = 4)`) rather than `dpar=`, e.g.:
>
> ```r
> m <- fit_metad(N ~ 1,
>   data = d, init = 0,
>   prior = prior(normal(0, 1), class = Intercept) +
>     prior(normal(0, 1), class = dprime) +
>     prior(normal(0, 1), class = c) +
>     set_prior("lognormal(0, 1)", class = metac2_parameters(K = 4))
> )
> ```
>
> The workshop tutorial consistently uses the fuller `bf(...)` + `dpar=` form shown above.
>
> **Both forms are confirmed valid** (verified against the shipped `hmetad` vignette source and
> the family object in the installed v0.1.0):
> - `fit_metad()` **auto-expands a lone `N ~ 1`** into an intercept for every model parameter, so
>   the compact form works. The getting-started vignette fits exactly `fit_metad(N ~ 1, prior =
>   prior(normal(0,1), class = Intercept) + prior(normal(0,1), class = dprime) + ...)`.
> - `dprime`, `c`, `metac2zero1diff`, `metac2one1diff`, … **are genuine brms distributional
>   parameters (`dpar`s)** of the custom family. You can therefore address them *either* as a
>   prior `class=` (compact form) *or* via `dpar=` on the `Intercept`/`b`/`sd` class (multivariate
>   `bf()` form). The family's dpars are: `mu` (= log M, `log` link — targeted by `Intercept` with
>   no `dpar`), `dprime` and `c` (`identity` link), and `metac2zero<k>diff` / `metac2one<k>diff`
>   (`log` link). Use the `bf()`/`dpar=` form whenever you add predictors or random effects, since
>   each parameter then needs its own sub-formula.

### Custom family and stanvars (manual brm() route)

`hmetad` provides a custom brms family and its Stan support functions. The vignette indicates
manual fitting via `brm()` uses:

```r
brm(bf(...), data = aggregate_metad(d, ...),
    family = metad(K = K, ...),
    stanvars = stanvars_metad(K = K, ...))
```

i.e. `family = metad(K = K, ...)` together with `stanvars = stanvars_metad(K = K, ...)`.
`fit_metad()` is the recommended wrapper that assembles the family, stanvars, and aggregation for
you. Verified signatures from the installed v0.1.0:

```r
metad(K, distribution = "normal", metac_absolute = TRUE, categorical = FALSE)
stanvars_metad(K, distribution = "normal", metac_absolute = TRUE, categorical = FALSE)
```

The two must be built with the **same** `K` / `distribution` / `metac_absolute` / `categorical`
so the family and its Stan code agree — which is exactly what `fit_metad()` guarantees for you.

### Adding fixed effects for participant (no pooling)

Fitting each participant independently — a fixed effect of `participant` in every sub-formula.
The tutorial uses sum-to-zero contrasts so the `Intercept` is the group mean and `participantX`
terms are deviations:

```r
# use sum-to-zero contrast coding
contrasts(d$participant) <- contr.sum

m_spe <- fit_metad(
  bf(
    N ~ 1 + participant,
    dprime ~ 1 + participant,
    c ~ 1 + participant,
    metac2zero1diff ~ 1 + participant,
    metac2zero2diff ~ 1 + participant,
    metac2one1diff ~ 1 + participant,
    metac2one2diff ~ 1 + participant
  ),
  data=d, init=0,
  prior=set_prior('normal(0, 0.5)', class='Intercept') +
    set_prior('normal(1, 0.5)', class='Intercept', dpar='dprime') +
    set_prior('normal(0, 0.5)', class='Intercept', dpar='c') +
    set_prior('normal(-0.5, 1)', class='Intercept',
              dpar=metac2_parameters(K=3)) +
    set_prior('normal(0, 1)', class='b') +
    set_prior('normal(0, 1)', class='b', dpar=c('dprime', 'c', metac2_parameters(K=3)))
)
```

Here fixed-effect coefficients use prior `class='b'` (per distributional parameter via `dpar=`).

### Hierarchical / multilevel model (random effects, partial pooling)

To make it hierarchical, the tutorial says only two changes are needed:

1. Replace the fixed `participant` predictor with a **group-level term** `(1 | participant)`, so
   each parameter has a group-mean intercept plus a per-participant offset.
2. Put a prior on the **standard deviation** of the between-participant differences (prior
   `class='sd'`) instead of on the differences themselves.

```r
m_he <- fit_metad(
  bf(
    N ~ 1 + (1 | participant),
    dprime ~ 1 + (1 | participant),
    c ~ 1 + (1 | participant),
    metac2zero1diff ~ 1 + (1 | participant),
    metac2zero2diff ~ 1 + (1 | participant),
    metac2one1diff ~ 1 + (1 | participant),
    metac2one2diff ~ 1 + (1 | participant)
  ),
  data=d, init=0,
  prior=set_prior('normal(0, 0.5)', class='Intercept') +
    set_prior('normal(1, 0.5)', class='Intercept', dpar='dprime') +
    set_prior('normal(0, 0.5)', class='Intercept', dpar='c') +
    set_prior('normal(-0.5, 1)', class='Intercept',
              dpar=metac2_parameters(K=3)) +
    set_prior('normal(0, 1)', class='sd') +
    set_prior('normal(0, 1)', class='sd', dpar=c('dprime', 'c', metac2_parameters(K=3)))
)
```

The resulting summary is split into `Multilevel Hyperparameters` (between-participant variability)
and `Regression Coefficients` (population-average parameters), while still containing
per-participant estimates. This produces **partial pooling** (shrinkage toward the group mean),
which the tutorial argues gives better per-participant estimates.

### Hierarchical model with a within-participant condition

Group-level *and* participant-level effects of a `condition` factor — the `condition | participant`
term estimates correlated random slopes/intercepts per participant. This example uses `K = 4`
(three thresholds per response, so `...3diff` parameters appear):

```r
m <- fit_metad(
  bf(
    N ~ condition + (condition | participant),
    dprime ~ condition + (condition | participant),
    c ~ condition + (condition | participant),
    metac2zero1diff ~ condition + (condition | participant),
    metac2zero2diff ~ condition + (condition | participant),
    metac2zero3diff ~ condition + (condition | participant),
    metac2one1diff ~ condition + (condition | participant),
    metac2one2diff ~ condition + (condition | participant),
    metac2one3diff ~ condition + (condition | participant)
  ),
  data=d, init=0,
  prior=set_prior('normal(0, 0.5)', class='Intercept') +
    set_prior('normal(1, 0.5)', class='Intercept', dpar='dprime') +
    set_prior('normal(0, 0.5)', class='Intercept', dpar='c') +
    set_prior('normal(-0.5, 1)', class='Intercept',
              dpar=metac2_parameters(K=4)) +
    set_prior('normal(0, 1)', class='b') +
    set_prior('normal(0, 1)', class='b', dpar=c('dprime', 'c', metac2_parameters(K=4))) +
    set_prior('normal(0, 1)', class='sd') +
    set_prior('normal(0, 1)', class='sd', dpar=c('dprime', 'c', metac2_parameters(K=4)))
)
```

**Prior classes summary** (standard brms semantics):
- `class='Intercept'` — population intercept of a parameter (target parameter via `dpar=`; no
  `dpar` = the main `log M` parameter).
- `class='b'` — population-level (fixed) regression coefficients.
- `class='sd'` — standard deviations of group-level (random) effects.

`init=0` is passed to help sampling initialize on the constrained/log scales.

### Extracting and post-processing the posterior (tidybayes-style)

`hmetad` mirrors `tidybayes` extraction functions and adds `*_metad` convenience variants that
return all parameters on their natural scale. Verbatim examples:

```r
newdata <- distinct(d, participant)          # or tibble(participant=1) for one subject

# expected joint-response probabilities (posterior predictive expectation)
draws <- epred_draws_metad(m, newdata)

# ROC draws
roc1_draws(m, newdata, bounds=TRUE)
roc2_draws(m, newdata, bounds=TRUE)

# latent parameter draws; .linpred is log(M) by default
linpred_draws(m_he, newdata)
linpred_draws(m_he, newdata, transform=TRUE)              # natural scale (M instead of log M)
linpred_draws(m_he, newdata, dpar=c('dprime', 'c'))       # request other parameters
linpred_draws_metad(m_he, newdata)                        # all parameters, natural scale

# average over participants (ignore random effects)
linpred_draws_metad(m_he, tibble(.row=1), re_formula=NA)

# metacognitive bias (meta-Delta), per response or averaged
metacognitive_bias_draws(m_he, newdata)
metacognitive_bias_draws(m_he, newdata, by_response=FALSE)
```

- `re_formula=NA` drops participant-level (random) effects to get population-average predictions;
  the default includes all random effects (per-participant predictions).
- The three prediction levels follow tidybayes conventions: `linpred_draws` (model parameters),
  `epred_draws` (expected predicted probabilities), `predicted_draws` (posterior predictive of
  the data).

`update()` refits with changed priors/data, e.g. `update(m, sample_prior='only', ...)` for prior
predictive checks, `update(m_prior, sample_prior='no')` to fit for real, and
`update(m_he, newdata=aggregate_metad(d2, participant))` to refit on new data.

---

## Appendix: verified function signatures (installed CRAN v0.1.0)

Pulled directly from the installed package via `args()`, so these are exact for the version in this
environment:

```r
fit_metad(formula, data, ...,
          aggregate = TRUE,
          .stimulus = "stimulus", .response = "response",
          .confidence = "confidence", .joint_response = "joint_response",
          K = NULL, distribution = "normal", metac_absolute = TRUE,
          stanvars = NULL, categorical = FALSE)

aggregate_metad(data, ...,
                .stimulus = "stimulus", .response = "response",
                .confidence = "confidence", .joint_response = "joint_response",
                .name = "N", K = NULL)

metad(K, distribution = "normal", metac_absolute = TRUE, categorical = FALSE)
stanvars_metad(K, distribution = "normal", metac_absolute = TRUE, categorical = FALSE)

joint_response(response, confidence, K)
type1_response(joint_response, K)
type2_response(joint_response, K)
to_unsigned(x)
to_signed(x)

sim_metad(N_trials = 100, dprime = 1, c = 0, log_M = 0,
          c2_0_diff = rep(0.5, 3), c2_1_diff = rep(0.5, 3),
          metac_absolute = TRUE, summarize = FALSE,
          lcdf = normal_lcdf, lccdf = normal_lccdf)
sim_metad_participant(N_participants = 100, N_trials = 100,
          mu_dprime = 1, sd_dprime = 0.5, mu_c = 0, sd_c = 0.5,
          mu_log_M = 0, sd_log_M = 0.5,
          mu_z_c2_0 = rep(-1, 3), sd_z_c2_0 = rep(0.1, 3), r_z_c2_0 = diag(3),
          mu_z_c2_1 = rep(-1, 3), sd_z_c2_1 = rep(0.1, 3), r_z_c2_1 = diag(3),
          metac_absolute = TRUE, summarize = FALSE,
          lcdf = normal_lcdf, lccdf = normal_lccdf)

epred_draws_metad(object, newdata, ...)
metacognitive_bias_draws(object, newdata, ..., by_response = TRUE)
```

Note: `fit_metad()` takes column names via `.stimulus` / `.response` / `.confidence` (rename these
if your columns differ) and turns aggregation off with `aggregate = FALSE` if your data is already
aggregated.

## Notes on sourcing and residual caveats

- The `hmetad` **vignettes ship with the installed package** as `.Rmd` source and are the most
  authoritative reference. On this machine:
  `~/Library/R/arm64/4.4/library/hmetad/doc/` — `hmetad.Rmd`, `parameterization.Rmd`,
  `categorical.Rmd`, `alternative_distributions.Rmd`, `history.Rmd`. The hierarchical `bf()` +
  `dpar=` examples in Section 3 were cross-checked against `categorical.Rmd`, and the compact
  `class=` form against `hmetad.Rmd`.
- A copy of the workshop helper functions is saved alongside this reference at
  `hmetad_tutorial_utils.R` (plotting/utility functions such as `plot_joint_response()`), pulled
  from `github.com/metacoglab/hmetad_workshop`.
- Residual caveat: published web-vignette examples occasionally reflect a **newer/different package
  version** than the installed CRAN v0.1.0 (most visibly `c2_0`/`c2_1` vs. `c2_0_diff`/`c2_1_diff`
  in `sim_metad()`). Where they diverge, the signatures in the appendix above win for *this*
  environment. Run `?fit_metad` / `browseVignettes("hmetad")` locally for the definitive, version-
  matched docs.
