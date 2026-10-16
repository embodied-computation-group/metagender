# Revision Figure S5 -- reaction time and staircase difficulty, the two confounds
# =============================================================================
# Reviewer 3 raised two alternative accounts of the gender difference that have
# nothing to do with metacognition:
#   1. IMPULSIVITY. If men simply respond faster and slow down less after an error,
#      an apparent confidence or efficiency difference could be a response-style
#      difference. Panels A and B: type-1 RT on correct and error trials, and the
#      gender x accuracy interaction (error slowing) from model 23.
#   2. STAIRCASE DIFFICULTY (Rahnev & Fleming, 2019). If the adaptive staircase
#      placed one gender's trials at a more variable stimulus level, meta-d' would
#      differ for reasons of task design. Panels C and D: the within-subject SD of
#      the stimulus level, z-scored within domain, and its gender contrast from
#      model 24. Memory has no staircase (three fixed block levels) and is absent.
#
# A  raw mean RT per domain x gender, split by trial accuracy (mean +/- 95% CI over
#    participants; nothing modelled)
# B  model 23: gender x accuracy (error slowing, Women - Men) per domain and
#    domain-general, posterior mean and 95% HDI, with BF10
# C  raw staircase SD (z-scored within domain) per domain x gender
# D  model 24: gender contrast on staircase SD per domain and domain-general
#
# Forest points are coloured by sign in the same gender palette as Figure 4:
# vermillion where the contrast is positive (women higher), blue where negative.
#
# Inputs   revision/data/cells.rds
#          revision/data/effects_rt.csv         (23_reaction_time.R)
#          revision/data/effects_staircase.csv  (24_staircase.R)
# Outputs  revision/figures/FigureS5_rt_staircase.{png,pdf}
#          revision/figures/FigureS5_rt_staircase_caption.txt
#
# Run from anywhere:  Rscript revision/code/figS5_rt_staircase.R

.this <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
ROOT  <- normalizePath(if (length(.this)) file.path(dirname(.this[1]), "..", "..") else ".")
setwd(ROOT)

suppressMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(cowplot); library(patchwork)
})
source("code/theme_high_impact.R")

GENDER_COLS <- c(Men = "#0072B2", Women = "#D55E00")            # Okabe-Ito
SIGN_COLS   <- c(`TRUE` = "#D55E00", `FALSE` = "#0072B2")       # positive = women higher
DOMAIN_LAB  <- c(memory = "Memory", vision = "Vision", GDP = "GDP", Calories = "Calories")
SC_DOMAINS  <- c("vision", "GDP", "Calories")                   # staircased tasks only

cells <- readRDS("revision/data/cells.rds") |>
  mutate(gender2 = factor(ifelse(gender == "Feminin", "Women", "Men"),
                          levels = c("Women", "Men")),
         domain_f = factor(DOMAIN_LAB[as.character(domain)], levels = unname(DOMAIN_LAB)))

mean_ci <- function(x) {
  x <- x[!is.na(x)]
  m <- mean(x); s <- sd(x) / sqrt(length(x))
  c(m = m, lo = m - 1.96 * s, hi = m + 1.96 * s)
}

# ---------------------------------------------------------------- panel A ----
rt_raw <- cells |>
  select(subj, gender2, domain_f, rt_cor, rt_inc) |>
  pivot_longer(c(rt_cor, rt_inc), names_to = "acc", names_prefix = "rt_",
               values_to = "rt") |>
  mutate(acc = factor(acc, levels = c("cor", "inc"),
                      labels = c("Correct trials", "Error trials"))) |>
  group_by(domain_f, gender2, acc) |>
  summarise(as.list(mean_ci(rt)) |> as_tibble(), .groups = "drop")

dodge <- position_dodge(width = 0.45)
pA <- ggplot(rt_raw, aes(x = domain_f, y = m, colour = gender2, group = gender2)) +
  geom_line(position = dodge, linewidth = 1.1, alpha = 0.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 1.4, position = dodge) +
  geom_point(position = dodge, size = 3.2, shape = 21, fill = "white", stroke = 1.4) +
  facet_wrap(~ acc) +
  scale_colour_manual(values = GENDER_COLS, name = NULL) +
  labs(x = NULL, y = "Type-1 RT (s), mean +/- 95% CI", tag = "A",
       title = "Reaction time on correct and error trials",
       subtitle = "Raw participant means; no model, no covariates") +
  theme_high_impact() + theme(legend.position = "top")

# ---------------------------------------------------------------- panel C ----
sc_raw <- cells |>
  filter(as.character(domain) %in% SC_DOMAINS) |>
  mutate(domain_f = droplevels(domain_f)) |>
  group_by(domain_f, gender2) |>
  summarise(as.list(mean_ci(sc_sd_z)) |> as_tibble(), .groups = "drop")

pC <- ggplot(sc_raw, aes(x = domain_f, y = m, colour = gender2, group = gender2)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.6) +
  geom_line(position = dodge, linewidth = 1.1, alpha = 0.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 1.4, position = dodge) +
  geom_point(position = dodge, size = 3.2, shape = 21, fill = "white", stroke = 1.4) +
  scale_colour_manual(values = GENDER_COLS, name = NULL) +
  labs(x = NULL, y = "Staircase SD (z within domain)", tag = "C",
       title = "Trial-to-trial variability of the staircase",
       subtitle = "Memory has no staircase and is not shown") +
  theme_high_impact() + theme(legend.position = "top")

# ------------------------------------------------------- forest panels -------
# Both forests read the effect tables written by scripts 23 and 24; nothing is refitted.
forest <- function(csv, pattern, doms, xlab, title, subtitle, tag) {
  e <- read.csv(csv, stringsAsFactors = FALSE) |>
    filter(grepl(pattern, effect)) |>
    mutate(domain = sub(paste0("^", pattern, ": "), "", effect),
           lab = ifelse(domain == "DOMAIN-GENERAL", "Domain-general",
                        unname(DOMAIN_LAB[domain])),
           lab = factor(lab, levels = rev(c("Domain-general", unname(DOMAIN_LAB[doms])))),
           pos = mean > 0,
           txt = ifelse(BF10 >= 1000 | BF10 < 0.001, sprintf("BF10 = %.1e", BF10),
                        sprintf("BF10 = %.2f", BF10)))
  stopifnot(nrow(e) == length(doms) + 1, !anyNA(e$lab))
  pad <- 0.5 * diff(range(c(e$hdi_lo, e$hdi_hi)))   # right margin for the BF label
  ggplot(e, aes(x = mean, y = lab, colour = pos)) +
    geom_vline(xintercept = 0, linewidth = 0.7, colour = "grey30") +
    geom_errorbar(aes(xmin = hdi_lo, xmax = hdi_hi), orientation = "y",
                  width = 0, linewidth = 1.4) +
    geom_point(size = 3.4, shape = 21, fill = "white", stroke = 1.4) +
    geom_text(aes(x = max(e$hdi_hi) + 0.92 * pad, label = txt), hjust = 1, vjust = -1.1,
              size = 3.1, colour = "grey20", show.legend = FALSE) +
    scale_colour_manual(values = SIGN_COLS, guide = "none") +
    coord_cartesian(xlim = c(min(e$hdi_lo) - 0.06 * pad, max(e$hdi_hi) + pad),
                    clip = "off") +
    labs(x = xlab, y = NULL, tag = tag, title = title, subtitle = subtitle) +
    theme_high_impact() +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())
}

pB <- forest("revision/data/effects_rt.csv", "gender x accuracy \\(error slowing F-M\\)",
             names(DOMAIN_LAB),
             "Women - Men difference in error slowing (s)",
             "Do men slow down less after an error?",
             "Model 23; positive = women slow more on errors. Formula in the caption", "B")

pD <- forest("revision/data/effects_staircase.csv", "gender \\(F-M\\)", SC_DOMAINS,
             "Women - Men, staircase SD (z)",
             "Was the staircase more variable for one gender?",
             "Model 24. Formula in the caption", "D")

# ----------------------------------------------------------------- output ----
fig <- (pA | pB) / (pC | pD)
dir.create("revision/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("revision/figures/FigureS5_rt_staircase.png", fig, dpi = 300,
       width = 15, height = 10, units = "in")
ggsave("revision/figures/FigureS5_rt_staircase.pdf", fig,
       width = 15, height = 10, units = "in")

writeLines(c(
"Figure S5. The two design confounds Reviewer 3 raised, tested directly.",
"",
"(A) Raw mean type-1 reaction time per domain and gender, on correct and on error trials",
"(mean +/- 95% CI over participants; N = 317).",
"",
"(B) Posterior gender x accuracy interaction from model 23 -- the Women - Men difference in",
"error slowing, per domain and domain-general (the draw-wise mean of the four). A positive",
"value means women slow down more after an error than men do. Model:",
"",
"    RT    ~ 0 + domain + domain:gender + domain:accuracy + domain:gender:accuracy",
"            + age + education + (1 | participant),",
"    sigma ~ 0 + domain:accuracy,",
"",
"with gender sum-coded (+0.5 women / -0.5 men) and trial accuracy sum-coded (+0.5 correct /",
"-0.5 incorrect).",
"",
"(C) Raw within-participant SD of the absolute stimulus level, z-scored within domain, per",
"domain and gender. Memory used three fixed blockwise difficulty levels rather than a",
"staircase and is therefore not shown.",
"",
"(D) Posterior gender contrast on that quantity from model 24:",
"",
"    staircase SD (z) ~ 0 + domain + domain:gender + age + education + (1 | participant).",
"",
"Points and intervals are posterior means with 95% highest-density intervals, coloured",
"vermillion where the contrast is positive and blue where it is negative, as in Figure 4.",
"Labels give the Savage-Dickey BF10 against a point null at zero."),
  "revision/figures/FigureS5_rt_staircase_caption.txt")

cat("wrote revision/figures/FigureS5_rt_staircase.{png,pdf} and its caption\n")
