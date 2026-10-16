#!/usr/bin/env Rscript
# 30_manuscript_tables.R
#
# Builds every manuscript and supplementary table as a markdown pipe table.
# Reads only the CSVs (and two .rds sample files) in revision/data/; it fits
# nothing and modifies nothing.  One file per table is written to
# docs/manuscript/tables/.
#
# Conventions used throughout:
#   * every contrast is women minus men
#   * intervals are 95% highest-density intervals, printed "[lo, hi]"
#   * minus signs are U+2212, multiplication signs U+00D7
#   * Bayes factors: >= 1000 as "> 10^k", 0.01-1000 to two significant
#     figures, < 0.01 as "< 0.01", non-finite as "> 10^15"
#   * rounding: 3 dp for log M-ratio and meta-Delta, 0 dp for the
#     percentage-point calibration scale, 1 dp for percentage-point accuracy,
#     2 dp otherwise

LIBS <- c("/mnt/penguino/micah/Rlib_hmetad_dev", "/mnt/penguino/micah/Rlib_meta_intero")
.libPaths(c(LIBS[dir.exists(LIBS)], .libPaths()))

args <- commandArgs(trailingOnly = FALSE)
sf   <- sub("^--file=", "", args[grep("^--file=", args)])
ROOT <- if (length(sf)) normalizePath(file.path(dirname(sf[1]), "..", "..")) else normalizePath(".")
DATA <- file.path(ROOT, "revision", "data")
OUT  <- file.path(ROOT, "docs", "manuscript", "tables")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)


## ---------------------------------------------------------------- formatting

SUPS <- c("⁰", "¹", "²", "³", "⁴",
          "⁵", "⁶", "⁷", "⁸", "⁹")

sup <- function(k) {
  paste(SUPS[as.integer(strsplit(as.character(k), "")[[1]]) + 1L], collapse = "")
}

minus <- function(s) gsub("-", "−", s, fixed = TRUE)

DASH <- "—"   # em dash, used for "not available"

fmt_num <- function(x, dp) {
  if (length(x) != 1L) return(vapply(x, fmt_num, character(1), dp = dp))
  if (is.na(x)) return(DASH)
  s <- formatC(round(x, dp), format = "f", digits = dp)
  if (grepl("^-0(\\.0*)?$", s)) s <- sub("^-", "", s)   # never print "-0"
  minus(s)
}

fmt_hdi <- function(lo, hi, dp) paste0("[", fmt_num(lo, dp), ", ", fmt_num(hi, dp), "]")

fmt_sig2 <- function(x) {
  dp <- max(0, 1 - floor(log10(x)))
  formatC(signif(x, 2), format = "f", digits = dp)
}

# Bayes factors at or beyond moderate evidence either way (BF >= 3 or <= 1/3) are set
# in bold, so a reader can see at a glance which rows carry evidence. The table
# legends (LEGEND below) say so.
fmt_bf <- function(x) {
  if (length(x) != 1L) return(vapply(x, fmt_bf, character(1)))
  if (is.na(x)) return(DASH)
  txt <- if (!is.finite(x)) paste0("> 10", sup(15))
         else if (x >= 1000) paste0("> 10", sup(min(15, floor(log10(x)))))
         else if (x < 0.01) "< 0.01"
         else fmt_sig2(x)
  if (!is.finite(x) || x >= 3 || x <= 1/3) paste0("**", txt, "**") else txt
}

# Shared legend text. Every table with a Bayes factor column appends LEGEND_BF; the
# units and the ROPE width are table-specific and come first.
LEGEND_BF <- paste(
  "Contrasts are women minus men, given as the posterior mean with its 95% highest-density",
  "interval (HDI). BF10 is the Savage–Dickey Bayes factor against a point null; Bayes factors",
  "in bold mark at least moderate evidence (BF10 above 3 for a difference, below 1/3 for no",
  "difference). % in ROPE is the share of the posterior inside the region of practical",
  "equivalence.")
ROPE_BEH <- "The ROPE is ±0.1 SD of the outcome."
ROPE_LOGM <- "The ROPE for log M-ratio is ±0.05 log units, about 5% of M-ratio."

fmt_bf01 <- function(bf10) {
  if (length(bf10) != 1L) return(vapply(bf10, fmt_bf01, character(1)))
  if (is.na(bf10)) return(DASH)
  if (!is.finite(bf10)) return("< 0.01")
  if (bf10 == 0) return(paste0("> 10", sup(15)))
  fmt_bf(1 / bf10)
}

fmt_pct <- function(x) {
  if (length(x) != 1L) return(vapply(x, fmt_pct, character(1)))
  if (is.na(x)) return(DASH)
  formatC(round(x, 1), format = "f", digits = 1)
}


## -------------------------------------------------------------- table output

# `widths`, when given, are relative column widths. pandoc then wraps the cells and
# sizes the columns by the dashes in the separator line, provided one line of the
# table exceeds the --columns setting of the build (250); the separator is scaled
# past that on purpose. Used for the two tables whose cells are sentences (T4, S12).
pipe_table <- function(df, widths = NULL) {
  cells <- as.matrix(df)
  cells[is.na(cells)] <- DASH
  sep <- if (is.null(widths)) rep("---", ncol(df)) else {
    stopifnot(length(widths) == ncol(df))
    w <- ceiling(widths / sum(widths) * 300)
    vapply(w, function(k) strrep("-", max(3, k)), character(1))
  }
  c(paste0("| ", paste(colnames(df), collapse = " | "), " |"),
    paste0("|", paste(sep, collapse = "|"), "|"),
    apply(cells, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}

# `blocks` is either a data frame or a list of list(head = <string or NULL>, df = <df>)
write_tbl <- function(file, caption, blocks, widths = NULL) {
  if (is.data.frame(blocks)) blocks <- list(list(head = NULL, df = blocks))
  # "Table N. Title." goes above the table; whatever follows is the legend and goes
  # below it, as a note under the table.
  m <- regmatches(caption, regexec("^(Table S?[0-9]+\\. [^.]*\\.)\\s*(.*)$", caption))[[1]]
  title  <- if (length(m)) m[2] else caption
  legend <- if (length(m)) trimws(m[3]) else ""
  # APA layout: bold number, italic title above the table; "Note." before the legend.
  num   <- sub("^(Table S?[0-9]+\\.).*$", "\\1", title)
  ttl   <- trimws(sub("^Table S?[0-9]+\\.\\s*", "", title))
  lines <- c("```{=latex}", "\\needspace{10\\baselineskip}", "```", "",
             paste0("**", num, "** *", ttl, "*"), "")
  for (b in blocks) {
    if (!is.null(b$head)) lines <- c(lines, b$head, "")
    lines <- c(lines, pipe_table(b$df, widths), "")
  }
  # The legend sits in a `legend` div: invisible in the PDF/Word builds, and what
  # preprint.lua uses to carry it into the table float.
  if (nzchar(legend)) lines <- c(lines, "::: legend", paste0("*Note.* ", legend), ":::", "")
  con <- file(file.path(OUT, file), open = "wt", encoding = "UTF-8")
  writeLines(lines, con)
  close(con)
  message("wrote ", file)
}


## ------------------------------------------------------------- data + labels

rd <- function(f) read.csv(file.path(DATA, f), check.names = FALSE)

pick <- function(d, eff) {
  r <- d[d$effect == eff, , drop = FALSE]
  if (nrow(r) != 1L) stop(sprintf("no unique row for effect '%s'", eff))
  r[1, ]
}

DOMS4 <- c("memory", "vision", "GDP", "Calories", "DOMAIN-GENERAL")
DOMS2 <- c("GDP", "Calories", "DOMAIN-GENERAL")

DOM_LAB <- c(memory = "Memory", vision = "Vision", GDP = "GDP",
             Calories = "Calories", `DOMAIN-GENERAL` = "Domain-general",
             `domain-general` = "Domain-general")

dlab <- function(x) {
  out <- unname(DOM_LAB[x])
  out[is.na(out)] <- pair_lab(x[is.na(out)])
  out
}

pair_lab <- function(x) {
  x <- gsub("memory", "Memory", x)
  x <- gsub("vision", "Vision", x)
  gsub(" - ", " − ", x, fixed = TRUE)
}

eff_lab <- function(x) {
  sub_all <- list(
    c("gender \\(F-M\\)",                          "Gender (women − men)"),
    c("gender x accuracy \\(error slowing F-M\\)", "Gender × accuracy (error slowing)"),
    c("gender x accuracy",                         "Gender × accuracy"),
    c("gender x time",                             "Gender × time"),
    c("gender x domain",                           "Gender × domain"),
    c("gender on CORRECT trials",                  "Gender, correct trials"),
    c("gender on INCORRECT trials",                "Gender, incorrect trials"),
    c("accuracy \\(cor-inc\\)",                    "Accuracy (correct − incorrect)"),
    c("error slowing \\(inc-cor\\)",               "Error slowing (incorrect − correct)"),
    c("time \\(post-pre\\)",                       "Time (post − pre)"),
    c("coupling to calibration \\(pre\\)",         "Coupling to baseline calibration"),
    c("coupling x gender \\(calibration\\)",       "Coupling × gender (calibration)"),
    c("coupling to pre->post update",              "Coupling to pre→post update"),
    c("coupling x gender \\(update\\)",            "Coupling × gender (update)"),
    c("^covariate: ",                              "Covariate: "))
  for (s in sub_all) x <- gsub(s[1], s[2], x)
  x
}

# "gender x domain: memory - GDP" -> label "gender x domain", domain "memory - GDP"
split_eff <- function(x) {
  i <- regexpr(": [^:]*$", x)
  list(label  = ifelse(i > 0, substr(x, 1, i - 1), x),
       domain = ifelse(i > 0, substring(x, i + 2), ""))
}

# one formatted row from a behavioural effects data frame
erow <- function(r, dp, bf_col = "BF10", rope_col = "pct_rope") {
  data.frame(Estimate    = fmt_num(r$mean, dp),
             `95% HDI`   = fmt_hdi(r$hdi_lo, r$hdi_hi, dp),
             BF10        = fmt_bf(r[[bf_col]]),
             `% in ROPE` = fmt_pct(if (rope_col %in% names(r)) r[[rope_col]] else NA),
             check.names = FALSE)
}

SAMPLE <- c(`first edition` = "Discovery", `VMP replication` = "VMP replication")


## ------------------------------------------------------------------ Table 1

tbl_T1 <- function() {
  cal <- subset(rd("effects_calibration.csv"), model == "calibration")

  # global calibration is the discovery sample only, so the contrast leads
  blk <- function(contrast, prefix, doms, dp) {
    do.call(rbind, lapply(doms, function(k) {
      r <- pick(cal, paste0(prefix, k))
      cbind(data.frame(Contrast = contrast, Domain = dlab(k)), erow(r, dp))
    }))
  }

  df <- rbind(
    blk("Gender",         "gender (F-M): ", DOMS4, 0),
    blk("Gender × time", "gender x time: ", DOMS4, 0))

  write_tbl("T1.md", paste(
    "Table 1. Gender contrasts on global calibration. Calibration is the self-estimated minus",
    "the actual per cent correct, in percentage points, in the discovery sample (N = 317).",
    "The gender × time contrast is the post-minus-pre change in the gender difference; a",
    "negative value means women revised their self-estimate further downward than men.",
    "Domain-general rows are the mean of the per-domain effects.", LEGEND_BF, ROPE_BEH), df)
}


## ------------------------------------------------------------------ Table 2

tbl_T2 <- function() {
  cnf <- subset(rd("effects_confidence.csv"),     model == "confidence")
  vmp <- subset(rd("effects_confidence_vmp.csv"), model == "confidence_vmp")

  blk <- function(d, sample, doms) {
    do.call(rbind, lapply(doms, function(k) {
      r <- pick(d, paste0("gender (F-M): ", k))
      cbind(data.frame(Sample = sample, Domain = dlab(k)), erow(r, 2))
    }))
  }

  df <- rbind(blk(cnf, "Discovery",        DOMS4),
              blk(vmp, "VMP replication", DOMS2))

  write_tbl("T2.md", paste(
    "Table 2. Gender contrasts on local confidence. Mean confidence on the seven-point scale,",
    "in scale points, with each participant's task accuracy controlled, in the discovery",
    "sample (N = 317, four domains) and the replication sample (N = 457, two domains).",
    "Domain-general rows are the mean of the per-domain effects.", LEGEND_BF, ROPE_BEH), df)
}


## ------------------------------------------------------------------ Table 3

# BF10 for the meta-Delta domain-general contrast lives in prior_sensitivity.csv
# (the prior SD 0.5 row is the reported fit). Table 3 takes every meta-Delta
# BF10, per domain and domain-general, from labels_<tag>.csv (07_contrast_draws.R),
# the same values Figure 4 shows.
metadelta_bf <- function(sample_key) {
  ps <- rd("prior_sensitivity.csv")
  r  <- subset(ps, sample == sample_key & prior_sd == 0.5 &
                   quantity == "meta-Delta" & effect == "domain-general")
  if (nrow(r) == 1L) r$bf10 else NA_real_
}

metadelta_contrasts <- function(file, doms) {
  d <- rd(file)
  d <- subset(d, quantity == "contrast_Feminin_minus_Masculin" & response == "both")
  d[match(doms, d$mod), ]
}

metadelta_bf_by_domain <- function(tag, doms) {
  d <- subset(rd(sprintf("labels_%s.csv", tag)), quantity == "metadelta")
  d$bf10[match(doms, d$domain)]
}

logm_gender <- function(file, doms) {
  d <- subset(rd(file), param == "log M-ratio")
  d[match(doms, d$effect), ]
}

tbl_T3 <- function() {
  spec <- list(
    list(sample = "Discovery",        gfile = "effects_fe_fixed_gender.csv",
         mfile = "effects_fe_fixed_metadelta.csv", tag = "fe_fixed",
         gd = c("memory", "vision", "GDP", "Calories", "domain-general"),
         md = c("memory", "vision", "GDP", "Calories", "domain-general")),
    list(sample = "VMP replication", gfile = "effects_vmp_fixed_gender.csv",
         mfile = "effects_vmp_fixed_metadelta.csv", tag = "vmp_fixed",
         gd = c("GDP", "Calories", "domain-general"),
         md = c("GDP", "Calories", "domain-general")))

  logm <- do.call(rbind, lapply(spec, function(s) {
    g <- logm_gender(s$gfile, s$gd)
    data.frame(Parameter = "log M-ratio", Sample = s$sample, Domain = dlab(g$effect),
               Estimate = fmt_num(g$mean, 3),
               `95% HDI` = fmt_hdi(g$hdi_lo, g$hdi_hi, 3),
               BF10 = fmt_bf(g$bf10), `% in ROPE` = fmt_pct(g$pct_in_rope),
               check.names = FALSE)
  }))

  meta <- do.call(rbind, lapply(spec, function(s) {
    m  <- metadelta_contrasts(s$mfile, s$md)
    bf <- metadelta_bf_by_domain(s$tag, s$md)
    data.frame(Parameter = "meta-Δ", Sample = s$sample, Domain = dlab(m$mod),
               Estimate = fmt_num(m$mean, 3),
               `95% HDI` = fmt_hdi(m$hdi_lo, m$hdi_hi, 3),
               BF10 = fmt_bf(bf), `% in ROPE` = DASH, check.names = FALSE)
  }))

  write_tbl("T3.md", paste(
    "Table 3. Gender contrasts from the unified hierarchical meta-d′ model. Log M-ratio",
    "(metacognitive efficiency) is in log units; meta-Δ (metacognitive bias) is the distance",
    "between the average confidence criterion and the metacognitive decision criterion on the",
    "internal evidence axis, averaged over response sides, where a positive contrast means",
    "women place their confidence criteria more conservatively. Discovery sample N = 318,",
    "replication sample N = 456. Domain-general rows are the mean of the per-domain effects.",
    LEGEND_BF, ROPE_LOGM), rbind(logm, meta))
}


## ----------------------------------------------------------------- Table S12

tbl_S12 <- function() {
  # Table S12 lists every claim of no difference in the paper with its evidence and a
  # verdict. (A main-text Table 4 with the headline rows existed until 2026-10-02; the
  # Bayes factors are all in the Results text, which is what the editor asked for, so
  # it was dropped. `plain` and `main` are kept on the rows in case it comes back.)
  rows <- list()
  verdict <- function(bf10, lo, hi, rope) {
    if (is.na(bf10))
      return(if (lo <= 0 && hi >= 0) "HDI includes zero" else "HDI excludes zero")
    if (bf10 <= 1/3) {
      if (!is.na(rope) && rope < 80) "evidence against a point difference; a small one is not excluded"
      else "evidence for no difference"
    } else if (bf10 >= 3) "evidence for a difference" else "inconclusive"
  }
  add <- function(claim, sample, r, dp, bf_col = "BF10", rope_col = "pct_rope",
                  plain = NULL, main = FALSE) {
    bf   <- if (bf_col %in% names(r)) r[[bf_col]] else NA_real_
    rope <- if (!is.null(rope_col) && rope_col %in% names(r)) r[[rope_col]] else NA_real_
    rows[[length(rows) + 1L]] <<- data.frame(
      Claim = claim, Plain = if (is.null(plain)) claim else plain, Sample = sample,
      Estimate = fmt_num(r$mean, dp),
      `95% HDI` = fmt_hdi(r$hdi_lo, r$hdi_hi, dp),
      BF01 = fmt_bf01(bf), `% in ROPE` = fmt_pct(rope),
      Verdict = verdict(bf, r$hdi_lo, r$hdi_hi, rope), main = main, check.names = FALSE)
  }

  pc <- subset(rd("effects_positive_controls.csv"), model == "accuracy (percentage points)")
  add("Accuracy, gender: domain-general", "Discovery", pick(pc, "gender (F-M): DOMAIN-GENERAL"), 1,
      plain = "Women and men were equally accurate (domain-general, percentage points)", main = TRUE)
  for (k in c("memory", "vision", "GDP", "Calories"))
    add(paste0("Accuracy, gender: ", dlab(k)), "Discovery", pick(pc, paste0("gender (F-M): ", k)), 1)

  for (s in list(c("effects_fe_fixed_gender.csv", "Discovery"),
                 c("effects_vmp_fixed_gender.csv", "VMP replication"))) {
    d <- subset(rd(s[1]), param == "d'")
    add("d′, gender: domain-general", s[2], pick(d, "domain-general"), 3,
        bf_col = "bf10", rope_col = "pct_in_rope",
        plain = "Type-1 sensitivity d′ did not differ by gender (domain-general)", main = TRUE)
  }

  # mean RT comes from the task-performance model, as in the Results text; the
  # accuracy-split RT model supplies only the error-slowing row
  prt <- subset(rd("effects_positive_controls.csv"), model == "mean RT (seconds)")
  add("Mean RT, gender: domain-general", "Discovery",
      pick(prt, "gender (F-M): DOMAIN-GENERAL"), 2)
  rt <- subset(rd("effects_rt.csv"), model == "rt")
  add("RT error slowing, gender × accuracy: domain-general", "Discovery",
      pick(rt, "gender x accuracy (error slowing F-M): DOMAIN-GENERAL"), 2)

  st <- subset(rd("effects_staircase.csv"), model == "staircase_sd")
  add("Staircase SD, gender: domain-general", "Discovery",
      pick(st, "gender (F-M): DOMAIN-GENERAL"), 2)

  cnf <- subset(rd("effects_confidence.csv"), model == "confidence")
  vmp <- subset(rd("effects_confidence_vmp.csv"), model == "confidence_vmp")
  err <- "Women did not lower their confidence selectively after errors (gender × accuracy, scale points)"
  add("Confidence, gender × accuracy: domain-general", "Discovery",
      pick(cnf, "gender x accuracy: DOMAIN-GENERAL"), 2, plain = err, main = TRUE)
  add("Confidence, gender × accuracy: domain-general", "VMP replication",
      pick(vmp, "gender x accuracy: DOMAIN-GENERAL"), 2, plain = err, main = TRUE)

  gv <- subset(rd("effects_vmp_fixed_gender.csv"), param == "log M-ratio")
  add("log M-ratio, gender: domain-general", "VMP replication",
      pick(gv, "domain-general"), 3, bf_col = "bf10", rope_col = "pct_in_rope",
      plain = "Metacognitive efficiency did not differ by gender (domain-general log M-ratio)", main = TRUE)

  # largest gender x domain pairwise difference on log M-ratio in each sample
  for (s in list(c("effects_fe_fixed_gender.csv", "Discovery"),
                 c("effects_vmp_fixed_gender.csv", "VMP replication"))) {
    g <- subset(rd(s[1]), param == "log M-ratio" & grepl("^gender x domain: ", effect))
    r <- g[which.max(abs(g$mean)), ]
    r$bf10 <- NA_real_
    add(paste0("log M-ratio, largest gender × domain difference (",
               pair_lab(sub("^gender x domain: ", "", r$effect)), ")"),
        s[2], r, 3, bf_col = "bf10", rope_col = "pct_in_rope")
  }

  lg <- rd("effects_local_global.csv")
  add("Local–global coupling × gender, calibration (log M-ratio)", "Discovery",
      pick(subset(lg, model == "logM"), "coupling x gender (calibration): DOMAIN-GENERAL"), 3)
  add("Local–global coupling × gender, update (log M-ratio)", "Discovery",
      pick(subset(lg, model == "logM"), "coupling x gender (update): DOMAIN-GENERAL"), 3)
  add("Local–global coupling × gender, calibration (meta-Δ)", "Discovery",
      pick(subset(lg, model == "metadelta"), "coupling x gender (calibration): DOMAIN-GENERAL"), 3)
  add("Local–global coupling × gender, update (meta-Δ)", "Discovery",
      pick(subset(lg, model == "metadelta"), "coupling x gender (update): DOMAIN-GENERAL"), 3)

  all <- do.call(rbind, rows)
  supp <- all[, c("Claim", "Sample", "Estimate", "95% HDI", "BF01", "% in ROPE", "Verdict")]
  write_tbl("S12.md", paste(
    "Table S12. Every claim of no difference in the paper, with its evidence. Contrasts are",
    "women minus men, posterior mean and 95% HDI. BF01 is the Savage–Dickey Bayes factor for",
    "the point null, in bold at or beyond moderate evidence; % in ROPE is the share of the",
    "posterior inside the region of practical equivalence (±0.1 SD of the outcome for the",
    "behavioural quantities, ±0.05 log units for log M-ratio). The verdict applies the",
    "thresholds given in Methods (BF01 above 3, evidence for no difference; below 1/3,",
    "evidence for a difference; otherwise inconclusive). Estimates are in percentage points (accuracy), seconds",
    "(reaction time), within-domain SD units (staircase variability), scale points",
    "(confidence), log units (log M-ratio) and evidence units (meta-Δ); pairwise gender ×",
    "domain differences have no point-null Bayes factor and are judged by their HDI."), supp,
    widths = c(40, 11, 8, 14, 6, 8, 26))
}


## ----------------------------------------------------------------- Table S2

tbl_S2 <- function() {
  qc  <- rd("cell_qc.csv");     qcv <- rd("vmp_cell_qc.csv")
  sub <- readRDS(file.path(DATA, "subjects.rds"))
  cel <- readRDS(file.path(DATA, "cells.rds"))
  cvm <- readRDS(file.path(DATA, "cells_vmp.rds"))

  n_by <- function(g, lev) sum(g == lev)
  age_str <- function(a) sprintf("%.1f (%.1f)", mean(a), sd(a))

  # one sample's block of rows
  block <- function(sample, beh_subj, beh_gender, beh_age, qcd) {
    keep  <- qcd[!qcd$drop, ]
    msubj <- unique(keep[, c("subj", "gender")])
    trial <- function(g) {
      x <- keep$n_trials[keep$gender == g]
      sprintf("%d (%d–%d)", as.integer(median(x)), min(x), max(x))
    }
    ndrop <- function(g, reason) sum(qcd$drop & qcd$gender == g & qcd$drop_reason == reason)
    r <- function(q, w, m, t) data.frame(Sample = sample, Quantity = q,
                                         Women = w, Men = m, Total = t, check.names = FALSE)
    rbind(
      r("N, behavioural analyses",
        n_by(beh_gender, "Feminin"), n_by(beh_gender, "Masculin"), length(beh_subj)),
      r("N, unified meta-d′ model",
        n_by(msubj$gender, "Feminin"), n_by(msubj$gender, "Masculin"), nrow(msubj)),
      r("Age, mean (SD)",
        age_str(beh_age[beh_gender == "Feminin"]),
        age_str(beh_age[beh_gender == "Masculin"]),
        age_str(beh_age)),
      r("Cells entering the meta-d′ model",
        sum(keep$gender == "Feminin"), sum(keep$gender == "Masculin"), nrow(keep)),
      r("Trials per cell, median (range)",
        trial("Feminin"), trial("Masculin"),
        sprintf("%d (%d–%d)", as.integer(median(keep$n_trials)),
                min(keep$n_trials), max(keep$n_trials))),
      r("Cells dropped, low d′",
        ndrop("Feminin", "dprime"), ndrop("Masculin", "dprime"),
        sum(qcd$drop & qcd$drop_reason == "dprime")),
      r("Cells dropped, too few trials",
        ndrop("Feminin", "thin"), ndrop("Masculin", "thin"),
        sum(qcd$drop & qcd$drop_reason == "thin")))
  }

  vsub <- unique(cvm[, c("subj", "gender", "age")])
  df <- rbind(block("Discovery", sub$subj, sub$gender, sub$age, qc),
              block("VMP replication", vsub$subj, vsub$gender, vsub$age, qcv))
  df[] <- lapply(df, as.character)

  write_tbl("S2.md", "Table S2. Samples and cell-level quality control.", df)
}


## ----------------------------------------------------------------- Table S3

tbl_S3 <- function() {
  p <- function(model, class, prior) data.frame(Model = model,
                                                `Parameter class` = class,
                                                Prior = prior, check.names = FALSE)
  H <- "Unified meta-d′ model"
  B <- "Behavioural models"
  df <- rbind(
    p(H, "Domain cell means, log M-ratio",      "normal(0, 0.5)"),
    p(H, "Domain cell means, d′",          "normal(1, 0.5)"),
    p(H, "Domain cell means, c",                "normal(0, 0.5)"),
    p(H, "Type-2 criteria",                     "normal(−0.5, 1)"),
    p(H, "Gender contrasts (all dpars)",        "normal(0, 0.5); sensitivity 0.2 and 1.0"),
    p(H, "Random-effect SDs",                   "normal(0, 1)"),
    p(H, "Random-effect correlations",          "lkj(2)"),
    p(H, "Sampler",                             "4 chains × 2000 iterations, 1000 warmup, adapt_delta 0.95, max_treedepth 12"),
    p(H, "ROPE, log M-ratio",                   "±0.05"),
    p(B, "Cell means and standardised covariates", "normal(0, SD(DV))"),
    p(B, "Contrast coefficients",               "normal(0, 0.5 × SD(DV)); sensitivity k = 0.25 and 1"),
    p(B, "Random-effect SDs",                   "normal(0, SD(DV))"),
    p(B, "Residual SD (log, per cell)",         "normal(log SD(DV), 1)"),
    p(B, "ROPE",                                "±0.1 × SD(DV)"))
  write_tbl("S3.md",
            "Table S3. Priors of the unified model and the behavioural models.", df)
}


## ----------------------------------------------------------------- Table S4

tbl_S4 <- function() {
  want <- c("gender x accuracy", "gender on CORRECT trials",
            "gender on INCORRECT trials", "gender x domain")
  blk <- function(file, model_name, sample) {
    d <- rd(file)
    d <- d[d$model == model_name, , drop = FALSE]
    s <- split_eff(d$effect)
    d <- d[s$label %in% want, ]
    s <- split_eff(d$effect)
    ord <- order(match(s$label, want))
    d <- d[ord, ]; s <- split_eff(d$effect)
    data.frame(Sample = sample, Effect = eff_lab(s$label), Domain = dlab(s$domain),
               Estimate = fmt_num(d$mean, 2),
               `95% HDI` = fmt_hdi(d$hdi_lo, d$hdi_hi, 2),
               BF10 = fmt_bf(d$BF10), `% in ROPE` = fmt_pct(d$pct_rope),
               check.names = FALSE)
  }
  df <- rbind(blk("effects_confidence.csv", "confidence", "Discovery"),
              blk("effects_confidence_vmp.csv", "confidence_vmp", "VMP replication"))
  write_tbl("S4.md",
            paste0("Table S4. Local confidence: per-domain gender × accuracy interactions, ",
                   "simple effects on correct and incorrect trials, and pairwise domain ",
                   "differences of the gender effect, in both samples. Estimates are in",
                   " scale points on the seven-point confidence scale, with task accuracy",
                   " controlled. ", LEGEND_BF, " ", ROPE_BEH), df)
}


## ----------------------------------------------------------------- Table S5

tbl_S5 <- function() {
  blk <- function(dom_file, gen_file, sample) {
    dm <- rd(dom_file)
    a <- data.frame(Sample = sample, Parameter = "log M-ratio",
                    Effect = ifelse(grepl("^cell mean: ", dm$effect),
                                    paste0("Domain mean: ",
                                           dlab(sub("^cell mean: ", "", dm$effect))),
                                    paste0("Domain difference: ", pair_lab(dm$effect))),
                    Estimate = fmt_num(dm$mean, 3),
                    `95% HDI` = fmt_hdi(dm$hdi_lo, dm$hdi_hi, 3),
                    BF10 = DASH, `% in ROPE` = DASH, check.names = FALSE)

    gn <- rd(gen_file)
    sdt <- subset(gn, param %in% c("d'", "c") & !grepl("^gender x domain: ", effect))
    b <- data.frame(Sample = sample,
                    Parameter = ifelse(sdt$param == "d'", "d′", "c"),
                    Effect = paste0("Gender: ", dlab(sdt$effect)),
                    Estimate = fmt_num(sdt$mean, 3),
                    `95% HDI` = fmt_hdi(sdt$hdi_lo, sdt$hdi_hi, 3),
                    BF10 = fmt_bf(sdt$bf10), `% in ROPE` = fmt_pct(sdt$pct_in_rope),
                    check.names = FALSE)

    gx <- subset(gn, param == "log M-ratio" & grepl("^gender x domain: ", effect))
    d <- data.frame(Sample = sample, Parameter = "log M-ratio",
                    Effect = paste0("Gender × domain: ",
                                    pair_lab(sub("^gender x domain: ", "", gx$effect))),
                    Estimate = fmt_num(gx$mean, 3),
                    `95% HDI` = fmt_hdi(gx$hdi_lo, gx$hdi_hi, 3),
                    BF10 = fmt_bf(gx$bf10), `% in ROPE` = fmt_pct(gx$pct_in_rope),
                    check.names = FALSE)
    rbind(a, b, d)
  }
  df <- rbind(blk("effects_fe_fixed_domain.csv", "effects_fe_fixed_gender.csv", "Discovery"),
              blk("effects_vmp_fixed_domain.csv", "effects_vmp_fixed_gender.csv", "VMP replication"))
  write_tbl("S5.md",
            paste0("Table S5. Unified model: domain means of log M-ratio and their pairwise ",
                   "differences; gender contrasts on d′ and c; pairwise gender × domain ",
                   "differences on log M-ratio, in both samples. Log M-ratio is in log",
                   " units; d′ and c are in units of the type-1 evidence axis. Domain means",
                   " are not contrasts and have no Bayes factor. ", LEGEND_BF, " ", ROPE_LOGM), df)
}


## ----------------------------------------------------------------- Table S6

tbl_S6 <- function() {
  blk <- function(file, sample) {
    d <- rd(file)
    pair <- gsub("^cor_subj__mod", "", d$effect)
    pair <- gsub("__mod", " − ", pair)
    data.frame(Sample = sample, `Domain pair` = pair_lab(pair),
               Estimate = fmt_num(d$mean, 2),
               `95% HDI` = fmt_hdi(d$hdi_lo, d$hdi_hi, 2), check.names = FALSE)
  }
  df <- rbind(blk("effects_fe_fixed_rho.csv", "Discovery"),
              blk("effects_vmp_fixed_rho.csv", "VMP replication"))
  write_tbl("S6.md",
            paste0("Table S6. Between-domain correlations of log M-ratio ",
                   "(participant-level random effects), in both samples. Entries are",
                   " posterior means with 95% highest-density intervals of the correlation",
                   " between a participant's log M-ratio deviations in two domains; a",
                   " correlation whose interval includes zero gives no evidence of shared",
                   " individual differences between the two domains."), df)
}


## ----------------------------------------------------------------- Table S7

BEH_MODEL_LAB <- c(calibration = "Global calibration", confidence = "Local confidence",
                   confidence_vmp = "Local confidence (VMP)",
                   logM = "Local–global link, log M-ratio",
                   metadelta = "Local–global link, meta-Δ",
                   `accuracy (percentage points)` = "Accuracy (percentage points)",
                   `mean RT (seconds)` = "Mean RT (seconds)",
                   rt = "Reaction time", staircase_sd = "Staircase SD")

BEH_MODEL_DP <- c(calibration = 0, confidence = 2, confidence_vmp = 2,
                  logM = 3, metadelta = 3, `accuracy (percentage points)` = 1,
                  `mean RT (seconds)` = 2, rt = 2, staircase_sd = 2)

tbl_S7 <- function() {
  ps <- rd("prior_sensitivity.csv")
  ps <- ps[ps$effect == "domain-general", ]
  ps$Sample <- unname(SAMPLE[ps$sample])
  ps$quantity <- ifelse(ps$quantity == "meta-Delta", "meta-Δ", ps$quantity)
  ps <- ps[order(match(ps$Sample, c("Discovery", "VMP replication")),
                 match(ps$quantity, c("log M-ratio", "meta-Δ")),
                 ps$prior_sd), ]
  A <- data.frame(Sample = ps$Sample, Parameter = ps$quantity, Domain = dlab(ps$effect),
                  `Prior SD` = formatC(ps$prior_sd, format = "f", digits = 1),
                  Estimate = fmt_num(ps$mean, 3),
                  `95% HDI` = fmt_hdi(ps$hdi_lo, ps$hdi_hi, 3),
                  BF10 = fmt_bf(ps$bf10), `% in ROPE` = fmt_pct(ps$pct_in_rope),
                  check.names = FALSE)

  pb <- rd("prior_sensitivity_behavioural.csv")
  pb <- pb[split_eff(pb$effect)$domain == "DOMAIN-GENERAL", ]
  # The gender contrasts of the two local-global link models are dropped as
  # well: they come from the descriptive regression on shrunk participant-level
  # estimates, and Table 3 carries the gender contrasts of record.
  pb <- pb[!(pb$model %in% c("logM", "metadelta") &
             grepl("^gender \\(F-M\\)", pb$effect)), ]
  pb <- pb[order(match(pb$model, names(BEH_MODEL_LAB)), pb$effect, pb$k), ]
  s  <- split_eff(pb$effect)
  dp <- unname(BEH_MODEL_DP[pb$model])
  B <- data.frame(Model = unname(BEH_MODEL_LAB[pb$model]),
                  Effect = paste0(eff_lab(s$label), ": ", dlab(s$domain)),
                  k = formatC(pb$k, format = "f", digits = 2),
                  Estimate = mapply(fmt_num, pb$mean, dp),
                  `95% HDI` = mapply(fmt_hdi, pb$hdi_lo, pb$hdi_hi, dp),
                  BF10 = fmt_bf(pb$BF10), `% in ROPE` = fmt_pct(pb$pct_rope),
                  check.names = FALSE)

  write_tbl("S7.md",
            paste0("Table S7. Prior sensitivity, domain-general estimates. ",
                   "Per-domain estimates for every model and prior width are in ",
                   "prior_sensitivity.csv and prior_sensitivity_behavioural.csv, ",
                   "under revision/data/ in the code repository. ", LEGEND_BF),
            list(list(head = "A. Unified meta-d′ model: contrast prior SD 0.2, 0.5 and 1.0.",
                      df = A),
                 list(head = "B. Behavioural models: contrast prior scaled by k = 0.25, 0.5 and 1.",
                      df = B)))
}


## ----------------------------------------------------------------- Table S8

tbl_S8 <- function() {
  d <- rd("effects_local_global.csv")
  d <- d[!grepl("^covariate: ", d$effect), ]
  # Gender main effects are dropped: they are contrasts of this descriptive
  # regression on shrunk participant-level estimates, which Table 3 already
  # reports from the unified model itself.
  d <- d[!grepl("^gender \\(F-M\\)", d$effect), ]
  d <- d[order(match(d$model, c("logM", "metadelta"))), ]
  s <- split_eff(d$effect)
  df <- data.frame(Outcome = ifelse(d$model == "logM", "log M-ratio", "meta-Δ"),
                   Effect = eff_lab(s$label), Domain = dlab(s$domain),
                   Estimate = fmt_num(d$mean, 3),
                   `95% HDI` = fmt_hdi(d$hdi_lo, d$hdi_hi, 3),
                   BF10 = fmt_bf(d$BF10), `% in ROPE` = fmt_pct(d$pct_rope),
                   check.names = FALSE)
  write_tbl("S8.md",
            paste0("Table S8. Local–global link. Outcomes are participant-level ",
                   "posterior means from the unified hierarchical model, regressed on ",
                   "within-domain z-scored baseline calibration and pre→post update, ",
                   "with gender as a moderator. Slopes are the change in the outcome per",
                   " SD of the predictor; the gender × coupling terms test whether the slope",
                   " differs between women and men. ", LEGEND_BF, " ", ROPE_BEH),
            df)
}


## ----------------------------------------------------------------- Table S9

FIT_LAB <- c(fe_fixed = "Discovery, main fit", fe_relative = "Discovery, relative criterion",
             vmp_fixed = "VMP replication, main fit",
             vmp_relative = "VMP replication, relative criterion")

DPAR_LAB <- function(dpar) {
  ifelse(dpar == "N", "log M-ratio",
  ifelse(dpar == "dprime", "d′",
  ifelse(dpar == "c", "c",
  ifelse(grepl("^metac2", dpar), "Type-2 criteria", "Between-domain correlation"))))
}

tbl_S9 <- function() {
  cv <- rd("convergence_metad.csv")
  # the correlation row is the between-domain correlations of log M only; the
  # random-effect SDs and the other dpars' correlations are not reported
  cv <- subset(cv, !is.na(dpar) | block == "between-domain correlation (log M)")
  cv$grp <- DPAR_LAB(ifelse(is.na(cv$dpar), "", cv$dpar))
  ord <- c("log M-ratio", "d′", "c", "Type-2 criteria", "Between-domain correlation")
  key <- interaction(cv$tag, cv$grp, drop = TRUE, sep = "|")
  A <- do.call(rbind, lapply(split(cv, key), function(g) {
    data.frame(Fit = unname(FIT_LAB[g$tag[1]]), `Parameter block` = g$grp[1],
               `Max R-hat` = formatC(max(g$rhat), format = "f", digits = 3),
               `Min bulk ESS` = as.character(round(min(g$ess_bulk))),
               `Min tail ESS` = as.character(round(min(g$ess_tail))),
               check.names = FALSE)
  }))
  A <- A[order(match(A$Fit, FIT_LAB), match(A$`Parameter block`, ord)), ]

  ppc <- function(file, sample) {
    p <- rd(file)
    data.frame(Sample = sample,
               Gender = ifelse(p$gender == "Feminin", "Women", "Men"),
               Domain = dlab(p$mod),
               `Accuracy observed` = fmt_num(p$acc_obs, 3),
               `Accuracy predicted [95% CrI]` =
                 paste0(fmt_num(p$acc_pred, 3), " ", fmt_hdi(p$acc_lo, p$acc_hi, 3)),
               `Confidence observed` = fmt_num(p$conf_obs, 2),
               `Confidence predicted [95% CrI]` =
                 paste0(fmt_num(p$conf_pred, 2), " ", fmt_hdi(p$conf_lo, p$conf_hi, 2)),
               check.names = FALSE)
  }
  B <- rbind(ppc("ppc_summary_fe_fixed.csv", "Discovery"),
             ppc("ppc_summary_vmp_fixed.csv", "VMP replication"))

  write_tbl("S9.md", "Table S9. Convergence and posterior predictive checks.",
            list(list(head = "A. Convergence of the reported coefficients, by parameter block.",
                      df = A),
                 list(head = "B. Posterior predictive check, by gender and domain.", df = B)))
}


## ---------------------------------------------------------------- Table S10

tbl_S10 <- function() {
  spec <- list(
    list(sample = "Discovery", g = "effects_fe_relative_gender.csv",
         m = "effects_fe_relative_metadelta.csv",
         d = c("memory", "vision", "GDP", "Calories", "domain-general")),
    list(sample = "VMP replication", g = "effects_vmp_relative_gender.csv",
         m = "effects_vmp_relative_metadelta.csv",
         d = c("GDP", "Calories", "domain-general")))
  df <- do.call(rbind, lapply(spec, function(s) {
    gg <- logm_gender(s$g, s$d)
    a <- data.frame(Parameter = "log M-ratio", Sample = s$sample, Domain = dlab(gg$effect),
                    Estimate = fmt_num(gg$mean, 3),
                    `95% HDI` = fmt_hdi(gg$hdi_lo, gg$hdi_hi, 3),
                    BF10 = fmt_bf(gg$bf10), `% in ROPE` = fmt_pct(gg$pct_in_rope),
                    check.names = FALSE)
    mm <- metadelta_contrasts(s$m, s$d)
    b <- data.frame(Parameter = "meta-Δ", Sample = s$sample, Domain = dlab(mm$mod),
                    Estimate = fmt_num(mm$mean, 3),
                    `95% HDI` = fmt_hdi(mm$hdi_lo, mm$hdi_hi, 3),
                    BF10 = DASH, `% in ROPE` = DASH, check.names = FALSE)
    rbind(a, b)
  }))
  write_tbl("S10.md", paste(
    "Table S10. Relative-criterion robustness fit. The unified model refitted with the",
    "confidence criteria placed relative to the type-1 criterion (meta-c/meta-d′ = c/d′)",
    "rather than at fixed positions; quantities and samples as in Table 3.",
    LEGEND_BF, ROPE_LOGM), df)
}


## ------------------------------------------------------------- versions.md

tbl_versions <- function() {
  ver <- function(p) tryCatch(as.character(utils::packageVersion(p)),
                              error = function(e) "not determined")
  cs <- tryCatch(suppressMessages(cmdstanr::cmdstan_version()),
                 error = function(e) "not determined")
  df <- data.frame(
    Software = c("R", "brms", "cmdstanr", "CmdStan", "hmetad", "ggplot2", "bayestestR"),
    Version  = c(as.character(getRversion()), ver("brms"), ver("cmdstanr"), cs,
                 ver("hmetad"), ver("ggplot2"), ver("bayestestR")),
    check.names = FALSE)
  write_tbl("versions.md", "Table S11. Software versions used for model fitting.", df)
}


## ------------------------------------------------------------ KEY_NUMBERS.md

key_numbers <- function() {
  cal  <- rd("effects_calibration.csv")
  calm <- subset(cal, model == "calibration_cellmean")
  cal  <- subset(cal, model == "calibration")
  cnf  <- subset(rd("effects_confidence.csv"), model == "confidence")
  vmp  <- subset(rd("effects_confidence_vmp.csv"), model == "confidence_vmp")
  pc   <- subset(rd("effects_positive_controls.csv"), model == "accuracy (percentage points)")
  rt   <- subset(rd("effects_rt.csv"), model == "rt")
  st   <- subset(rd("effects_staircase.csv"), model == "staircase_sd")
  lgM  <- subset(rd("effects_local_global.csv"), model == "logM")
  lgD  <- subset(rd("effects_local_global.csv"), model == "metadelta")
  gfe  <- subset(rd("effects_fe_fixed_gender.csv"), param == "log M-ratio")
  gvm  <- subset(rd("effects_vmp_fixed_gender.csv"), param == "log M-ratio")
  qc   <- rd("cell_qc.csv"); qcv <- rd("vmp_cell_qc.csv")
  cvm  <- readRDS(file.path(DATA, "cells_vmp.rds"))
  vsub <- unique(cvm[, c("subj", "gender", "age")])

  e <- function(r, dp, bf = TRUE, rope = FALSE) {
    s <- paste0(fmt_num(r$mean, dp), " ", fmt_hdi(r$hdi_lo, r$hdi_hi, dp))
    if (bf)   s <- paste0(s, ", BF10 ", fmt_bf(if ("BF10" %in% names(r)) r$BF10 else r$bf10))
    if (rope) s <- paste0(s, ", ROPE ", fmt_pct(if ("pct_rope" %in% names(r)) r$pct_rope else r$pct_in_rope), "%")
    s
  }
  cm <- function(g, t) fmt_num(pick(calm, sprintf("calibration: DOMAIN-GENERAL, %s, %s", g, t))$mean, 0)
  md <- function(f, key) {
    r <- metadelta_contrasts(f, "domain-general")
    paste0(fmt_num(r$mean, 3), " ", fmt_hdi(r$hdi_lo, r$hdi_hi, 3),
           ", BF10 ", fmt_bf(metadelta_bf(key)))
  }
  age <- function(g) sprintf("%.1f (%.1f)", mean(vsub$age[vsub$gender == g]),
                             sd(vsub$age[vsub$gender == g]))

  L <- c(
    "# Key numbers for the prose",
    "",
    "All contrasts are women minus men; intervals are 95% HDIs.",
    "",
    paste0("- Calibration, domain-general cell means (pp): women pre ", cm("Women", "pre"),
           ", post ", cm("Women", "post"), "; men pre ", cm("Men", "pre"),
           ", post ", cm("Men", "post")),
    paste0("- Calibration, gender, domain-general (pp): ",
           e(pick(cal, "gender (F-M): DOMAIN-GENERAL"), 0)),
    paste0("- Calibration, gender x time, domain-general (pp): ",
           e(pick(cal, "gender x time: DOMAIN-GENERAL"), 0)),
    paste0("- Confidence, gender, domain-general (discovery): ",
           e(pick(cnf, "gender (F-M): DOMAIN-GENERAL"), 2)),
    paste0("- Confidence, gender, domain-general (VMP): ",
           e(pick(vmp, "gender (F-M): DOMAIN-GENERAL"), 2)),
    paste0("- Confidence, gender x accuracy, domain-general (discovery): ",
           e(pick(cnf, "gender x accuracy: DOMAIN-GENERAL"), 2, rope = TRUE)),
    paste0("- Confidence, gender x accuracy, domain-general (VMP): ",
           e(pick(vmp, "gender x accuracy: DOMAIN-GENERAL"), 2, rope = TRUE)),
    paste0("- Confidence, correct trials, domain-general (discovery): ",
           e(pick(cnf, "gender on CORRECT trials: DOMAIN-GENERAL"), 2)),
    paste0("- Confidence, incorrect trials, domain-general (discovery): ",
           e(pick(cnf, "gender on INCORRECT trials: DOMAIN-GENERAL"), 2)),
    paste0("- Confidence, correct trials, domain-general (VMP): ",
           e(pick(vmp, "gender on CORRECT trials: DOMAIN-GENERAL"), 2)),
    paste0("- Confidence, incorrect trials, domain-general (VMP): ",
           e(pick(vmp, "gender on INCORRECT trials: DOMAIN-GENERAL"), 2)),
    paste0("- meta-Delta, gender, domain-general (discovery): ",
           md("effects_fe_fixed_metadelta.csv", "first edition")),
    paste0("- meta-Delta, gender, domain-general (VMP): ",
           md("effects_vmp_fixed_metadelta.csv", "VMP replication")),
    paste0("- log M-ratio, gender, domain-general (discovery): ",
           e(pick(gfe, "domain-general"), 3, rope = TRUE)),
    paste0("- log M-ratio, gender, domain-general (VMP): ",
           e(pick(gvm, "domain-general"), 3, rope = TRUE)),
    paste0("- Accuracy, gender, domain-general (pp): ",
           e(pick(pc, "gender (F-M): DOMAIN-GENERAL"), 1, rope = TRUE)),
    paste0("- Accuracy, gender, Calories (pp): ",
           e(pick(pc, "gender (F-M): Calories"), 1, rope = TRUE)),
    paste0("- RT error slowing, gender x accuracy, domain-general (s): ",
           e(pick(rt, "gender x accuracy (error slowing F-M): DOMAIN-GENERAL"), 2, rope = TRUE)),
    paste0("- Staircase SD, gender, domain-general: ",
           e(pick(st, "gender (F-M): DOMAIN-GENERAL"), 2, rope = TRUE)),
    paste0("- Coupling of log M to baseline calibration, domain-general: ",
           e(pick(lgM, "coupling to calibration (pre): DOMAIN-GENERAL"), 3)),
    paste0("- Coupling of log M to pre->post update, domain-general: ",
           e(pick(lgM, "coupling to pre->post update: DOMAIN-GENERAL"), 3)),
    paste0("- Coupling of meta-Delta to baseline calibration, domain-general: ",
           e(pick(lgD, "coupling to calibration (pre): DOMAIN-GENERAL"), 3)),
    paste0("- Coupling of meta-Delta to pre->post update, domain-general: ",
           e(pick(lgD, "coupling to pre->post update: DOMAIN-GENERAL"), 3)),
    paste0("- Coupling x gender (calibration), log M / meta-Delta: ",
           e(pick(lgM, "coupling x gender (calibration): DOMAIN-GENERAL"), 3), " / ",
           e(pick(lgD, "coupling x gender (calibration): DOMAIN-GENERAL"), 3)),
    paste0("- Coupling x gender (update), log M / meta-Delta: ",
           e(pick(lgM, "coupling x gender (update): DOMAIN-GENERAL"), 3), " / ",
           e(pick(lgD, "coupling x gender (update): DOMAIN-GENERAL"), 3)),
    paste0("- VMP age, mean (SD): women ", age("Feminin"), ", men ", age("Masculin")),
    paste0("- Cells dropped, original: ", sum(qc$drop), " of ", nrow(qc),
           " (", sum(qc$drop & qc$drop_reason == "dprime"), " low d-prime, ",
           sum(qc$drop & qc$drop_reason == "thin"), " too few trials)"),
    paste0("- Cells dropped, VMP: ", sum(qcv$drop), " of ", nrow(qcv),
           " (", sum(qcv$drop & qcv$drop_reason == "dprime"), " low d-prime, ",
           sum(qcv$drop & qcv$drop_reason == "thin"), " too few trials)"))

  con <- file(file.path(OUT, "KEY_NUMBERS.md"), open = "wt", encoding = "UTF-8")
  writeLines(L, con); close(con)
  message("wrote KEY_NUMBERS.md (", length(L), " lines)")
}


## ------------------------------------------------------------------- run all

tbl_T1(); tbl_T2(); tbl_T3(); tbl_S12()
tbl_S2(); tbl_S3(); tbl_S4(); tbl_S5(); tbl_S6()
tbl_S7(); tbl_S8(); tbl_S9(); tbl_S10()
tbl_versions()
key_numbers()
