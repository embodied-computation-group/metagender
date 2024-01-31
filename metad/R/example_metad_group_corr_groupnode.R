#####################################

# Example of using wrapper to estimate 4-task covariance at group level, together with top-level node for group Mratio
#
# Steve Fleming May 2023

#####################################

## Packages ----------------------------------------------------------------
library(rjags)
library(coda)
library(magrittr)
library(tibble)

## Create data for 3 participants and 4 tasks -------------------------------------------------------------

# Task 1
nR_S1_1 <- data.frame(
  p1 = c(52,32,35,37,26,12,4,2),
  p2 = c(27,39,46,52,14,10,9,3),
  p3 = c(112,30,15,17,17,9,0,0))
nR_S2_1 <- data.frame(
  p1 = c(2,5,15,22,33,38,40,45),
  p2 = c(2,4,9,21,57,48,34,25),
  p3 = c(0,1,7,18,12,17,27,118))

# Task 2
nR_S1_2 <- data.frame(
  p1 = c(97,49,13,9,20,11,1,0),
  p2 = c(37,41,49,44,17,11,0,1),
  p3 = c(61,45,34,28,21,9,1,1))
nR_S2_2 <- data.frame(
  p1 = c(0,1,8,23,17,33,22,96),
  p2 = c(0,2,9,18,44,46,43,38),
  p3 = c(2,5,3,22,32,38,27,71))

# Task 3
nR_S1_3 <- data.frame(
  p1 = c(47,34,36,37,26,13,4,2),
  p2 = c(112,30,15,17,17,9,0,0),
  p3 = c(27,39,46,52,14,10,9,3))
nR_S2_3 <- data.frame(
  p1 = c(0,1,7,18,12,17,27,118),
  p2 = c(2,5,15,22,33,38,40,45),
  p3 = c(2,4,9,21,57,48,34,25))

# Task 4
nR_S1_4 <- data.frame(
  p1 = c(92,52,15,9,19,9,3,1),
  p2 = c(37,41,49,44,17,11,0,1),
  p3 = c(59,47,34,28,22,7,2,1))
nR_S2_4 <- data.frame(
  p1 = c(0,1,8,23,17,33,22,96),
  p2 = c(0,2,9,18,44,46,43,38),
  p3 = c(2,5,3,22,32,38,27,71))

## Hierarchical meta_d correlation function ------------------------------------------------------

# List creation for model inputs
nR_S1 <- list(nR_S1_1,
              nR_S1_2,
              nR_S1_3,
              nR_S1_4)

nR_S2 <- list(nR_S2_1,
              nR_S2_2,
              nR_S2_3,
              nR_S2_4)

# Fit all data at once
source("fit_metad_groupcorr_groupnode.R")
output <- fit_metad_groupcorr(nR_S1 = nR_S1, nR_S2 = nR_S2)

## Model output ------------------------------------------------------------

# Values 
Value <- summary(output)
stat <- data.frame(mean = Value$statistics[,"Mean"])
stat %<>%
  rownames_to_column(var = "name")

# Rhat 
Value <- gelman.diag(output, confidence = 0.95)
Rhat <- data.frame(conv = Value$psrf)

# HDI 
HDI <- data.frame(HPDinterval(output, prob = 0.95))
HDI %<>%
    rownames_to_column(var = "name")

# All values in the same dataframe
Fit <- stat %>%
  cbind(lower = HDI$lower,
        upper = HDI$upper,
        Rhat = Rhat[,1])

## Plots ---------------------------------------------------------

# Plot trace mcmc
traceplot(output_f)
traceplot(output_m)
