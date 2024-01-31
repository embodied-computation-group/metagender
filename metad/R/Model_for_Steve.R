
# Gender differences full hierarchical model ------------------------------

# call packages
library(rjags)
library(coda)
library(magrittr)
library(tibble)
library(tidyr)
library(dplyr)
library(car)
library(rstatix)
library(HDInterval)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(ggsignif)
library(gridExtra)
library(patchwork)
library(cowplot)
library(plotrix)

# source code
source("fit_metad_groupcorr_groupnode.R")

# prepare data for modelling ----------------------------------------------
# load data
trialdata <- read.csv('./Hmetad_data.csv')
trialdata <- trialdata[, -1]
trialdata <- na.omit(trialdata)

# identify subject numbers and modalities
subs <- unique(trialdata$subj)
mods <- unique(trialdata$mod)


# Run the model with nR_S -------------------------------------------------
# females
output_f <- fit_metad_groupcorr(nR_S1 = nR_S1_f, nR_S2 = nR_S2_f)
# males
output_m <- fit_metad_groupcorr(nR_S1 = nR_S1_m, nR_S2 = nR_S2_m)

# Convergence check: calculate Rhat
# Rhat females
value_f <- gelman.diag(output_f, confidence = 0.95)
Rhat_f <- data.frame(conv = value_f$psrf)
# Rhat males
value_m <- gelman.diag(output_m, confidence = 0.95)
Rhat_m <- data.frame(conv = value_m$psrf)

# Convergence check: any values above 1.1
# for females
sum(Rhat_f$conv.Point.est. > 1.1)
Rhat_f[Rhat_f$conv.Point.est. > 1.1,]

# for males
sum(Rhat_m$conv.Point.est. > 1.1)
Rhat_m[Rhat_m$conv.Point.est. > 1.1,]


# M-ratio differences and HDI's -------------------------------------------

# get group mratio's
groupf <- output_f[,"mu_logMratio_group"]
groupm <- output_m[,"mu_logMratio_group"]
# get memory mratio's
memf <- output_f[,"mu_logMratio_task[1]"]
memm <- output_m[,"mu_logMratio_task[1]"]
# get vision mratio's
visf <- output_f[,"mu_logMratio_task[2]"]
vism <- output_m[,"mu_logMratio_task[2]"]
# get gdp mratio's
gdpf <- output_f[,"mu_logMratio_task[3]"]
gdpm <- output_m[,"mu_logMratio_task[3]"]
# get cal mratio's
calf <- output_f[,"mu_logMratio_task[4]"]
calm <- output_m[,"mu_logMratio_task[4]"]

# average models
# group
groupfm <- rbind(as.data.frame(groupf[[1]]), as.data.frame(groupf[[2]]), as.data.frame(groupf[[3]]))
groupmm <- rbind(as.data.frame(groupm[[1]]), as.data.frame(groupm[[2]]), as.data.frame(groupm[[3]]))
# memory
memfm <- rbind(as.data.frame(memf[[1]]), as.data.frame(memf[[2]]), as.data.frame(memf[[3]]))
memmm <- rbind(as.data.frame(memm[[1]]), as.data.frame(memm[[2]]), as.data.frame(memm[[3]]))
# vision
visfm <- rbind(as.data.frame(visf[[1]]), as.data.frame(visf[[2]]), as.data.frame(visf[[3]]))
vismm <- rbind(as.data.frame(vism[[1]]), as.data.frame(vism[[2]]), as.data.frame(vism[[3]]))

# gdp
gdpfm <- rbind(as.data.frame(gdpf[[1]]), as.data.frame(gdpf[[2]]), as.data.frame(gdpf[[3]]))
gdpmm <- rbind(as.data.frame(gdpm[[1]]), as.data.frame(gdpm[[2]]), as.data.frame(gdpm[[3]]))

# calories
calfm <- rbind(as.data.frame(calf[[1]]), as.data.frame(calf[[2]]), as.data.frame(calf[[3]]))
calmm <-rbind(as.data.frame(calm[[1]]), as.data.frame(calm[[2]]), as.data.frame(calm[[3]]))

# new group value
groupfm <- rbind(memfm, visfm, gdpfm, calfm)
groupmm <- rbind(memmm, vismm, gdpmm, calmm)
# calculate F-M
groupdiff <- groupfm - groupmm
memdiff <- memfm - memmm
visdiff <- visfm - vismm
gdpdiff <- gdpfm - gdpmm
caldiff <- calfm - calmm

# calculate HDI's
hdigroup <- hdi(groupdiff, credMass = 0.95)
hdimem <- hdi(memdiff, credMass = 0.95)
hdivis <- hdi(visdiff, credMass = 0.95)
hdigdp <- hdi(gdpdiff, credMass = 0.95)
hdical <- hdi(caldiff, credMass = 0.95)

# in single data frame
hdis <- cbind(hdigroup, hdimem, hdivis, hdigdp, hdical)
colnames(hdis) <- cbind("group", "mem", "vis", "gdp", "cal")
hdis <- (t(hdis))


# Plot M-ratio histograms -------------------------------------------------
HDIV <- -90
HDIU <- -50
HDIL<- -130

# group
# data manipulation
group <- as.data.frame(groupdiff)
colnames(group) <- "mratio"
# plot
groupplot <- ggplot(group, aes(x = mratio)) +
  geom_histogram(color = "black", fill = "#440154FF", bins = 50, alpha = 0.5) +
  geom_vline(aes(xintercept = mean(mratio)), colour = "black", linetype = "dashed", size = 0.5) +
  coord_cartesian(clip="on") +
  annotate("segment", x = hdigroup[1], xend = hdigroup[2], y = HDIV, yend = HDIV) +
  annotate("segment", x = hdigroup[1], xend = hdigroup[1], y = HDIU, yend = HDIL) +
  annotate("segment", x = hdigroup[2], xend = hdigroup[2], y = HDIU, yend = HDIL) + 
  labs(title = "Group", y= "Sample count", x = "M-ratio") +
  theme_cowplot()+
  theme(axis.text.x = element_text(family = "serif", color = "black", size = 10), axis.text.y = element_text(size = 10), text=element_text(family = "serif", size = 10), plot.title = element_text(hjust = 0.5, size = 14)) 

# mem
# data manipulation
mem <- as.data.frame(memdiff)
colnames(mem) <- "mratio"
# plot
memplot <- ggplot(mem, aes(x = mratio)) +
  geom_histogram(color = "black", fill = "#440154FF", bins = 50, alpha = 0.5) +
  geom_vline(aes(xintercept = mean(mratio)), colour = "black", linetype = "dashed", size = 0.5) +
  coord_cartesian(clip="on") +
  annotate("segment", x = hdimem[1], xend = hdimem[2], y = HDIV, yend = HDIV) +
  annotate("segment", x = hdimem[1], xend = hdimem[1], y = HDIU, yend = HDIL) +
  annotate("segment", x = hdimem[2], xend = hdimem[2], y = HDIU, yend = HDIL) + 
  labs(title = "Memory", y= "Sample count", x = "M-ratio") +
  theme_cowplot()+
  theme(axis.text.x = element_text(family = "serif", color = "black", size = 10), axis.text.y = element_text(size = 10), text=element_text(family = "serif", size = 10), plot.title = element_text(hjust = 0.5, size = 14)) 

# vis
# data manipulation
vis <- as.data.frame(visdiff)
colnames(vis) <- "mratio"
# plot
visplot <- ggplot(vis, aes(x = mratio)) +
  geom_histogram(color = "black", fill = "#440154FF", bins = 50, alpha = 0.5) +
  geom_vline(aes(xintercept = mean(mratio)), colour = "black", linetype = "dashed", size = 0.5) +
  coord_cartesian(clip="on") +
  annotate("segment", x = hdivis[1], xend = hdivis[2], y = HDIV, yend = HDIV) +
  annotate("segment", x = hdivis[1], xend = hdivis[1], y = HDIU, yend = HDIL) +
  annotate("segment", x = hdivis[2], xend = hdivis[2], y = HDIU, yend = HDIL) + 
  labs(title = "Vision", y= "Sample count", x = "M-ratio") +
  theme_cowplot()+
  theme(axis.text.x = element_text(family = "serif", color = "black", size = 10), axis.text.y = element_text(size = 10), text=element_text(family = "serif", size = 10), plot.title = element_text(hjust = 0.5, size = 14)) 

# gdp
# data manipulation
gdp <- as.data.frame(gdpdiff)
colnames(gdp) <- "mratio"
# plot
gdpplot <- ggplot(gdp, aes(x = mratio)) +
  geom_histogram(color = "black", fill = "#440154FF", bins = 50, alpha = 0.5) +
  geom_vline(aes(xintercept = mean(mratio)), colour = "black", linetype = "dashed", size = 0.5) +
  coord_cartesian(clip="on") +
  annotate("segment", x = hdigdp[1], xend = hdigdp[2], y = HDIV, yend = HDIV) +
  annotate("segment", x = hdigdp[1], xend = hdigdp[1], y = HDIU, yend = HDIL) +
  annotate("segment", x = hdigdp[2], xend = hdigdp[2], y = HDIU, yend = HDIL) + 
  labs(title = "GDP", y= "Sample count", x = "M-ratio") +
  theme_cowplot()+
  theme(axis.text.x = element_text(family = "serif", color = "black", size = 10), axis.text.y = element_text(size = 10), text=element_text(family = "serif", size = 10), plot.title = element_text(hjust = 0.5, size = 14)) 

# cal
# data manipulation
cal <- as.data.frame(caldiff)
colnames(cal) <- "mratio"
# plot
calplot <- ggplot(cal, aes(x = mratio)) +
  geom_histogram(color = "black", fill = "#440154FF", bins = 50,  alpha = 0.5) +
  geom_vline(aes(xintercept = mean(mratio)), colour = "black", linetype = "dashed", size = 0.5) +
  coord_cartesian(clip="on") +
  annotate("segment", x = hdical[1], xend = hdical[2], y = HDIV, yend = HDIV) + #  vertical line
  annotate("segment", x = hdical[1], xend = hdical[1], y = HDIU, yend = HDIL) + # first horizontal
  annotate("segment", x = hdical[2], xend = hdical[2], y = HDIU, yend = HDIL) + # second horizontal
  labs(title = "Calories", y= "Sample count", x = "M-ratio") +
  theme_cowplot()+
  theme(axis.text.x = element_text(family = "serif", color = "black", size = 10), axis.text.y = element_text(size = 10), text=element_text(family = "serif", size = 10), plot.title = element_text(hjust = 0.5, size = 14)) 

groupplot / (memplot + visplot) / (gdpplot + calplot) + plot_layout()

