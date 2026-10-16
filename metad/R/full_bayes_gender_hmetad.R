# Gender differences in metacognitive efficiency --------------------------

# clear workspace
rm(list = ls()) 

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

# select female and male subject numbers
fem <- trialdata %>%
  filter(gender == "Feminin") %>%
  select(subj) %>%
  unlist()
mas <- trialdata %>%
  filter(gender == "Masculin") %>%
  select(subj)%>%
  unlist()

fem <- as.character(unique(fem))
mas <- as.character(unique(mas))

# preallocate data frames
nR_S1_mem <- data.frame(rep(0, 14))
nR_S2_mem <- data.frame(rep(0, 14))
nR_S1_vis <- data.frame(rep(0, 14))
nR_S2_vis <- data.frame(rep(0, 14))
nR_S1_gdp <- data.frame(rep(0, 14))
nR_S2_gdp <- data.frame(rep(0, 14))
nR_S1_cal <- data.frame(rep(0, 14))
nR_S2_cal <- data.frame(rep(0, 14))

# run trials to counts
for(m in 1:length(mods)){
  modtrials = subset(trialdata, mod == mods[m])
  
  for (i in 1:length(subs)) {
    trials = subset(modtrials, subj == subs[i])
    
    stimID = trials$signal
    response = trials$response
    rating = trials$confidence
    nRatings = 7
    padAmount = 0
    padCells= 0
    
    
    nR_S1 <- list()
    nR_S2 <- list()
    
    if (padAmount == 0){
      padAmount = 1/(2*nRatings)}
    # S1 responses
    for (r in nRatings:1){
      cs1 <- 0
      cs2 <- 0
      for (t in 1:length(stimID)){
        s = stimID[t]
        x = response[t]
        y = rating[t]
        
        if ((s==0) & (x==0) & (y==r)){
          (cs1 <- cs1+1)}
        if ((s==1) & (x==0) & (y==r)){
          (cs2 <- cs2+1)}
      }
      nR_S1 <- append(nR_S1,cs1)
      nR_S2 <- append(nR_S2,cs2)
    }
    
    # S2 responses
    for (r in 1:nRatings){
      cs1 <- 0
      cs2 <- 0
      for (t in 1:length(stimID)){
        s = stimID[t]
        x = response[t]
        y = rating[t]
        
        if ((s==0) & (x==1) & (y==r)){
          (cs1 <- cs1+1)}
        if ((s==1) & (x==1) & (y==r)){
          (cs2 <- cs2+1)}
      }
      nR_S1 <- append(nR_S1,cs1)
      nR_S2 <- append(nR_S2,cs2)
    }
    
    
    # pad response counts to avoid zeros
    nR_S1 <- as.numeric(nR_S1)
    nR_S2 <- as.numeric(nR_S2)
    if (padCells == 1){
      nR_S1 <- lapply(nR_S1,FUN= function(x) x+padAmount)
      nR_S2 <- lapply(nR_S2,FUN= function(x) x+padAmount)}
    
    # make task dataframe
    nR_S1_temp <- data.frame(nR_S1)
    colnames(nR_S1_temp) <- subs[i]
    
    nR_S2_temp <- data.frame(nR_S2)
    colnames(nR_S2_temp) <- subs[i]
    
    # save modalities seperately
    if (mods[m] == 'memory'){
      nR_S1_mem <- cbind(nR_S1_mem, nR_S1_temp)
      nR_S2_mem <- cbind(nR_S2_mem, nR_S2_temp)
    }
    if (mods[m] == 'vision'){
      nR_S1_vis <- cbind(nR_S1_vis, nR_S1_temp)
      nR_S2_vis <- cbind(nR_S2_vis, nR_S2_temp)
    }
    if (mods[m] == 'GDP'){
      nR_S1_gdp <- cbind(nR_S1_gdp, nR_S1_temp)
      nR_S2_gdp <- cbind(nR_S2_gdp, nR_S2_temp)
    }
    if (mods[m] == 'Calories'){
      nR_S1_cal <- cbind(nR_S1_cal, nR_S1_temp)
      nR_S2_cal <- cbind(nR_S2_cal, nR_S2_temp)
    }
  }
  
}

# delete useless first columns
nR_S1_mem <- nR_S1_mem[, -1]
nR_S2_mem <- nR_S2_mem[, -1]
nR_S1_vis <- nR_S1_vis[, -1]
nR_S2_vis <- nR_S2_vis[, -1]
nR_S1_gdp <- nR_S1_gdp[, -1]
nR_S2_gdp <- nR_S2_gdp[, -1]
nR_S1_cal <- nR_S1_cal[, -1]
nR_S2_cal <- nR_S2_cal[, -1]

# List creation for model inputs
nR_S1_f <- list(nR_S1_mem[, fem],
                nR_S1_vis[, fem],
                nR_S1_gdp[, fem],
                nR_S1_cal[, fem])
nR_S2_f <- list(nR_S2_mem[, fem],
                nR_S2_vis[, fem],
                nR_S2_gdp[, fem],
                nR_S2_cal[, fem])

nR_S1_m <- list(nR_S1_mem[, mas],
                nR_S1_vis[, mas],
                nR_S1_gdp[, mas],
                nR_S1_cal[, mas])
nR_S2_m <- list(nR_S2_mem[, mas],
                nR_S2_vis[, mas],
                nR_S2_gdp[, mas],
                nR_S2_cal[, mas])

# Run model ---------------------------------------------------------------
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

# check for model convergence
# for females
sum(Rhat_f$conv.Point.est. > 1.1)
Rhat_f[Rhat_f$conv.Point.est. > 1.1,]

# for males
sum(Rhat_m$conv.Point.est. > 1.1)
Rhat_m[Rhat_m$conv.Point.est. > 1.1,]


# Mratio's and HDI --------------------------------------------------------
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

# plots -------------------------------------------------------------------
# check convergence: plot trace mcmc
#traceplot(output_f)
#traceplot(output_m)

# plot histograms

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

HDIV <- -90
HDIU <- -50
HDIL<- -130

# now one big plot
group$mod <- "group"
mem$mod <- "mem"
vis$mod <- "vis"
gdp$mod <- "gdp"
cal$mod <- "cal"

allratio <- rbind(group, mem, vis, gdp, cal)

ggplot(allratio, aes(x = mratio, fill = mod, col = mod)) +
  geom_histogram(alpha = 0.3, bins = 80, position = "identity", size = .1) +
  #  geom_vline(aes(xintercept = mean(mratio[mod == "group"])), colour = "#0D0887FF", linetype = "dashed", size = 0.5) +
  geom_vline(aes(xintercept = mean(mratio[mod == "mem"])), colour = "#B12A90FF", linetype = "dashed", size = 0.5) +
  geom_vline(aes(xintercept = mean(mratio[mod == "vis"])), colour = "#6A00A8FF", linetype = "dashed", size = 0.5) +
  geom_vline(aes(xintercept = mean(mratio[mod == "gdp"])), colour = "#FCA636FF", linetype = "dashed", size = 0.5) +
  geom_vline(aes(xintercept = mean(mratio[mod == "cal"])), colour = "#E16462FF", linetype = "dashed", size = 0.5) +
  labs(title = "Metacognitive efficiency", y= "Sample count", x = "M-ratio") +
  xlim(c(-1.5, 1.5)) +
  scale_color_manual(values=c("#0D0887FF", "#B12A90FF", "#6A00A8FF", "#FCA636FF", "#E16462FF"), name = "Modality", limits = c("group", "mem", "vis", "gdp","cal"), labels = c("Group", "Memory", "Vision", "GDP", "Calories")) +
  scale_fill_manual(values=c("#0D0887FF", "#B12A90FF", "#6A00A8FF", "#FCA636FF", "#E16462FF"), name = "Modality", limits = c("group", "mem", "vis", "gdp","cal"), labels = c("Group", "Memory", "Vision", "GDP", "Calories")) +
  theme_cowplot()+
  theme(axis.text.x = element_text(family = "serif", color = "black", size = 10), axis.text.y = element_text(size = 10), text=element_text(family = "serif", size = 10), plot.title = element_text(hjust = 0.5, size = 14)) 


ggplot(allratio, aes(x = mratio, fill = mod, col = mod)) +
  geom_histogram(alpha = 0.3, bins = 80, position = "identity", size = .1) +
#  geom_vline(aes(xintercept = mean(mratio[mod == "group"])), colour = "#0D0887FF", linetype = "dashed", size = 0.5) +
  geom_vline(aes(xintercept = mean(mratio[mod == "mem"])), colour = "#B12A90FF", linetype = "dashed", size = 0.5) +
  geom_vline(aes(xintercept = mean(mratio[mod == "vis"])), colour = "#6A00A8FF", linetype = "dashed", size = 0.5) +
  geom_vline(aes(xintercept = mean(mratio[mod == "gdp"])), colour = "#FCA636FF", linetype = "dashed", size = 0.5) +
  geom_vline(aes(xintercept = mean(mratio[mod == "cal"])), colour = "#E16462FF", linetype = "dashed", size = 0.5) +
  labs(title = "Metacognitive efficiency", y= "Sample count", x = "M-ratio") +
  xlim(c(-.2, .5)) +
  scale_color_manual(values=c("#0D0887FF", "#B12A90FF", "#6A00A8FF", "#FCA636FF", "#E16462FF"), name = "Modality", limits = c("group", "mem", "vis", "gdp","cal"), labels = c("Group", "Memory", "Vision", "GDP", "Calories")) +
  scale_fill_manual(values=c("#0D0887FF", "#B12A90FF", "#6A00A8FF", "#FCA636FF", "#E16462FF"), name = "Modality", limits = c("group", "mem", "vis", "gdp","cal"), labels = c("Group", "Memory", "Vision", "GDP", "Calories")) +
  theme_cowplot()+
  theme(axis.text.x = element_text(family = "serif", color = "black", size = 10), axis.text.y = element_text(size = 10), text=element_text(family = "serif", size = 10), plot.title = element_text(hjust = 0.5, size = 14)) 



stats <- allratio %>%
  group_by(mod) %>%
  summarize(mean_mratio = mean(mratio), sd_mratio = sd(mratio))

modorder <- c("group", "mem", "vis", "gdp", "cal")
stats <- stats %>%
  slice(match(modorder, mod))

stats <- cbind(stats, hdis)

save(nR_S2_m)
