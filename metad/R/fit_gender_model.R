# fit gender model on data
rm(list = ls()) 

## Packages ----------------------------------------------------------------
library(rjags)
library(coda)
library(magrittr)
library(tibble)
library(tidyr)
library(dplyr)
library(rstatix)
library(ggplot2)
library(cowplot)


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
nR_S1_gen <- data.frame(rep(0, 14))
nR_S2_gen <- data.frame(rep(0, 14))

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
  
  # save modalities in group
  nR_S1_gen <- cbind(nR_S1_gen, nR_S1_temp)
  nR_S2_gen <- cbind(nR_S2_gen, nR_S2_temp)
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
nR_S1_gen <- nR_S1_gen[, -1]
nR_S2_gen <- nR_S2_gen[, -1]

# List creation for model inputs
  nR_S1_f <- list(nR_S1_mem[, fem],
                  nR_S1_vis[, fem],
                  nR_S1_gdp[, fem],
                  nR_S1_cal[, fem],
                  nR_S1_gen[, fem])
  nR_S2_f <- list(nR_S2_mem[, fem],
                  nR_S2_vis[, fem],
                  nR_S2_gdp[, fem],
                  nR_S2_cal[, fem],
                  nR_S2_gen[, fem])

  nR_S1_m <- list(nR_S1_mem[, mas],
                  nR_S1_vis[, mas],
                  nR_S1_gdp[, mas],
                  nR_S1_cal[, mas],
                  nR_S1_gen[, mas])
  nR_S2_m <- list(nR_S2_mem[, mas],
                  nR_S2_vis[, mas],
                  nR_S2_gdp[, mas],
                  nR_S2_cal[, mas],
                  nR_S2_gen[, mas])

# run model ---------------------------------------------------------------
# females
source("fit_metad_groupcorr_groupnode.R")
output_f <- fit_metad_groupcorr(nR_S1 = nR_S1_f, nR_S2 = nR_S2_f)
# males
output_m <- fit_metad_groupcorr(nR_S1 = nR_S1_m, nR_S2 = nR_S2_m)

## Model output ------------------------------------------------------------
# Values females
value_f <- summary(output_f)
stat_f <- data.frame(mean = value_f$statistics[,"Mean"])
stat_f %<>%
  rownames_to_column(var = "name")
# Values males
value_m <- summary(output_m)
stat_m <- data.frame(mean = value_m$statistics[,"Mean"])
stat_m %<>%
  rownames_to_column(var = "name")

# Rhat females
value_f <- gelman.diag(output_f, confidence = 0.95)
Rhat_f <- data.frame(conv = value_f$psrf)
# Rhat males
value_m <- gelman.diag(output_m, confidence = 0.95)
Rhat_m <- data.frame(conv = value_m$psrf)

# HDI females
HDI_f <- data.frame(HPDinterval(groupf1, prob = 0.95))
HDI_f %<>%
  rownames_to_column(var = "name")

library(HDInterval)
hdi(groupm1, credMass = 0.95)
# HDI males
HDI_m <- data.frame(HPDinterval(m1, prob = 0.95))
HDI_m %<>%
  rownames_to_column(var = "name")

HDI_f - HDI_m

# All values in the same dataframe
Fit_f <- stat_f %>%
  cbind(lower = HDI_f$lower,
        upper = HDI_f$upper,
        Rhat = Rhat_f[,1])
# All values in the same dataframe
Fit_m <- stat_m %>%
  cbind(lower = HDI_m$lower,
        upper = HDI_m$upper,
        Rhat = Rhat_m[,1])

## Check model convergence ---------------------------------------------------------
# Plot trace mcmc
traceplot(output_f)
traceplot(output_m)
# looks great

# Check Rhats
sum(Fit_f$Rhat > 1.1) #one value exceeds: 1.106552, seems acceptable
sum(Fit_m$Rhat > 1.1)

# make mratio histograms
mrat_f <- Fit_f[c(1:(4*length(fem))), ]
mrat_m <- Fit_m[c(1:(4*length(mas))), ]

hist(mrat_f$mean)
hist(mrat_m$mean)


# make HDI overview -------------------------------------------------------
groupm <- Fit_m[589,c(3,4)]
groupf <- Fit_f[693,c(3,4)]

taskm <- Fit_m[c(590:593),c(3,4)]
taskf <- Fit_f[c(694:697),c(3,4)]

fems <- rbind(groupf,taskf)
mascs <- rbind(groupm, taskm)
hdis <- cbind(mods, fems)
hdis <- cbind(hdis, mascs)
colnames(hdis) <- cbind("mod", "low_f", "high_f", "low_m", "high_m")
hdis$lowdif <- hdis$low_f - hdis$low_m
hdis$highdif <- hdis$high_f - hdis$high_m

ggplot(hdis, aes(y = mod, x = lowdif, col = mod)) + 
  geom_segment(aes(yend = mod, xend = highdif))

hdisl <- rbind(fems, mascs)

hdisl <- rbind(hdisl, hdis[,c(6,7)])
hdisl <- cbind(g, hdisl)
hdisl <- cbind(modss, hdisl)


ggplot(hdisl, aes(x = modss, ymin = lower, ymax, col = g)) + 
  geom_segment(aes(yend = modss, xend = upper),alpha = 0.5, size=7)+
  scale_color_manual(values=c("indianred2", "cyan4", "darkorchid3"), labels = c("Female", "Male", "Difference")) +
  scale_fill_manual(values=c("indianred2", "cyan4", "darkorchid3"), labels = c("Female", "Male", "Difference")) +
  labs(title="HDI per node", x="Bounds", y="Modality") +
  scale_y_discrete(limits = c("cal", "gdp", "vis", "mem", "group"))
  

# prep for real plot
library(dplyr)
library(tidyr)
library(car)
library(rstatix)
library(stringr)
library(ggplot2)
library(ggpubr)
library(ggsignif)
library(gridExtra)
library(patchwork)
library(cowplot)
library(plotrix)

mrat_f$gender <- "F"
mrat_m$gender <- "M"
mrat <- rbind(mrat_f, mrat_m)
colnames(mrat)[2] <- "mratio"
mratplot <- mrat[, c(1, 2, 6)]

# and now better

ggplot(comp1, aes(x = mratio, fill = gender, color = gender))+
  geom_histogram(alpha = 0.5, position = "dodge", bins = 40)+
  scale_color_manual(values=c("indianred2", "cyan4"), name = "Gender", labels = c("Female", "Male")) +
  scale_fill_manual(values=c("indianred2", "cyan4"), name = "Gender", labels = c("Female", "Male")) +
  labs(title = "M-ratio by gender", y= "Sample count", x = "M-ratio") +
  theme_cowplot()+
  theme(axis.text.x = element_text(family = "serif", color = "black", size = 10), axis.text.y = element_text(size = 10), text=element_text(family = "serif", size = 10), plot.title = element_text(hjust = 0.5, size = 18)) 

# memory
memplot <- mratplot[grepl(",1]", mratplot$name), ]

ggplot(memplot, aes(x=mean, fill = gender, color = gender))+
  geom_histogram(alpha = 0.5, position = "dodge", bins = 40)+
  scale_color_manual(values=c("indianred2", "cyan4"), name = "Gender", labels = c("Female", "Male")) +
  scale_fill_manual(values=c("indianred2", "cyan4"), name = "Gender", labels = c("Female", "Male")) +
  labs(title = "Memory M-ratio by gender", y= "Sample count", x = "M-ratio") +
  theme_cowplot()+
  theme(axis.text.x = element_text(family = "serif", color = "black", size = 10), axis.text.y = element_text(size = 10), text=element_text(family = "serif", size = 10), plot.title = element_text(hjust = 0.5, size = 18)) 


# vision
visplot <- mratplot[grepl(",2]", mratplot$name), ]

ggplot(visplot, aes(x=mean, fill = gender, color = gender))+
  geom_histogram(alpha = 0.5, position = "dodge", bins = 40)+
  scale_color_manual(values=c("indianred2", "cyan4"), name = "Gender", labels = c("Female", "Male")) +
  scale_fill_manual(values=c("indianred2", "cyan4"), name = "Gender", labels = c("Female", "Male")) +
  labs(title = "Vision M-ratio by gender", y= "Sample count", x = "M-ratio") +
  theme_cowplot()+
  theme(axis.text.x = element_text(family = "serif", color = "black", size = 10), axis.text.y = element_text(size = 10), text=element_text(family = "serif", size = 10), plot.title = element_text(hjust = 0.5, size = 18)) 

# gdp
gdpplot <- mratplot[grepl(",3]", mratplot$name), ]

ggplot(gdpplot, aes(x=mean, fill = gender, color = gender))+
  geom_histogram(alpha = 0.5, position = "dodge", bins = 40)+
  scale_color_manual(values=c("indianred2", "cyan4"), name = "Gender", labels = c("Female", "Male")) +
  scale_fill_manual(values=c("indianred2", "cyan4"), name = "Gender", labels = c("Female", "Male")) +
  labs(title = "GDP M-ratio by gender", y= "Sample count", x = "M-ratio") +
  theme_cowplot()+
  theme(axis.text.x = element_text(family = "serif", color = "black", size = 10), axis.text.y = element_text(size = 10), text=element_text(family = "serif", size = 10), plot.title = element_text(hjust = 0.5, size = 18)) 

# cal
calplot <- mratplot[grepl(",4]", mratplot$name), ]

ggplot(calplot, aes(x=mean, fill = gender, color = gender))+
  geom_histogram(alpha = 0.5, position = "dodge", bins = 40)+
  scale_color_manual(values=c("indianred2", "cyan4"), name = "Gender", labels = c("Female", "Male")) +
  scale_fill_manual(values=c("indianred2", "cyan4"), name = "Gender", labels = c("Female", "Male")) +
  labs(title = "Calories M-ratio by gender", y= "Sample count", x = "M-ratio") +
  theme_cowplot()+
  theme(axis.text.x = element_text(family = "serif", color = "black", size = 10), axis.text.y = element_text(size = 10), text=element_text(family = "serif", size = 10), plot.title = element_text(hjust = 0.5, size = 18)) 



# new mratios plot ------------------------------------------------------------
f1 <- output_f[[1]]
f2 <- output_f[[2]]
f3 <- output_f[[3]]
m1 <- output_m[[1]]
m2 <- output_m[[2]]
m3 <- output_m[[3]]

groupf1 <- f1[,"mu_logMratio_group"]
groupf2 <- f2[,"mu_logMratio_group"]
groupf3 <- f3[,"mu_logMratio_group"]
groupm1 <- m1[,"mu_logMratio_group"]
groupm2 <- m2[,"mu_logMratio_group"]
groupm3 <- m3[,"mu_logMratio_group"]

diff1 <- groupf1-groupm1
diff2 <- groupf2-groupm2
diff3 <- groupf3-groupm3
hdig <- hdi(diff1, credMass = 0.95)
hdig

hist(diff1)
all <- rowSums(cbind(diff1, diff2, diff3))
hist(all)

memf <- f3[,"mu_logMratio_task[1]"]
memm <- m3[,"mu_logMratio_task[1]"]
visf <- f2[,"mu_logMratio_task[2]"]
vism <- m2[,"mu_logMratio_task[2]"]
gdpf <- f3[,"mu_logMratio_task[3]"]
gdpm <- m3[,"mu_logMratio_task[3]"]
calf <- f1[,"mu_logMratio_task[4]"]
calm <- m1[,"mu_logMratio_task[4]"]

memdiff <- memf-memm
visdiff <- visf-vism
gdpdiff <- gdpf-gdpm
caldiff <- calf-calm

hdim <- hdi(memdiff, credMass = 0.95)
hdiv <- hdi(visdiff, credMass = 0.95)
hdig <- hdi(gdpdiff, credMass = 0.95)
hdic <- hdi(caldiff, credMass = 0.95)


hist(caldiff)

shapiro.test(memdiff[:,c(1:500)])
