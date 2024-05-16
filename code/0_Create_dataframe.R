# create megatable from self belief and confidence data
# clear environment
rm(list = ls())

library(tidyr)
library(dplyr)
library(rstatix)

# load data ---------------------------------------------------------------
data_meta <- read.csv('./data/metacognition_TrialData_master.csv')
sb_pre <- read.table('./data/self_belief_pre_labels.csv', sep = ";", header = T) 
sb_post <- read.table('./data/self_belief_post_labels.csv', sep = ";", header = T)  

# check duplicates in data
sum(duplicated(data_meta))
sum(duplicated(sb_pre))
sum(duplicated(sb_post))

# check duplicates in subject number
sum(table(data_meta$subject)!=600)
sum(duplicated(sb_pre$Sid))
sum(duplicated(sb_post$Sid))
# both self belief scores have duplicates, delete them
sb_pre <- sb_pre[-which(duplicated(sb_pre$Sid), arr.ind = FALSE, useNames = TRUE), ]
sb_post <- sb_post[-which(duplicated(sb_post$Sid), arr.ind = FALSE, useNames = TRUE),]


# Exclude trials less than 50 ms or MAD -----------------------------------
# too fast RT
data_meta <- filter(data_meta, rt>0.05)

# MAD outliers
source("./code/remove_outliers.R")
data_meta$rt <- remove_outliers(data_meta$rt)
data_meta$rt_conf <- remove_outliers(data_meta$rt_conf)

# remove rows with NA
data_meta <- na.omit(data_meta)

# For T1 data -------------------------------------------------------------
# data wrangling
t1data <- data_meta[,c(1, 2, 6,8)]
colnames(t1data) <- c("subj", "mod", "acc", "rt")

# average the confidence scores over similar trials
t1l <- t1data %>%
  group_by(subj, mod)%>%
  summarise(mean(acc, na.rm = T), mean(rt, na.rm=T))
colnames(t1l) <- c("subj", "mod", "acc", "rt")

# put data in wide format
t1w <- t1l %>%
  pivot_wider(id_cols = subj, names_from = c(mod), values_from = c(acc, rt))
colnames(t1w) <- c("subj", "acc_cal", "acc_gdp", "acc_mem", "acc_vis", "rt_cal", "rt_gdp", "rt_mem", "rt_vis")
t1w <- t1w[,c(1, 4, 5, 3, 2, 8, 9, 7, 6)]

# calculate and add averages
t1w$avg_acc <- rowMeans(subset(t1w, select = c("acc_cal", "acc_gdp", "acc_mem", "acc_vis")))
t1w$avg_rt <- rowMeans(subset(t1w, select = c("rt_cal", "rt_gdp", "rt_mem", "rt_vis")))


# For self belief data ----------------------------------------------------
# select relevant data

# pre test data
pref <- as.data.frame(cbind(sb_pre$Sid, sb_pre$age, sb_pre$gender, sb_pre$self_memory_pre, sb_pre$self_visual_pre, sb_pre$self_gdp_pre, sb_pre$self_calories_pre))
colnames(pref) <- c("subj","age", "gender", "pre_mem", "pre_vis", "pre_gdp", "pre_cal")
pref[, c(1,2,4:7)] <- as.numeric(unlist(pref[, c(1,2,4:7)])) # because numbers were characters

# post test data
postf <- as.data.frame(cbind(sb_post$Sid, sb_post$self_memory_post, sb_post$self_visual_post, sb_post$self_gdp_post, sb_post$self_calories_post))
colnames(postf) <- c("subj", "post_mem", "post_vis", "post_gdp", "post_cal")
postf[, c(1:5)] <- as.numeric(unlist(postf[, c(1:5)])) # because numbers were integers

# merge the two sets
selfbel <- inner_join(pref, postf, by="subj")

# calculate and add additional measures
# average per modality
selfbel$mem_sb <- rowMeans(subset(selfbel, select = c("pre_mem", "post_mem")))
selfbel$vis_sb <- rowMeans(subset(selfbel, select = c("pre_vis", "post_vis")))
selfbel$gdp_sb <- rowMeans(subset(selfbel, select = c("pre_gdp", "post_gdp")))
selfbel$cal_sb <- rowMeans(subset(selfbel, select = c("pre_cal", "post_cal")))

# average per time
selfbel$pre_avg <- rowMeans(subset(selfbel, select = c("pre_mem", "pre_vis", "pre_gdp", "pre_cal")))
selfbel$post_avg <- rowMeans(subset(selfbel, select = c("post_mem", "post_vis", "post_gdp", "post_cal")))
selfbel$sb_diff_mem <- selfbel$post_mem - selfbel$pre_mem
selfbel$sb_diff_vis <- selfbel$post_vis - selfbel$pre_vis
selfbel$sb_diff_gdp <- selfbel$post_gdp - selfbel$pre_gdp
selfbel$sb_diff_cal <- selfbel$post_cal - selfbel$pre_cal
selfbel$sb_diff <- selfbel$post_avg - selfbel$pre_avg
selfbel$sb_avg <- rowMeans(subset(selfbel, select = c("pre_avg", "post_avg")))


# For trial confidence data -----------------------------------------------------------
# select relevant data
confdata <- data_meta[,c(1, 2, 6, 7)]
colnames(confdata) <- c("subj", "mod", "acc", "conf")

# average the confidence scores over similar trials
confw <- confdata %>%
  group_by(subj, mod, acc)%>%
  summarise(mean(conf, na.rm = T))
colnames(confw) <- c("subj", "mod", "acc", "conf")

# from long to wide format
conffinal <- confw %>%
  pivot_wider(id_cols = subj, names_from = c(mod, acc), values_from = conf)
colnames(conffinal) <- cbind("subj", "cal_0", "cal_1", "gdp_0", "gdp_1", "mem_0", "mem_1", "vis_0", "vis_1")
conffinal <- conffinal[,c(1, 7, 6, 9, 8, 5, 4, 3, 2)]

# add modality and accuracy averages --------------------------------------
# modality
conffinal$conf_mem <- rowMeans(subset(conffinal, select = c("mem_0", "mem_1")))
conffinal$conf_vis <- rowMeans(subset(conffinal, select = c("vis_0", "vis_1")))
conffinal$conf_gdp <- rowMeans(subset(conffinal, select = c("gdp_0", "gdp_1")))
conffinal$conf_cal <- rowMeans(subset(conffinal, select = c("cal_0", "cal_1")))

# hit minus miss
conffinal$conf_diff_mem <- conffinal$mem_1 - conffinal$mem_0
conffinal$conf_diff_vis <- conffinal$vis_1 - conffinal$vis_0
conffinal$conf_diff_gdp <- conffinal$gdp_1 - conffinal$gdp_0
conffinal$conf_diff_cal <- conffinal$cal_1 - conffinal$cal_0

# accuracy
conffinal$avg_0 <- rowMeans(subset(conffinal, select = c("mem_0", "vis_0", "gdp_0", "cal_0")))
conffinal$avg_1 <- rowMeans(subset(conffinal, select = c("mem_1", "vis_1", "gdp_1", "cal_1")))

# grand average
conffinal$avg_conf <- rowMeans(subset(conffinal[,10:13]))


# Combine the three sets ----------------------------------------------------
all_data <- left_join(selfbel, t1w, by = "subj")
all_data <- left_join(all_data, conffinal, by = "subj")

# delete non-binary person
all_data <- filter(all_data, gender!="non-binary")
# delete outlier person
all_data <- filter(all_data, subj!=214)
# delete na
all_data <- na.omit(all_data)

# check duplicates
sum(duplicated(all_data$subj))
# no duplicates 


# put data in long format -------------------------------------------------------------
all_long <- all_data
all_long <- gather(all_long, key="mod", value="measure", pre_mem:avg_conf)

# Save tables -------------------------------------------------------------

# save megatable as csv
write.csv(all_data, file = "./data/megatable.csv")
write.csv(all_long, file = "./data/megatable_long.csv")
