rm(list = ls())

library(tidyr)
library(dplyr)
library(ggplot2)

# load data

megatable <- read.csv('../data/megatable.csv')
megatable <- megatable[,-1]
megatable <- megatable[,c(1:52)]
mratio <- read.csv('../metad/Matlab/mratio_data.csv')

# merge

mratable <- inner_join(megatable, mratio[,c(1,3:7)], by = "subj")
write.csv(mratable, file = "../data/megatable.csv")

mratable$conf_diff_tot <- rowMeans(subset(mratable, select = c("conf_diff_mem", "conf_diff_vis", "conf_diff_gdp", "conf_diff_cal")))

mratable %>%
  ggplot(aes(x=Mmem, y=conf_diff_mem, col = gender)) +
  geom_point()+
  geom_smooth(method = lm)

mratable %>%
  ggplot(aes(x=Mvis, y=conf_diff_vis, col = gender)) +
  geom_point()+
  geom_smooth(method = lm)

mratable %>%
  ggplot(aes(x=Mgdp, y=conf_diff_gdp, col = gender)) +
  geom_point()+
  geom_smooth(method = lm)

mratable %>%
  ggplot(aes(x=Mcal, y=conf_diff_cal, col = gender)) +
  geom_point()+
  geom_smooth(method = lm)

mratable %>%
  ggplot(aes(x=Mtot, y=conf_diff_tot, col = gender)) +
  geom_point()+
  geom_smooth(method = lm)

# testing the model
ratio <- as.data.frame(cbind(mratable$subj, mratable$gender, mratable$Mmem, mratable$Mvis, mratable$Mgdp, mratable$Mcal))
ratio[,c(1, 3:6)] <- sapply(ratio[,c(1, 3:6)], as.numeric)
conf_diffs <- as.data.frame(cbind(mratable$subj, mratable$conf_diff_mem, mratable$conf_diff_mem, mratable$conf_diff_mem, mratable$conf_diff_mem))
colnames(ratio) <- cbind("subj", "gender", "mem", "vis", "gdp", "cal")
colnames(conf_diffs) <- cbind("subj", "mem", "vis", "gdp", "cal")

ratio_long <- ratio
ratio_long <- gather(ratio_long, key="mod", value="mratio", mem:cal)

es_long <- conf_diffs
es_long <- gather(es_long, key="mod", value="es", mem:cal)

reg_dat <- inner_join(ratio_long, es_long, by = c("subj", "mod"))


model <- lm(mratio ~ gender + mod + es, data = reg_dat)
summary(model)

modelm <- lm(Mmem ~ gender + conf_diff_mem, data = mratable)
summary(modelm)

modelv <- lm(Mvis ~ gender + conf_diff_vis, data = mratable)
summary(modelv)

modelg <- lm(Mgdp ~ gender + conf_diff_gdp, data = mratable)
summary(modelg)

modelc <- lm(Mcal ~ gender + conf_diff_cal, data = mratable)
summary(modelc)

