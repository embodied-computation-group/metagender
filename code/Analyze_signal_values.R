# clear environment
rm(list = ls())

library(tidyr)
library(dplyr)
library(rstatix)
library(ggplot2)
library(nlme)
library(bayestestR)
library(rstanarm)

# load data ---------------------------------------------------------------
data_meta <- read.csv('./data/metacognition_TrialData_master.csv')
all_data <- read.table('./data/megatable.csv', sep = ",", header = T) 
all_data$Gender <- as.factor(all_data$Gender)
# Dataframe for staircasing -----------------------------------------------
# select relevant data for memory task only
signal <- data_meta[,c(1, 2, 7, 10)]
colnames(signal) <- c("subj", "mod", "conf", "level")
signal_mem <- signal[signal$mod == "memory", ]

# average the confidence scores over the same step
signal_mem_sum <- signal_mem %>%
  group_by(subj, level)%>%
  summarise(mean(conf, na.rm = T))
colnames(signal_mem_sum) <- c("Subject", "Level", "Confidence")

# merge in gender data
signal_memory <- left_join(all_data[,c(1,3)], signal_mem_sum, by = "Subject")
signal_memory$Level <- as.factor(signal_memory$Level)
signal_memory <- na.omit(signal_memory)

# Data for staircased tasks -----------------------------------------------
colnames(signal) <- c("Subject", "Domain", "Confidence", "Level")
signal_stair <- left_join(all_data[,c(1,3)], signal, by = "Subject")
signal_stair$Level <- abs(signal_stair$Level)

# Vision
signal_vision <- signal_stair[signal_stair$Domain == "vision", ]
signal_vision <- na.omit(signal_vision)
# GDP
signal_gdp <- signal_stair[signal_stair$Domain == "GDP", ]
signal_gdp <- na.omit(signal_gdp)
# Calories
signal_calories <- signal_stair[signal_stair$Domain == "Calories", ]
signal_calories <- na.omit(signal_calories)

# Plot all tasks ----------------------------------------------------------
# Memory
ggplot(data = signal_memory, aes(x = Level, y = Confidence)) + 
  geom_boxplot(aes(fill = Gender))

# Vision
ggplot(data = signal_vision, aes(x = Level, y = Confidence)) +
  geom_point(aes(color = Gender)) +
  geom_smooth(aes(color = Gender, fill = Gender), method = "lm")

# GDP
ggplot(data = signal_gdp, aes(x = Level, y = Confidence)) +
  geom_point(aes(color = Gender)) +
  geom_smooth(aes(color = Gender, fill = Gender), method = "lm")

# Calories
ggplot(data = signal_calories, aes(x = Level, y = Confidence)) +
  geom_point(aes(color = Gender)) +
  geom_smooth(aes(color = Gender, fill = Gender), method = "lm")


# Statistical Testing -----------------------------------------------------
# memory
model_m <- lme(Confidence ~ Gender * Level, random=~1|Subject, data=signal_memory)
anova(model_m)

# vision
model_v <- lm(Confidence ~ Gender * Level, data = signal_vision)
summary(model_v)

bayes_v <- stan_glm(Confidence~Gender * Level, data=signal_vision, seed=111)
print(bayes_v, digits = 3)
describe_posterior(bayes_v)


# GDP
model_g <- lm(Confidence ~ Gender * Level, data = signal_gdp)
summary(model_g)

bayes_g <- stan_glm(Confidence~Gender * Level, data=signal_gdp, seed=111)
print(bayes_g, digits = 3)
describe_posterior(bayes_g)

# calories
model_c <- lm(Confidence ~ Gender * Level, data = signal_calories)
summary(model_c)

bayes_c <- stan_glm(Confidence~Gender * Level, data=signal_calories, seed=111)
print(bayes_c, digits = 3)
describe_posterior(bayes_c)

write.csv(signal_data, file = "./data/signal_data.csv", row.names = F)
