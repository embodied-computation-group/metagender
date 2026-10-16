# test differences between hit or miss ----------------------------------
# clear environment and load core tables ----------------------------------
# clear environment
rm(list = ls())

# megatable 
megatable <- read.csv('../data/megatable.csv')
megatable_long <- read.csv('../data/megatable_long.csv')

# trialwise confidence
conf_data <- read.csv('../data/trial_confidence.csv')
conf_long <- read.csv('../data/trial_confidence_long.csv')

# remove useless first columns
megatable <- megatable[,-1]
megatable_long <- megatable_long[,-1]
conf_data <- conf_data[,-1]
conf_long  <- conf_long[,-1]

# load packages
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

# subset modality specific table ------------------------------------------
conf_mod <- conf_long %>%
  filter(!str_detect(mod, "avg"))

# test for normality of data distribution ----------------------------------------------------
# shaprio test the values for each group
norm <- conf_mod %>%
  group_by(mod,gender) %>%
  shapiro_test(conf)

# any significant columns?
sum(norm$p <= 0.05)
# significant groups 


# test data for equal variance in standard deviation ---------------------------
# apply levene test to relevant columns
leveneconf <- lapply(conf_data[c(3:10)], leveneTest, group = conf_data$gender)

# check results
leveneconf #mem_0 is significant


# calculate averages ------------------------------------------------------
mod_mean <- conf_mod %>%
  group_by(gender, mod) %>%
  summarise(mean(conf))
#View(mod_mean)

mod_sd <- conf_mod %>%
  group_by(gender, mod) %>%
  summarise(sd(conf))

# run anovas -------------------------------------------
# seperate modalities
conf_mem <- conf_mod %>%
  filter(str_detect(mod, "mem"))
conf_vis <- conf_mod %>%
  filter(str_detect(mod, "vis"))
conf_gdp <- conf_mod %>%
  filter(str_detect(mod, "gdp"))
conf_cal <- conf_mod %>%
  filter(str_detect(mod, "cal"))

# run anovas
# memory
mem.aov <- anova_test(data = conf_mem, dv = conf, wid = subj, between = gender, within = mod)
#vision
vis.aov <- anova_test(data = conf_vis, dv = conf, wid = subj, between = gender, within = mod)
#gdp
gdp.aov <- anova_test(data = conf_gdp, dv = conf, wid = subj, between = gender, within = mod)
#cal
cal.aov <- anova_test(data = conf_cal, dv = conf, wid = subj, between = gender, within = mod)

# get results
get_anova_table(mem.aov) #all effects are significant
get_anova_table(vis.aov) #effect of gender and accuracy
get_anova_table(gdp.aov) #effect of gender and accuracy
get_anova_table(cal.aov) #effect of accuracy and interaction effect


# run post-hocs -----------------------------------------------------------

# memory
mem.gen <- conf_mem %>%
  group_by(mod) %>%
  anova_test(dv = conf, wid = subj, between = gender) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
mem.gen #effect of gender on 0 (**)

mem.acc <- conf_mem %>%
  group_by(gender) %>%
  anova_test(dv = conf, wid = subj, within = mod) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
mem.acc #effect of time in women and men (***)

# vision
vis.gen <- conf_vis %>%
  group_by(mod) %>%
  anova_test(dv = conf, wid = subj, between = gender) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
vis.gen #effect of gender on 0 and 1 (*)

vis.acc <- conf_vis %>%
  group_by(gender) %>%
  anova_test(dv = conf, wid = subj, within = mod) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
vis.acc #effect of time in women and men (***)

# gdp
gdp.gen <- conf_gdp %>%
  group_by(mod) %>%
  anova_test(dv = conf, wid = subj, between = gender) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
gdp.gen #effect of gender on 1 an 0 (***)

gdp.acc <- conf_gdp %>%
  group_by(gender) %>%
  anova_test(dv = conf, wid = subj, within = mod) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
gdp.acc #effect of time in women and men (***)

# calorie
cal.acc <- conf_cal %>%
  group_by(gender) %>%
  anova_test(dv = conf, wid = subj, within = mod) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
cal.acc #effect of time in women and men (***)



# plot plot ploooooot -----------------------------------------------------
# first calculate the means for lines
mem_mean <- conf_mem %>%
  group_by(gender, mod) %>%
  summarise(conf = mean(conf)) %>%
  ungroup()
vis_mean <- conf_vis %>%
  group_by(gender, mod) %>%
  summarise(conf = mean(conf)) %>%
  ungroup()
gdp_mean <- conf_gdp %>%
  group_by(gender, mod) %>%
  summarise(conf = mean(conf)) %>%
  ungroup()
cal_mean <- conf_cal %>%
  group_by(gender, mod) %>%
  summarise(conf = mean(conf)) %>%
  ungroup()


# plot data with interaction effect
memacc <- conf_mem %>%
  ggplot(aes(x=mod, y=conf, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2) +
  scale_x_discrete(limits = c("mem_1", "mem_0")) +
  xlab(element_blank()) +
  ylim(0, 7.5) +
  labs(title="Memory confidence by accuracy", y="Confidence", x = "Accuracy") +
  theme_cowplot() + 
  geom_line(data = mem_mean, aes(group = gender, col = gender)) 

visacc <- conf_vis %>%
  ggplot(aes(x=mod, y=conf, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2) +
  scale_x_discrete(limits = c("vis_1", "vis_0")) +
  xlab(element_blank()) +
  ylim(0, 7.5) +
  labs(title="Vision confidence by accuracy", y="Confidence", x = "Accuracy") +
  theme_cowplot() + 
  geom_line(data = vis_mean, aes(group = gender, col = gender))

gdpacc <- conf_gdp %>%
  ggplot(aes(x=mod, y=conf, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2) +
  scale_x_discrete(limits = c("gdp_1", "gdp_0")) +
  xlab(element_blank()) +
  ylim(0, 7.5) +
  labs(title="GDP confidence by accuracy", y="Confidence", x = "Accuracy") +
  theme_cowplot() + 
  geom_line(data = gdp_mean, aes(group = gender, col = gender))

calacc <- conf_cal %>%
  ggplot(aes(x=mod, y=conf, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2) +
  scale_x_discrete(limits = c("cal_1", "cal_0")) +
  xlab(element_blank()) +
  ylim(0, 7.5) +
  labs(title="Calories confidence by accuracy", y="Confidence", x = "Accuracy") +
  theme_cowplot() + 
  geom_line(data = cal_mean, aes(group = gender, col = gender))

memacc + visacc + gdpacc + calacc + plot_layout(guides = "collect")
