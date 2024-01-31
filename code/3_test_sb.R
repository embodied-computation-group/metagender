# test for sb between modalities------------------------------------------------------------------

# clear environment and load core tables ----------------------------------
# clear environment
rm(list = ls())

# megatable 
megatable <- read.csv('../data/megatable.csv')
megatable_long <- read.csv('../data/megatable_long.csv')

# self belief
sb_data <- read.csv('../data/self_belief.csv')
sb_long <- read.csv('../data/self_belief_long.csv')

# remove useless first columns
megatable <- megatable[,-1]
megatable_long <- megatable_long[,-1]
sb_data <- sb_data[,-1]
sb_long <- sb_long[,-1]

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


# make modality specific long table ---------------------------------------
sb_avg <- sb_long %>%
  filter(str_detect(mod, "mem_|vis_|gdp_|cal_")) 
sb_time <- sb_long %>%
  filter(str_detect(mod, "_mem|_vis|_gdp|_cal"))

# test for normality of data distribution ----------------------------------------------------
# shaprio test the values for each group
# average values
normavg <- sb_avg %>%
  group_by(mod,gender) %>%
  shapiro_test(conf)

# time dependent
normtime <- sb_time %>%
  group_by(mod,gender) %>%
  shapiro_test(conf)

# any significant columns?
sum(normavg$p <= 0.05)
sum(normtime$p <= 0.05)
# both tests have significant groups


# test data for equal variance in standard deviation ---------------------------
# apply levene test to relevant columns
leveneavg <- lapply(sb_data[c(11:14)], leveneTest, group = sb_data$gender)
levenetime <- lapply(sb_data[c(3:10)], leveneTest, group = sb_data$gender)

# see results
leveneavg
levenetime #post_cal is significant


# calculate means ---------------------------------------------------------
avg_mean <- sb_avg %>%
  group_by(gender, mod) %>%
  summarise(mean(conf))
#View(avg_mean)
sb_mean <- sb_time %>%
  group_by(gender, mod) %>%
  summarise(mean(conf))
#View(sb_mean)

avg_sd <- sb_avg %>%
  group_by(gender, mod) %>%
  summarise(sd(conf))
#View(avg_sd)
sb_sd <- sb_time %>%
  group_by(gender, mod) %>%
  summarise(sd(conf))
#View(sb_sd)

# run anovas ---------------------------------------------------------------
# assumption of sphericity is automatically tested

# test avg self belief
avg.aov <- anova_test(data = sb_avg, dv = conf, wid = subj, between = gender, within = mod)
get_anova_table(avg.aov)
# all effects are significant (***)

# remake tables to be modality specific
sb_mem <- sb_time %>%
  filter(str_detect(mod, "mem"))
sb_vis <- sb_time %>%
  filter(str_detect(mod, "vis"))
sb_gdp <- sb_time %>%
  filter(str_detect(mod, "gdp"))
sb_cal <- sb_time %>%
  filter(str_detect(mod, "cal"))

# run anovas
# mem
mem.aov <- anova_test(data = sb_mem, dv = conf, wid = subj, between = gender, within = mod)
# vis
vis.aov <- anova_test(data = sb_vis, dv = conf, wid = subj, between = gender, within = mod)
# gdp
gdp.aov <- anova_test(data = sb_gdp, dv = conf, wid = subj, between = gender, within = mod)
# cal
cal.aov <- anova_test(data = sb_cal, dv = conf, wid = subj, between = gender, within = mod)

# see results
get_anova_table(mem.aov) #time effect
get_anova_table(vis.aov) #all effects are significant
get_anova_table(gdp.aov) #all effects are significant
get_anova_table(cal.aov) #gender and time effect


# post hoc testing of specific differences --------------------------------

# averages
avg.gen <- sb_avg %>%
  group_by(mod) %>%
  anova_test(dv = conf, wid = subj, between = gender) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
avg.gen #effect of gender on vis (***) and gdp (***)

avg.time <- sb_avg %>%
  group_by(gender) %>%
  anova_test(dv = conf, wid = subj, within = mod) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
avg.time #effect of time in women (***) and men (*)

pwcavg <- sb_avg %>%
  group_by(gender) %>%
  pairwise_t_test(conf ~ mod, p.adjust.method = "holm")
pwcavg

# time specific comparisons
# memory
mem.time <- sb_mem %>%
  group_by(gender) %>%
  anova_test(dv = conf, wid = subj, within = mod) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
mem.time #effect of time in women and men (***)

# vision
vis.gen <- sb_vis %>%
  group_by(mod) %>%
  anova_test(dv = conf, wid = subj, between = gender) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
vis.gen #effect of gender on post (***)

vis.time <- sb_vis %>%
  group_by(gender) %>%
  anova_test(dv = conf, wid = subj, within = mod) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
vis.time #effect of time in women and men (***)

# gdp
gdp.gen <- sb_gdp %>%
  group_by(mod) %>%
  anova_test(dv = conf, wid = subj, between = gender) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
gdp.gen #effect of gender on pre and post (***)

gdp.time <- sb_gdp %>%
  group_by(gender) %>%
  anova_test(dv = conf, wid = subj, within = mod) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
gdp.time #effect of time in women and men (***)

# calorie
cal.gen <- sb_cal %>%
  group_by(mod) %>%
  anova_test(dv = conf, wid = subj, between = gender) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
cal.gen #effect of gender on post (*)

cal.time <- sb_cal %>%
  group_by(gender) %>%
  anova_test(dv = conf, wid = subj, within = mod) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
cal.time #effect of time in women (**)

# duh

# visualize results -------------------------------------------------------

# for the average self belief scores
# make means table
sb_mean <- sb_avg %>%
  group_by(gender, mod) %>%
  summarise(conf = mean(conf)) %>%
  ungroup()

# plot -------------------- MAKE GEOM SIGNIF WORK HERE
avg_plot <- sb_avg %>% 
ggplot(aes(x=mod, y = conf, fill = gender)) +
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2, show.legend = F) +
  scale_x_discrete(limits = c("mem_sb", "vis_sb", "gdp_sb", "cal_sb"))+
  labs(title="Average self belief per modality", y="self belief") +
  xlab(element_blank()) +
  ylim(0,100) + 
  theme_cowplot() +
  geom_line(data = sb_mean, aes(group = gender, col = gender),  show.legend = F)+
geom_signif(y_position = 95, xmin = c(1.8, 2.8), xmax = c(2.2, 3.2), annotation = c("***", "***"), tip_length = 0.01, textsize = 7, size = 10, col = "black")
  

# interaction per modality plot -------------------------------------------
# make mean tables for lines
mem_mean <- sb_mem %>%
  group_by(gender, mod) %>%
  summarise(conf = mean(conf)) %>%
  ungroup()
vis_mean <- sb_vis %>%
  group_by(gender, mod) %>%
  summarise(conf = mean(conf)) %>%
  ungroup()
gdp_mean <- sb_gdp %>%
  group_by(gender, mod) %>%
  summarise(conf = mean(conf)) %>%
  ungroup()
cal_mean <- sb_cal %>%
  group_by(gender, mod) %>%
  summarise(conf = mean(conf)) %>%
  ungroup()

intmem <- sb_mem %>%
  ggplot(aes(x = mod, y = conf, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2) +
  scale_x_discrete(limits = c("pre_mem", "post_mem")) +
  labs(title="Memory", y="self belief") +
  xlab(element_blank()) +
  ylim(0,100) + 
  theme_cowplot() +
  geom_line(data = mem_mean, aes(group = gender, col = gender)) 

intvis <- sb_vis %>%
  ggplot(aes(x = mod, y = conf, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2) +
  scale_x_discrete(limits = c("pre_vis", "post_vis")) +
  labs(title="Vision", y="self belief") +
  xlab(element_blank()) +
  ylim(0,100) + 
  theme_cowplot() +
  geom_line(data = vis_mean, aes(group = gender, col = gender)) 

intgdp <- sb_gdp %>%
  ggplot(aes(x = mod, y = conf, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2) +
  scale_x_discrete(limits = c("pre_gdp", "post_gdp")) +
  labs(title="GDP", y="self belief") +
  xlab(element_blank()) +
  ylim(0,100) + 
  theme_cowplot() +
  geom_line(data = gdp_mean, aes(group = gender, col = gender)) 

intcal <- sb_cal %>%
  ggplot(aes(x = mod, y = conf, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2) +
  scale_x_discrete(limits = c("pre_cal", "post_cal")) +
  labs(title="Calories", y="self belief") +
  xlab(element_blank()) +
  ylim(0,100) + 
  theme_cowplot() +
  geom_line(data = cal_mean, aes(group = gender, col = gender)) 

intplots <- intmem + intvis + intgdp + intcal + plot_layout(guides = "collect")


# combine the two plots ---------------------------------------------------
avg_plot + intplots + plot_layout()

