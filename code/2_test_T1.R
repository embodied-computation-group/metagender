# clear environment and load core tables ----------------------------------
# clear environment
rm(list = ls())

# megatable 
megatable <- read.csv('../data/megatable.csv')
megatable_long <- read.csv('../data/megatable_long.csv')

# t1
t1_data <- read.csv('../data/t1.csv')
t1_long <-read.csv('../data/t1_long.csv')

# remove useless first columns
megatable <- megatable[,-1]
megatable_long <- megatable_long[,-1]
t1_data <- t1_data[,-1]
t1_long <- t1_long[,-1]

# load packages
library(dplyr)
library(tidyr)
library(car)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(ggsignif)
library(gridExtra)
library(patchwork)
library(cowplot)
library(stringr)


# Split the tables --------------------------------------------------------
t1_rt <- t1_long %>%
  filter(str_detect(mod, "rt")) 
t1_rt <- t1_rt %>%
  filter(!str_detect(mod, "avg"))

t1_acc <- t1_long %>%
  filter(str_detect(mod, "acc"))
t1_acc <- t1_acc %>%
  filter(!str_detect(mod, "avg"))
colnames(t1_rt)[4] <- "rt"
colnames(t1_acc)[4] <- "acc"


# test assumptions --------------------------------------------------------
# shaprio test the values for each group

# for accuracy
# for rt
normrt <- t1_rt %>%
  group_by(gender, mod) %>%
  shapiro_test(rt)

normacc <- t1_acc %>%
  group_by(gender, mod) %>%
  shapiro_test(acc)

# any significant columns?
sum(normrt$p <= 0.05)
sum(normacc$p <= 0.05)
#yup 

# levene test for equal variance
# apply correct and relevant columns
levenert <- lapply(t1_data[c(7:10)], leveneTest, group = t1_data$gender)
leveneacc <- lapply(t1_data[c(3:6)], leveneTest, group = t1_data$gender)

# see outcomes
levenert #vision is significant (**)
leveneacc # memory is significant (**)


# get means ---------------------------------------------------------------
options(digits = 12)

rt_mean <- t1_rt %>%
  group_by(gender, mod) %>%
  summarise(mean(rt))
#View(rt_mean)
rt_sd <- t1_rt %>%
  group_by(gender, mod) %>%
  summarise(sd(rt))
#View(rt_sd)

acc_mean <- t1_acc %>%
  group_by(gender, mod) %>%
  summarise(mean(acc))
#View(acc_mean)
acc_sd <- t1_acc %>%
  group_by(gender, mod) %>%
  summarise(sd(acc))
#View(acc_sd)

# run anovas --------------------------------------------------------------
# rt
rt.aov <- anova_test(data = t1_rt, dv = rt, wid = subj, between = gender, within = mod)
get_anova_table(rt.aov) #modality effect

# acc
acc.aov <- anova_test(data = t1_acc, dv = acc, wid = subj, between = gender, within = mod)
get_anova_table(acc.aov) # all significant

# post hoc for modality effect rt
rt.one <- t1_rt %>%
  group_by(gender) %>%
  anova_test(dv = rt, wid = subj, within = mod) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
rt.one
# modality had an effect in men and women

# paired t-tests to see which modalities have different reaction times within a gender
pwcrt <- t1_rt %>%
  group_by(gender) %>%
  pairwise_t_test(rt ~ mod, p.adjust.method = "holm")
pwcrt

# now the post hoc for accuracy differences
acc.gen <- t1_acc %>%
  group_by(mod) %>%
  anova_test(dv = acc, wid = subj, between = gender) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
acc.gen

acc.mod <- t1_acc %>%
  group_by(gender) %>%
  anova_test(dv = acc, wid = subj, within = mod) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "holm")
acc.mod
# accuracy differences across modalities in both genders

# paired t-tests to see which modalities have different accuracy within a gender
pwcacc <- t1_acc %>%
  group_by(gender) %>%
  pairwise_t_test(acc ~ mod, p.adjust.method = "holm")
pwcacc

# plot plot plot ----------------------------------------------------------
# for reaction time
rtmem <- ggplot(t1_data, aes(x = gender, y = rt_mem, group = gender, fill=gender))+
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2, show.legend = FALSE) +
  labs(title="Memory", y="rt (s)", tag = "A") +
  ylim(0,3.5)+
  xlab(element_blank())+
  theme_cowplot()

rtvis <- ggplot(t1_data, aes(x = gender, y = rt_vis, group = gender, fill=gender))+
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2, show.legend = FALSE) +
  labs(title="Vision", y="rt (s)", tag = "B") +
  ylim(0,3.5)+
  xlab(element_blank())+
  theme_cowplot()

rtgdp <- ggplot(t1_data, aes(x = gender, y = rt_gdp, group = gender, fill=gender))+
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2, show.legend = FALSE) +
  labs(title="Trivia: GDP", y="rt (s)", tag = "C") +
  ylim(0,3.5)+
  xlab(element_blank())+
  theme_cowplot()

rtcal <- ggplot(t1_data, aes(x = gender, y = rt_cal, group = gender, fill=gender))+
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2, show.legend = FALSE) +
  labs(title="Trivia: calorie", y="rt (s)", tag = "D") +
  ylim(0,3.5)+
  xlab(element_blank())+
  theme_cowplot()

rtmem + rtvis + rtgdp + rtcal + plot_layout()

# for accuracy
accmem <- ggplot(t1_data, aes(x = gender, y = acc_mem, group = gender, fill=gender))+
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2, show.legend = FALSE) +
  labs(title="Memory", y="Accuracy", tag = "A") +
  ylim(0.5, 1.05) +
  xlab(element_blank())+
  theme_cowplot()+
  geom_signif(y_position = 0.98, xmin = 1, xmax = 2, annotation = c("*"), tip_length = 0.01, textsize = 5, size = 0.5, col = "black")

accvis <- ggplot(t1_data, aes(x = gender, y = acc_vis, group = gender, fill=gender))+
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2, show.legend = FALSE) +
  labs(title="Vision", y="Accuracy", tag = "B") +
  ylim(0.5, 1.05)+
  xlab(element_blank())+
  theme_cowplot()

accgdp <- ggplot(t1_data, aes(x = gender, y = acc_gdp, group = gender, fill=gender))+
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2, show.legend = FALSE) +
  labs(title="Trivia: GDP", y="Accuracy", tag = "C") +
  ylim(0.5, 1.05)+
  xlab(element_blank())+
  theme_cowplot()


acccal <- ggplot(t1_data, aes(x = gender, y = acc_cal, group = gender, fill=gender))+
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2, show.legend = FALSE) +
  labs(title="Trivia: calorie", y="Accuracy", tag = "D") +
  ylim(0.5, 1.05)+  
  xlab(element_blank())+
  theme_cowplot()

accmem + accvis + accgdp + acccal + plot_layout()

