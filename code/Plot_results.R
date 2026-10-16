# make plots for paper ------------------------------------
rm(list = ls())

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
library(ggdist)
library(ggthemes)
library(ggpubr)
library(ggpol)
library(ggridges)
library(see)

# load data
meta <- read.csv('../data/megatable.csv')
meta_long <- read.csv('../data/megatable_long.csv')

# T1 plots ----------------------------------------------------------------
# reaction time
rtmem <- ggplot(meta, aes(x = gender, y = rt_mem, group = gender, fill=gender))+
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2, show.legend = FALSE, fill = c("white", "gray40")) +
  labs(title="Memory", y="rt (s)", tag = "A") +
  scale_x_discrete(limits = c("Feminin", "Masculin"), labels = c("Female", "Male"))+
  ylim(0,3.5)+
  xlab(element_blank())+
  theme_cowplot() +
  theme(text = element_text(family = "serif"), plot.title = element_text(hjust = 0.5)) 

rtvis <- ggplot(meta, aes(x = gender, y = rt_vis, group = gender, fill=gender))+
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2, show.legend = FALSE, fill = c("white", "gray40")) +
  labs(title="Vision", y="rt (s)", tag = "B") +
  scale_x_discrete(limits = c("Feminin", "Masculin"), labels = c("Female", "Male"))+
  ylim(0,3.5)+
  xlab(element_blank())+
  theme_cowplot()+
  geom_signif(y_position = 2.5, xmin = 1, xmax = 2, annotation = c("*"), tip_length = 0.03, textsize = 5, size = 0.5, col = "black") +
  theme(text = element_text(family = "serif"), plot.title = element_text(hjust = 0.5)) 

rtgdp <- ggplot(meta, aes(x = gender, y = rt_gdp, group = gender, fill=gender))+
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2, show.legend = FALSE, fill = c("white", "gray40")) +
  labs(title="Trivia: GDP", y="rt (s)", tag = "C") +
  scale_x_discrete(limits = c("Feminin", "Masculin"), labels = c("Female", "Male"))+
  ylim(0,3.5)+
  xlab(element_blank())+
  theme_cowplot()+  geom_signif(y_position = 3, xmin = 1, xmax = 2, annotation = c("*"), tip_length = 0.03, textsize = 5, size = 0.5, col = "black") +
  theme(text = element_text(family = "serif"), plot.title = element_text(hjust = 0.5)) 

rtcal <- ggplot(meta, aes(x = gender, y = rt_cal, group = gender, fill=gender))+
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2, show.legend = FALSE, fill = c("white", "gray40")) +
  labs(title="Trivia: calories", y="rt (s)", tag = "D") +
  scale_x_discrete(limits = c("Feminin", "Masculin"), labels = c("Female", "Male"))+
  ylim(0,3.5)+
  xlab(element_blank())+
  theme_cowplot()+
  theme(text = element_text(family = "serif"), plot.title = element_text(hjust = 0.5)) 

rtmem + rtvis + rtgdp + rtcal + plot_layout(guides = 'auto')

# for accuracy
accmem <- ggplot(meta, aes(x = gender, y = acc_mem, group = gender, fill=gender))+
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2, show.legend = FALSE, fill = c("white", "gray40")) +
  labs(title="Memory", y="Accuracy", tag = "A") + 
  scale_x_discrete(limits = c("Feminin", "Masculin"), labels = c("Female", "Male"))+
  ylim(0.5, 1.05) +
  xlab(element_blank())+
  theme_cowplot()+
  theme(text = element_text(family = "serif"), plot.title = element_text(hjust = 0.5)) 

accvis <- ggplot(meta, aes(x = gender, y = acc_vis, group = gender, fill=gender))+
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2, show.legend = FALSE, fill = c("white", "gray40")) +
  labs(title="Vision", y="Accuracy", tag = "B") +
  scale_x_discrete(limits = c("Feminin", "Masculin"), labels = c("Female", "Male"))+
  ylim(0.5, 1.05)+
  xlab(element_blank())+
  theme_cowplot()+
  theme(text = element_text(family = "serif"), plot.title = element_text(hjust = 0.5)) 

accgdp <- ggplot(meta, aes(x = gender, y = acc_gdp, group = gender, fill=gender))+
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2, show.legend = FALSE, fill = c("white", "gray40")) +
  labs(title="Trivia: GDP", y="Accuracy", tag = "C") +
  scale_x_discrete(limits = c("Feminin", "Masculin"), labels = c("Female", "Male"))+
  ylim(0.5, 1.05)+
  xlab(element_blank())+
  theme_cowplot()+  
  theme(text = element_text(family = "serif"), plot.title = element_text(hjust = 0.5)) 

acccal <- ggplot(meta, aes(x = gender, y = acc_cal, group = gender, fill=gender))+
  geom_boxplot(notch = T, outlier.shape = NA, width = 0.2, show.legend = FALSE, fill = c("white", "gray40")) +
  labs(title="Trivia: calories", y="Accuracy", tag = "D") +
  scale_x_discrete(limits = c("Feminin", "Masculin"), labels = c("Female", "Male"))+
  ylim(0.5, 1.05)+  
  xlab(element_blank())+
  theme_cowplot()+  
  geom_signif(y_position = 0.98, xmin = 1, xmax = 2, annotation = c("*"), tip_length = 0.03, textsize = 5, size = 0.5, col = "black") +
  theme(text = element_text(family = "serif"), plot.title = element_text(hjust = 0.5)) 

accmem + accvis + accgdp + acccal + plot_layout()


# Self belief data --------------------------------------------------------
# pre post interaction
sb_meta <- meta_long %>%
  filter(str_detect(mod, "pre_a|post_a")) %>%
  group_by(gender, mod) %>%
  summarise(msb = mean(measure))  
sb_meta2 <- meta_long %>%
  filter(str_detect(mod, "pre_a|post_a")) %>%
  group_by(gender, mod) %>%
  summarise(se = std.error(measure))  
sb_meta <- cbind(sb_meta, sb_meta2$se)
colnames(sb_meta)[4] <- "se"
sb_viol <- meta_long %>%
  filter(str_detect(mod, "pre_a|post_a")) %>%
  group_by(gender, mod)
femraw <- sb_viol%>%
  filter(str_detect(gender, "Feminin")) %>%
  group_by(gender, mod)

#femraw <- rbind(femraw[,C(4:6)], )
masraw <- sb_viol%>%
  filter(str_detect(gender, "Masculin")) %>%
  group_by(gender, mod)

text_label = expression(F[Gender] * "(2,256) = 29, p < 0.001")
ggplot(data = sb_viol, aes(x = mod, y = measure))+
  geom_half_violin(data = femraw, aes(x = mod, y = measure, color = gender), "side" = "l", linewidth = 1.25, alpha = .01) +
  geom_half_violin(data = masraw, aes(x = mod, y = measure, color = gender), "side" = "l", linewidth = 1.25, alpha = .01) +
  geom_half_boxplot(data = femraw, aes(x = mod, y = measure, fill = gender), "side" = "l", alpha = .5, width = 0.20, notch = TRUE, outlier.shape = NA,linewidth = 1.25) +
  geom_half_boxplot(data = masraw, aes(x = mod, y = measure, fill = gender), "side" = "r", alpha = .5, width = 0.20, notch = TRUE, outlier.shape = NA, linewidth = 1.25) +
  geom_label(x = 1.5, y = Inf, label = text_label, 
               vjust = "inward", hjust = "inward", 
               color = "black", fill = "white", label.size = 1.5) +
  scale_x_discrete(name = "Time", limits = c("pre_avg", "post_avg"), labels = c("Pre-task", "Post-task"))+
  scale_fill_manual(name = "Gender", labels = c("Female", "Male"), values = c("#b9e4c9", "#a2cffe"))+
  scale_color_manual(name = "Gender", labels = c("Female", "Male"), values = c("#b9e4c9", "#a2cffe"))+
  labs(title="Self-belief before and after task", y="Self-belief", tag = "A") +
  theme_high_impact()

# Calculate the difference scores without pivoting to wide format
sb_viol_diff <- sb_viol %>%
  group_by(subj) %>%
  summarize(
    age = first(age),  # Assuming age remains constant for each subject
    gender = first(gender),  # Assuming gender remains constant for each subject
    pre_avg = sum(measure[mod == "pre_avg"]),
    post_avg = sum(measure[mod == "post_avg"]),
    diff_score = post_avg - pre_avg
  )

ggplot(data = sb_viol_diff, aes (x = gender, y = diff_score))+
  geom_boxplot(notch= TRUE)


ggplot(sb_viol, aes(x = mod, y = measure, group = subj)) +
  geom_point() +
  geom_line(color = "gray") +
  facet_wrap(~ gender) +  # Faceting by gender
  labs(title = "Pre and Post Measures for Each Participant, by Gender",
       x = "Measurement Type",
       y = "Measure Value",
       color = "Participant") +
  theme_minimal()


# Calculate the difference for each participant
diff_data <- sb_viol %>%
  group_by(subj) %>%
  summarize(
    diff = measure[mod == "post_avg"] - measure[mod == "pre_avg"]
  )

# Merge the difference back into the original data frame
sb_viol <- sb_viol %>%
  left_join(diff_data, by = "subj")

# Create the plot
ggplot(sb_viol, aes(x = mod, y = measure, group = subj)) +
  geom_line(aes(color = diff), position = position_dodge(width = 0.75)) +
  geom_point(aes(color = as.factor(subj)), position = position_dodge(width = 0.75)) +
  geom_boxplot(aes(fill = mod), outlier.shape = NA) +  # Hide outliers to avoid overplotting
  facet_wrap(~ gender) +  # Separate facets for each gender
  scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Individual Pre and Post Measures by Gender",
       x = "Condition",
       y = "Value",
       color = "Difference",
       fill = "Condition") +
  theme(legend.position = "bottom")



# T-test on the difference score between gender groups
t_test_result <- t.test(diff_score ~ gender, data = sb_viol_diff)

# Display the results
print(t_test_result)


sbtime <- 
  sb_meta %>%
  ggplot(aes(x = mod, y = msb, group = interaction(gender, mod), shape=gender),)+
  geom_line(aes(group = gender), col = "gray30") +
 # geom_dotplot(data = sb_viol, aes(x = mod, y = measure, fill = gender,  alpha = 0.5), binaxis = "y", binwidth = 5, method="histodot", stackdir="up", stackgroups =F, position=position_dodge(width=0.2), dotsize =0.2)+ 
  geom_dotplot(data = femraw, aes(x = mod, y = measure), alpha=0.6 , fill = "white", binaxis = "y", binwidth = 5, method="histodot", stackdir="down", position=position_dodge(width=0.2), dotsize =0.2)+ 
  geom_dotplot(data = masraw, aes(x = mod, y = measure), alpha=0.6 , fill = "gray30", binaxis = "y", binwidth = 5, method="histodot", stackdir="up", position=position_dodge(width=0.2), dotsize =0.2)+ 
  geom_errorbar(aes(ymin= msb-se, ymax=msb+se), width=0.05, col = "gray30") +
  geom_point(size = 2, col = "gray30", fill = 'white', stroke = 0.7) +
  scale_shape_manual(values = c(21, 19), name = "Gender", labels = c("Female", "Male"))+
  scale_x_discrete(limits = c("pre_avg", "post_avg"), labels = c("Pre-task", "Post-task"))+
  labs(title="Self-belief before and after task", y="Self-belief ± SEM", tag = "A") +
  ylim(25, 100)+
  xlab(element_blank())+
  theme_cowplot()+
  theme(text = element_text(family = "serif"), plot.title = element_text(hjust = 0.5, size = 20))

  sb_viol %>%
    ggplot(aes(x = mod, y = measure, fill = gender, shape = gender))+
    annotate("segment", x = 0.75, xend = 1.7, y =65 , yend = 53.75) +
    annotate("segment", x = 1.18, xend = 2.1, y =68.75 , yend = 63.75) +
    geom_boxjitter(notch =  TRUE, outlier.shape = NA, width = 0.5, position = position_dodge(0.8), jitter.shape = c(21))+
    scale_fill_manual(name = "Gender", labels = c("Female", "Male"), values = c("white", "gray40"))+
    scale_x_discrete(limits = c("pre_avg", "post_avg"), labels = c("Pre-task", "Post-task"))+
    labs(title="Self-belief before and after task", y="Self-belief", tag = "A") +
    ylim(20, 100)+
    xlab(element_blank())+
    theme_cowplot()+
    theme(text = element_text(family = "serif"), plot.title = element_text(hjust = 0.5, size = 20))
  
# updating per modality
sb_dom <- meta_long %>%
  filter(str_detect(mod, "mem_sb|vis_sb|gdp_sb|cal_sb")) %>%
  group_by(gender, mod) %>%
  summarise(md = mean(measure))  
sb_dom2 <- meta_long %>%
  filter(str_detect(mod, "mem_sb|vis_sb|gdp_sb|cal_sb")) %>%
  group_by(gender, mod) %>%
  summarise(se = std.error(measure))  
sb_dom <- cbind(sb_dom, sb_dom2$se)
colnames(sb_dom)[4] <- "sed"

mods <-c("mem_sb", "vis_sb", "gdp_sb", "cal_sb")
sb_dom <- sb_dom[order(factor(sb_dom$mod, levels = mods)),]


sbdiff <- sb_dom %>%
 ggplot(aes(x = mod, y = md, group = interaction(gender, mod), shape=gender))+
  geom_errorbar(aes(ymin=md-sed, ymax=md+sed), width=.2,position=position_dodge(width=0.2), col = "gray30") +
  geom_line(aes(group = gender), col = "gray30", position=position_dodge(width=0.2)) +
  geom_point(aes(y= md, group = interaction(gender,mod), shape = gender),size = 4, position=position_dodge(width=0.2), col = "gray30", fill = 'white', stroke = 0.7,show.legend=F) +
  scale_shape_manual(values = c(21, 19), name = "Gender", labels = c("Female", "Male"))+
  scale_x_discrete(limits = c("mem_sb", "vis_sb", "gdp_sb", "cal_sb"), labels = c("Memory", "Vision", "GDP", "Calories"))+
  labs(title="Average self-belief per domain", y="Self-belief ± SEM", tag = "B") +
  ylim(44, 75)+
  xlab(element_blank())+
  theme_cowplot()+  
  geom_signif(y_position = 70, xmin = c(1.9, 2.9), xmax = c(2.1, 3.1), annotation = c("*", "***"), tip_length = 0.03, textsize = 5, size = 0.5, col = "black") +
  theme(text = element_text(family = "serif"), plot.title = element_text(hjust = 0.5, size = 20), legend.position = "none") 

sbtime + sbdiff + plot_layout(guides = 'collect',widths = c(1.8, 1)) & 
  scale_colour_continuous(limits = range(c(sb_meta$gender, diff$gender)))


# Confidence data ---------------------------------------------------------
# confidence per modality
conf <- meta[,c(2, 4, 43:46)]
conf_long <- gather(conf, key="mod", value="conf", conf_mem:conf_cal)

mconf <- conf_long %>%
  group_by(gender, mod) %>%
  summarise(mc = mean(conf))
sconf <- conf_long %>%
  group_by(gender, mod) %>%
  summarise(se = std.error(conf))  
avgconf <- cbind(mconf, sconf$se)
colnames(avgconf)[4] <- "se"

confmod <-avgconf %>%
  ggplot(aes(x = mod, y = mc, group = interaction(gender, mod), shape=gender))+
  geom_line(aes(group = gender), position=position_dodge(width=0.2), col = "gray30") +
  geom_errorbar(aes(ymin= mc-se, ymax=mc+se), position=position_dodge(width=0.2), width=.07, col = "gray30") +
  geom_point(size = 3, position=position_dodge(width=0.2), col = "gray30", fill = 'white', stroke = 0.7) +
  scale_shape_manual(values = c(21, 19), name = "Gender", labels = c("Female", "Male"))+
  scale_x_discrete(limits = c("conf_mem", "conf_vis", "conf_gdp", "conf_cal"), labels = c("Memory", "Vision", "GDP", "Calories"))+
  labs(title="Trialwise confidence per domain", y="Confidencce ± SEM", tag = "A") +
  ylim(3.5, 5.5)+
  xlab(element_blank())+
  theme_cowplot()+
  geom_signif(y_position = 5, xmin = c(2.95), xmax = c(3.05), annotation = c("***"), tip_length = 0.02, textsize = 5, size = 0.5, col = "black") +
  theme(text = element_text(family = "serif"), plot.title = element_text(hjust = 0.5, size = 22))


# confidence difference for hit and miss
diff <- meta_long %>%
  filter(str_detect(mod, "conf_diff_")) %>%
  group_by(gender, mod) %>%
  summarise(md = mean(measure))  
diff2 <- meta_long %>%
  filter(str_detect(mod, "conf_diff_")) %>%
  group_by(gender, mod) %>%
  summarise(sed = std.error(measure))  
diff <- cbind(diff, diff2$sed)
colnames(diff)[4] <- "sed"


confdiff <- diff %>%
  ggplot(aes(x = mod, y = md, group = interaction(gender, mod), shape=gender))+
  geom_line(aes(group = gender), position=position_dodge(width=-0.2), col = "gray30") +
  geom_errorbar(aes(ymin=md-sed, ymax=md+sed), width=.1,position=position_dodge(-0.2), col = "gray30") +
  geom_point(size = 3, position=position_dodge(width=-0.2), col = "gray30", fill = 'white', stroke = 0.7) +
  scale_shape_manual(values = c(21, 19), name = "Gender", labels = c("Female", "Male"))+
  scale_x_discrete(limits = c("conf_diff_mem", "conf_diff_vis", "conf_diff_gdp", "conf_diff_cal"), labels = c("Memory", "Vision", "GDP", "Calories"))+
  labs(title="Error sensitivity per domain", y="Correct-error confidence ± SEM", tag = "B") +
  ylim(0.5, 2.5)+
  xlab(element_blank())+
  theme_cowplot()+
  geom_signif(y_position = 2.35, xmin = c(0.95), xmax = c(1.05), annotation = c("**"), tip_length = 0.02, textsize = 5, size = 0.5, col = "black") +
  theme(text = element_text(family = "serif"), plot.title = element_text(hjust = 0.5, size = 22), legend.position = "none")

confmod + confdiff + plot_layout(guides = "collect")

