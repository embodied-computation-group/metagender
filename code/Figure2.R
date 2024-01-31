# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(gghalves)
library(viridis)
library(cowplot)
library(patchwork)
source("code/theme_high_impact.R")


# Data Loading
meta <- read.csv('data/megatable.csv')
meta_long <- read.csv('data/megatable_long.csv')

# Data Transformation


# Self belief data --------------------------------------------------------
# pre post interaction


sb_viol <- meta_long %>%
  filter(str_detect(mod, "pre_a|post_a")) %>%
  group_by(gender, mod)

femraw <- sb_viol%>%
  filter(str_detect(gender, "Feminin")) %>%
  group_by(gender, mod)

masraw <- sb_viol%>%
  filter(str_detect(gender, "Masculin")) %>%
  group_by(gender, mod)

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



# updating per modality
sb_dom <- meta_long %>%
  filter(str_detect(mod, "mem_sb|vis_sb|gdp_sb|cal_sb")) %>%
  group_by(gender, mod) %>%
  summarise(
    n = n(),  # Count the number of observations
    md = mean(measure),  # Mean
    sd = sd(measure),  # Standard deviation
    .groups = "drop"  # Optional: drops the grouping structure
  ) %>%
  mutate(
    se = sd / sqrt(n),  # Calculate standard error
    lower_ci = md - 1.96 * se,  # Calculate lower bound of 95% CI
    upper_ci = md + 1.96 * se   # Calculate upper bound of 95% CI
  )



mods <-c("mem_sb", "vis_sb", "gdp_sb", "cal_sb")
sb_dom <- sb_dom[order(factor(sb_dom$mod, levels = mods)),]


# Calculate the difference for each participant
diff_data <- sb_viol %>%
  group_by(subj) %>%
  summarize(
    diff = measure[mod == "post_avg"] - measure[mod == "pre_avg"]
  )

# Merge the difference back into the original data frame
sb_viol <- sb_viol %>%
  left_join(diff_data, by = "subj")

# Reshape the data to wide format to calculate the differences
# For each subject, create a column for pre_avg and post_avg
wide_data <- sb_viol %>%
  select(subj, gender, mod, measure) %>%
  spread(key = mod, value = measure) %>%
  mutate(difference = post_avg - pre_avg)

# Merge the calculated differences back to the original data
# This gives us a dataframe where each subject has the same difference value on both rows
plot_data <- sb_viol %>%
  left_join(wide_data %>% select(subj, gender, difference), by = c("subj", "gender"))

plot_data$mod <- factor(plot_data$mod, levels = c("pre_avg", "post_avg"))

filtered_dataB <- subset(plot_data, mod == "pre_avg")
filtered_dataA <- subset(plot_data, mod == "post_avg")
mod_labels <- c("pre_avg" = "Pre", "post_avg" = "Post")

custom_gender_labels <- c("Masculin" = "Male", "Feminin" = "Female")

subtitle_label <- "Gender × Time, F(1, 315) = 8.91, p = 0.003"
custom_colors <- viridis_pal(option = "C")(length(levels(plot_data$mod)))



## plotting


# Plot
p1 <- ggplot(plot_data, aes(x = mod, y = measure)) +
  geom_line(aes(group = subj, color = difference), alpha = 0.5) +
   geom_half_boxplot(data = filtered_dataA, aes(fill = mod), "side" = "r", alpha = .5, width = 0.20, notch = TRUE, outlier.shape = NA,linewidth = 1.25) +
  geom_half_boxplot(data = filtered_dataB, aes(fill = mod), "side" = "l", alpha = .5, width = 0.20, notch = TRUE, outlier.shape = NA, linewidth = 1.25) +
  facet_wrap(~ gender, labeller = labeller(gender = custom_gender_labels), scales = "free_y") +
  ylim(0,100) +  # Set the common y-limits
  theme_minimal() +
  labs(title = "Metacognitive Self Beliefs: Time by Gender Interaction", x = "Time", y = "Average Self Belief", subtitle = subtitle_label, tag = "A") +
  scale_color_viridis(option = "C", name = "Difference") +  # Set the legend title for the color scale
  guides(fill = FALSE) +  # Turn off the legend for "mod"
  scale_x_discrete(limits = levels(plot_data$mod), labels = mod_labels) +  # Set custom level names
  scale_fill_manual(values = custom_colors, name = "Mod", labels = levels(plot_data$mod)) +
  theme_high_impact()



p2 <- sb_dom %>%
  ggplot(aes(x = mod, y = md, group = interaction(gender, mod))) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0, position = position_dodge(width = 0.2), col = "gray30", linewidth = 1.5) +
  geom_line(aes(group = gender), col = "gray30", position = position_dodge(width = 0.2), linewidth = 1.5) +
  geom_point(aes(shape = gender), size = 3, position = position_dodge(width = 0.2), col = "gray30", fill = 'white', stroke = 0.7) +
  scale_shape_manual(values = c(21, 19), name = "Gender", labels = c("Female", "Male")) +
  scale_x_discrete(limits = c("mem_sb", "vis_sb", "gdp_sb", "cal_sb"), labels = c("Memory", "Vision", "GDP", "Calories"), expand = c(0.1, 0)) +
  labs(title = "Average self-belief per domain", y = "Self-belief ± 95% CI", tag = "B", subtitle = "Modality × Gender, F(3,945) = 30.85, p < 0.001") +
  ylim(40, 70) +
  xlab(element_blank()) +
  theme_high_impact()


p1 / p2

ggsave("figures/Figure2.png", plot  = last_plot(), dpi = 300, height = 11, width = 11, units = "in")
