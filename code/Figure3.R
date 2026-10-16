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

# Selecting and transforming confidence data
confidence <- meta %>% 
  select(subj, gender, mem_1:cal_0)

long_confidence <- confidence %>%
  pivot_longer(
    cols = starts_with("mem_") | starts_with("vis_") | starts_with("gdp_") | starts_with("cal_"),
    names_to = c(".value", "accuracy"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  mutate(
    accuracy = factor(accuracy, levels = c("1", "0"), labels = c("Correct", "Incorrect"))
  )

# Calculate mean scores and reshape for difference calculation
mean_scores <- long_confidence %>%
  group_by(subj, gender, accuracy) %>%
  summarise(
    mean_score = mean(c(mem, vis, gdp, cal), na.rm = TRUE),
    .groups = "drop"
  )

diff_scores <- mean_scores %>%
  pivot_wider(
    names_from = accuracy,
    values_from = mean_score
  ) %>%
  mutate(
    diff_score = Incorrect - Correct
  )

# Preparing data for plotting
enhanced_summary_df <- mean_scores %>%
  left_join(diff_scores, by = c("subj", "gender"))

filtered_data_correct <- subset(enhanced_summary_df, accuracy == "Correct")
filtered_data_incorrect <- subset(enhanced_summary_df, accuracy == "Incorrect")

# Define custom elements for plotting
custom_gender_labels <- c("Masculin" = "Male", "Feminin" = "Female")
custom_colors <- viridis_pal(option = "C")(length(levels(enhanced_summary_df$accuracy)))
subtitle_labelA <- "Gender × Accuracy, F(3, 315) = 7.18, p = 0.008"
subtitle_labelB <- "Gender × Domain, F(3, 945)  = 15, p < 0.001"

# Plotting

# Plot 1: Local Confidence
p1 <- ggplot(data = enhanced_summary_df, aes(x = accuracy, y = mean_score)) +
  geom_line(aes(group = subj, x = accuracy, color = diff_score), alpha = 0.5) +
  geom_half_boxplot(data = filtered_data_correct, aes(fill = accuracy), side = "l", outlier.shape = NA, notch = TRUE, alpha = 1, width = .20, linewidth =1.25) +
  geom_half_boxplot(data = filtered_data_incorrect, aes(fill = accuracy), side = "r", outlier.shape = NA, notch = TRUE, alpha = 1, width = .20, linewidth =1.25) +
  facet_wrap(~ gender, labeller = labeller(gender = custom_gender_labels), scales = "free_y") +
  scale_color_viridis(option = "C", name = "Difference") +
  scale_fill_brewer(palette = "Set2") +
  guides(fill = FALSE) +
  scale_x_discrete(limits = levels(enhanced_summary_df$accuracy)) +
  #scale_fill_manual(values = custom_colors, name = "Accuracy") +
  scale_y_continuous(limits = c(1,7), breaks = c(1:7)) +
  labs(title = "Local Confidence: Accuracy by Gender Interaction", x = "Trialwise Accuracy", y = "Average Confidence", subtitle = subtitle_labelA, tag = "A") +
  theme_high_impact()

# Plot 2: Confidence by Modality and Gender
longer_confidence <- long_confidence %>%
  pivot_longer(
    cols = c(mem, vis, gdp, cal),
    names_to = "task_modality",
    values_to = "score"
  )

# Calculate summary data with standard error
summary_data <- longer_confidence %>%
  group_by(task_modality, gender, accuracy) %>%
  summarise(
    mean_score = mean(score, na.rm = TRUE),
    se_score = sd(score, na.rm = TRUE) / sqrt(n()),  # Standard error
    .groups = "drop"
  ) %>%
  mutate(
    lower_ci = mean_score - 1.96 * se_score,  # Lower bound of 95% CI
    upper_ci = mean_score + 1.96 * se_score   # Upper bound of 95% CI
  )

# reorder factors to match other figues
summary_data$task_modality <- factor(summary_data$task_modality, 
                                     levels = c("mem", "vis", "gdp", "cal"))


# Plotting
p2 <- ggplot(summary_data, aes(x = task_modality, y = mean_score, group = interaction(accuracy, gender), color = accuracy)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0, linewidth = 1.5, position = position_dodge(width = 0.2)) +
  geom_line(aes(linetype = gender), linewidth = 1.5, position = position_dodge(width = 0.2)) +
  geom_point(size = 3, shape = 21, position = position_dodge(width = 0.2), stroke = 0.7, fill = "white") +
  labs(x = "Task Domain", y = "Mean Score", color = "Accuracy", linetype = "Gender") +
  scale_color_brewer(palette = "Set2") +
  scale_linetype_manual(values = c("solid", "dotdash"), labels = custom_gender_labels) +  # Custom linetype labels
  scale_y_continuous(limits = c(2,6), breaks = c(2:6)) +
  scale_x_discrete(labels = c("Memory", "Vision", "GDP", "Calories"), expand = c(0.1, 0)) +
  labs(title = "Local Confidence: Gender, Domain, & Accuracy", tag = "B", y = "Average Confidence ± 95% CI", subtitle = subtitle_labelB) +
  theme_high_impact() +
  theme(
    #legend.title = element_text(size = 12),   # Adjust legend title size
    #legend.text = element_text(size = 10),    # Adjust legend text size
    legend.key.size = unit(1.5, 'lines')#,     # Adjust legend key size
    #plot.margin = unit(c(0, 0, 0, 0), "lines")
    
  )



# Combining plots
combined_plot <- p1 / p2
combined_plot
# Save combined plot
ggsave(filename = "figures/Figure3.png", plot = combined_plot, dpi = 300, width = 11, height = 11, units = "in")




