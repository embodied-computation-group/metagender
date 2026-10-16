library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(gghalves)
library(viridis)
library(cowplot)
library(patchwork)
source("code/theme_high_impact.R")

# Access the sample.diff data
sample_diff = allResults$allResults[[1]][[4]]
hdi = allResults$allResults[[1]][[1]]
posterior_prob = allResults$allResults[[1]][[3]]

label_text <- paste("Posterior Probability of Difference >", round(posterior_prob, 3))

# Convert the data to a data frame
sample_diff_df <- data.frame(sample_diff = as.vector(sample_diff))

p1 <- ggplot(sample_diff_df, aes(x = sample_diff)) +
  geom_histogram(binwidth = 0.01, fill = "gray", color = "black") +
  geom_segment(aes(x = hdi[1, 1], y = 0, xend = hdi[1, 2], yend = 0),
               color = "red", size = 2) +
  theme_high_impact() +  # Replace theme_high_impact with theme_minimal or another standard theme
  labs(title = "Total Model",
       subtitle = label_text,
       x = "Sample Difference",
       y = "Count")

# Print the plot
print(p1)
