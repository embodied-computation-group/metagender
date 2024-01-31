# setup

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(gghalves)
library(viridis)
library(cowplot)
library(patchwork)
library(osfr)
library(R.matlab)

source("code/theme_high_impact.R")

## import and plot


mat_file_path <- "data/allResults.mat"

# Read the .mat file
allResults <- readMat(mat_file_path)


# Initialize an empty list to store the plots
plots <- list()

# Define the names for the models
model_names <- c("Calories", "GDP", "Memory", "Total Model", "Vision")


# Loop through each entry in allResults
for (i in 1:length(allResults$allResults)) {
  # Access the data for each entry
  sample_diff = allResults$allResults[[i]][[4]]
  hdi = allResults$allResults[[i]][[1]]
  posterior_prob = allResults$allResults[[i]][[3]]
  zero_line_color <- "#D62728"  # A colorblind-friendly red
  # Create label text
  label_text <- paste("Post. Prob. F > M = ", round(posterior_prob, 3))
  
  # Convert the data to a data frame and include HDI info
  sample_diff_df <- data.frame(sample_diff = as.vector(sample_diff),
                               hdi_start = rep(hdi[1, 1], length(sample_diff)),
                               hdi_end = rep(hdi[1, 2], length(sample_diff)))
  
  xlabel_text = expression(italic(Delta*~{" meta-d'/d'"}))
  
  # Create a plot for each entry
  p <- ggplot(sample_diff_df, aes(x = sample_diff)) +
    geom_histogram(binwidth = 0.01, fill = "gray", color = "black") +
    geom_segment(aes(x = hdi_start, y = 0, xend = hdi_end, yend = 0),
                 color = "#2E86C1", size = 2) +
    geom_vline(xintercept = 0, color = zero_line_color, size = 1, linetype = "longdash") +
    theme_high_impact() +
    labs(title = model_names[i],
         subtitle = label_text,
         x =  xlabel_text,
         y = "Count")
  
  # Store the plot in the list
  plots[[i]] <- p
}


# New order of model names
new_order <- c("Total Model", "Memory", "Vision", "GDP", "Calories")

# Create a vector that maps the new order to the old order
order_vector <- match(new_order, model_names)

# Reorder the plots list
plots <- plots[order_vector]

# Extract the first plot and save it
top_plot <- plots[[1]]

# Remove the first plot from the list
plots <- plots[-1]

# Combine the remaining four plots into a 2x2 grid
bottom_grid <- wrap_plots(plots, nrow = 2, ncol = 2)

# Combine the top plot with the 2x2 grid of the remaining plots
combined_plot <- top_plot / bottom_grid + 
  plot_layout(heights = c(1, 2))  # Adjust the relative heights as necessary

# Print the combined plot
print(combined_plot)


ggsave(filename = "figures/figure4.png", dpi = 600, height = 8, width = 11, units = "in")
