theme_high_impact <- function() {
  theme_cowplot() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      panel.grid.major = element_line(color = "grey80", size),
      panel.grid.minor = element_line(color = "grey90"),
      axis.text = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 14, face = "bold"),
      axis.line = element_line(size = 1.5), # Bolder axis lines
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      plot.caption = element_text(size = 10),
      legend.background = element_rect(fill = "white"),
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 12),
      strip.background = element_rect(fill = "white", color = "white"),  # Remove black background
      strip.text = element_text(size = 14)  # Customize facet label text
    )
}
