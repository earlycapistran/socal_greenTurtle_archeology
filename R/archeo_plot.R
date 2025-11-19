
library(tidyverse)
library(viridisLite)

plot_data <- readRDS("./data/processed/archeo_plot_ranked.rds")

# Create color palette
n_colors <- n_distinct(plot_data$subcategory)
color_palette <- viridis(n_colors, option = "viridis")

# Get the unique subcategories in the order they appear in plot_data
ordered_subcategories <- plot_data %>%
  arrange(category_rank, subcategory_rank) %>%
  pull(subcategory) %>%
  unique()

# Create a named vector of colors, matching the order of subcategories
subcategory_colors <- setNames(color_palette, ordered_subcategories)

# Create the lollipop chart
ggplot(plot_data, aes(x = subcategory_total, 
                      y = category, 
                      color = subcategory)) +
  # The "stick" of the lollipop
  geom_segment(aes(x = 1, xend = subcategory_total, 
                   yend = category),
               linewidth = 1.5) +
  # The "pop" at the end for group totals
  geom_point(size = 4) +
  geom_text(aes(x = category_total, label = paste("Total:", scales::comma(category_total))), 
            hjust = -0.2, size = 3, fontface = "bold", color = "black") +
  scale_x_log10(breaks = c(1, 10, 100, 1000), 
                labels = scales::comma_format(),
                limits = c(1, max(plot_data$category_total) * 1.2)) +
  scale_color_manual(values = subcategory_colors) +
  labs(x = "Number of References (log scale)",
       y = NULL,
       color = "Subcategory") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    plot.margin = margin(10, 30, 10, 10)
  ) +
  guides(color = guide_legend(ncol = 1)) 
  
