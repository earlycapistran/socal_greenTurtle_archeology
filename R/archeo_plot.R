
library(tidyverse)
library(viridisLite)
library(ggthemes)

plot_data <- readRDS("./data/processed/archeo_plot_ranked.rds")

# Make custom pallette based on viridis --------------
n_colors <- n_distinct(plot_data$subcategory)
color_palette <- viridis(n_colors, option = "viridis", direction = -1)

# Get the unique subcategories in the order they appear in plot_data
ordered_subcategories <- plot_data %>%
  arrange(category_rank, subcategory_rank) %>%
  pull(subcategory) %>%
  unique()

# Create a named vector of colors, matching the order of subcategories
subcategory_colors <- setNames(color_palette, ordered_subcategories)

# Plotting ----------------------------------------------

# Create the basic lollipop chart
lolli_plot <- ggplot(plot_data, aes(x = subcategory_total, y = category)) +
  # The "stick" of the lollipop
  geom_segment(aes(x = 1, 
                   xend = category_cumulative, # Reaches cumulative total for category
                   yend = category,
                   color = subcategory),
               linewidth = 1.5) +
  # The "pop" at the end for category totals
  geom_point(aes(x = category_total)) +
  geom_text(aes(x = category_total, 
                label = paste(scales::comma(category_total))
                ), 
            hjust = -0.2, 
            size = 5)

# Add points for subcategories with viridis pallette
lolli_plot_2 <- lolli_plot +  
  geom_point(aes(x = category_previous), 
             size = 3, 
             color = subcategory_colors) +
  scale_color_manual(values = subcategory_colors)
lolli_plot_2 

# Fixing aesthetics
lolli_plot_3 <- lolli_plot_2 +
  labs(x = "Number of References",
       y = NULL,
       color = "Subcategory") +
  scale_x_continuous(labels = scales::comma_format(),
                     expand = expansion(mult = c(0, .1)), # Add some space on the right for labels
                     limits = c(0, max(plot_data$category_total) * 1.1)) + # Set proper limits
  theme_few(base_size = 16) +
  theme(
    text=element_text(family="Lato"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 12),
    plot.margin = margin(10, 30, 10, 10)
  ) +
  guides(color = guide_legend(ncol = 1)) 

lolli_plot_3


