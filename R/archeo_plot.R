
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
                   xend = category_cumulative,
                   yend = category,
                   color = subcategory),
               linewidth = 1.5) +
  # The "pop" at the end for category totals
  geom_point(aes(x = category_cumulative, 
                 color = subcategory), 
             size = 3) +
  # Label - Total value per category
  geom_text(data = . %>% 
              group_by(category) %>% 
              slice(1),  # Take only first row per category
            aes(x = category_total,
                label = scales::comma(category_total)),
            hjust = -0.5,
            size = 7,
            family = "Lato") +
  # Add points for subcategories, excluding the first point (at 0)
  geom_point(data = . %>% filter(category_previous != 0),
             aes(x = category_previous, 
                 color = subcategory,
                 size = is_highest)) +
  scale_color_manual(values = subcategory_colors) +
  scale_size_manual(values = c(`FALSE` = 3, `TRUE` = 5), guide = "none")

# Fixing aesthetics
lolli_plot_final <- lolli_plot +
  labs(x = "Number of Records in Archeological Literature",
       y = NULL,
       color = "Analytical Groups") +
  scale_x_continuous(labels = scales::comma_format(),
                     expand = expansion(mult = c(0, .1)),
                     limits = c(0, max(plot_data$category_total) * 1.3)) +
  theme_few() +
  theme(
    text = element_text(family = "Lato"),
    axis.text.y = element_text(size = 22, face = "bold"),
    axis.text.x = element_text(size = 16),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 16),
    plot.margin = margin(10, 30, 10, 10)
  ) +
  guides(color = guide_legend(ncol = 1))

lolli_plot_final
