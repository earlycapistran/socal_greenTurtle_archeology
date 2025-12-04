
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

show.legend = c(color = FALSE)
# Create a named vector of colors, matching the order of subcategories
subcategory_colors <- setNames(color_palette, ordered_subcategories)

# Plotting ----------------------------------------------

lolli_plot <- ggplot(plot_data, aes(x = subcategory_total, y = category)) +
  # The "stick" of the lollipop
  geom_segment(aes(x = 1, 
                   xend = category_cumulative,
                   yend = category,
                   color = subcategory),
               linewidth = 5,
               show.legend = FALSE) +
  
  # Add points for subcategories (excluding first point at 0)
  geom_point(data = . %>% filter(category_previous != 0),
             aes(x = category_previous, 
                 color = subcategory),
             size = 9,
             shape = 16) +  # This one SHOWS in legend
  
  # The "pop" at the end - make it bigger for the max value per category
  geom_point(aes(x = category_cumulative, 
                 color = subcategory), 
             size = 9,
             shape = 16) +
  
  # Label - Total value per category
  geom_text(data = . %>% 
              group_by(category) %>% 
              slice(1),
            aes(x = category_total,
                label = scales::comma(category_total)),
            hjust = -0.7,
            size = 7,
            family = "Lato") +
  
  scale_color_manual(values = subcategory_colors) +
  scale_size_manual(values = c(`FALSE` = 5, `TRUE` = 7), guide = "none") +
  theme_minimal()


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
    axis.text.y = element_text(size = 24, face = "bold"),
    axis.text.x = element_text(size = 16),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 22),
    plot.margin = margin(10, 30, 10, 10)
  ) +
  guides(color = guide_legend(ncol = 1))

lolli_plot_final

########################
# Stacked bar chart version
stacked_bar <- ggplot(plot_data, aes(x = category_total, y = category)) +
  # Stacked bars
  geom_col(aes(x = subcategory_total,
               fill = subcategory),
           position = position_stack(reverse = FALSE)) +
  
  # Border around the full bar
  geom_col(data = . %>% 
             group_by(category) %>% 
             slice(1),
           aes(x = category_total),
           fill = NA,
           color = "gray10",
           linewidth = 0.2) +
  
  # Label - Total value per category
  geom_text(data = . %>% 
              group_by(category) %>% 
              slice(1),
            aes(x = category_total,
                label = scales::comma(category_total)),
            hjust = -0.2,
            size = 7,
            family = "Lato") +
  
  scale_fill_manual(values = subcategory_colors) +
  scale_x_continuous(
    labels = scales::comma_format(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(x = "Number of Records in Archeological Literature",
       y = NULL,
       fill = "Analytical Groups") +
  theme_classic() +
  theme(
    text = element_text(family = "Lato"),
    axis.text.y = element_text(size = 24, face = "bold"),
    axis.text.x = element_text(size = 16),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 22),
    plot.margin = margin(10, 30, 10, 10)
  ) +
  guides(fill = guide_legend(reverse = TRUE, ncol = 1))


stacked_bar

