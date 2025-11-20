
library(tidyverse)
library(viridisLite)

plot_data <- readRDS("./data/processed/archeo_plot_ranked.rds")

# Create color palette
n_colors <- n_distinct(plot_data$subcategory)
color_palette <- viridis(n_colors, option = "viridis", direction = -1)

# Get the unique subcategories in the order they appear in plot_data
ordered_subcategories <- plot_data %>%
  arrange(category_rank, subcategory_rank) %>%
  pull(subcategory) %>%
  unique()

# Create a named vector of colors, matching the order of subcategories
subcategory_colors <- setNames(color_palette, ordered_subcategories)

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
            hjust = -0.2, size = 5)

# Add points for subcategories with viridis pallette
lolli_plot_2 <- lolli_plot +  
  geom_point(aes(x = category_previous), size = 3, color = subcategory_colors) +
  scale_color_manual(values = subcategory_colors)
lolli_plot_2 

   +
  labs(x = "Number of References",
       y = NULL,
       color = "Subcategory") +
  scale_x_continuous(labels = scales::comma_format(),
                     expand = expansion(mult = c(0, .1)), # Add some space on the right for labels
                     limits = c(0, max(plot_data$category_total) * 1.1)) + # Set proper limits
  theme_minimal() +
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

# scale_x_log10(breaks = c(1, 10, 100, 1000), 
#               labels = scales::comma_format(),
#               limits = c(1, max(plot_data$category_total) * 1.2)) +

lolli_plot  


# Create the plot
# ggplot(plot_data_with_totals, aes(x = cumulative_total, 
#                                   y = fct_rev(category),
#                                   color = subcategory)) +
#   geom_line(aes(group = category), size = 1.5) +
#   geom_point(aes(color = subcategory), size = 4) +
#   geom_text(aes(label = scales::comma(subcategory_total), color = subcategory),
#             hjust = -0.3, vjust = -0.5, size = 3) +
#   geom_text(data = subset(plot_data_with_totals, subcategory == "Total"),
#             aes(label = paste("Total:", scales::comma(category_total))), 
#             hjust = -0.2, vjust = 1.5, size = 4, color = "black", fontface = "bold") +
#   scale_color_manual(values = subcategory_colors) +
#   scale_x_continuous(labels = scales::comma_format(),
#                      expand = expansion(mult = c(0, .2))) +
#   labs(x = "Cumulative Number of References",
#        y = NULL,
#        color = "Subcategory") +
#   theme_minimal() +
#   theme(
#     text = element_text(family = "Lato"),
#     axis.text.y = element_text(size = 12, face = "bold"),
#     axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
#     axis.ticks.y = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor = element_blank(),
#     legend.position = "right",
#     legend.title = element_text(size = 10),
#     legend.text = element_text(size = 8),
#     plot.title = element_text(hjust = 0.5, size = 16),
#     axis.title.x = element_text(size = 12),
#     plot.margin = margin(10, 30, 10, 10)
#   ) +
#   guides(color = guide_legend(ncol = 1))

