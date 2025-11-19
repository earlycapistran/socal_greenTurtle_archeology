library(tidyverse)
library(viridisLite)
faunal_data <- readRDS("./data/processed/archaeo_plot_data.rds") %>% 
  na.omit()

# Prepare the data
  plot_data <- faunal_data %>%
    group_by(category, group) %>%
    summarise(group_total = sum(num_results), .groups = "drop") %>%
    group_by(category) %>%
    mutate(
      category_total = sum(group_total),
      group_rank = rank(-group_total, ties.method = "first")
    ) %>%
    arrange(category, desc(group_total)) %>%
    ungroup() %>%
    mutate(
      category = fct_reorder(category, category_total),
      group = fct_reorder2(group, category, -group_rank)
    )
  
# Create the lollipop chart
  ggplot(plot_data, aes(x = group_total, y = category, color = group)) +
    # The "stick" of the lollipop
    geom_segment(aes(x = 1, xend = group_total, 
                     yend = category),
                 linewidth = 1.5) +
    # The "pop" at the end for group totals
    geom_point(size = 4) +
    geom_text(aes(x = category_total, label = paste("Total:", scales::comma(category_total))), 
              hjust = -0.2, size = 3, fontface = "bold", color = "black") +
    scale_x_log10(breaks = c(1, 10, 100, 1000), 
                  labels = scales::comma_format(),
                  limits = c(1, max(plot_data$category_total) * 1.2)) +
    scale_color_viridis_d(discrete = TRUE) +
    labs(title = "Number of References by Group and Category",
         x = "Number of References (log scale)",
         y = NULL,
         color = "Group") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 12, face = "bold"),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title.x = element_text(size = 12, face = "bold"),
      plot.margin = margin(10, 30, 10, 10)
    )
  
  
  ####
  
  # First, let's prepare our color palette
  n_total_groups <- n_distinct(plot_data$group)
  full_palette <- viridis(n_total_groups, option = "viridis")  # You can change "viridis" to "magma", "plasma", etc.
  
  # Create a function to assign colors to groups within a category
  assign_category_colors <- function(category_data, start_index) {
    n_groups <- n_distinct(category_data$group)
    end_index <- start_index + n_groups - 1
    setNames(full_palette[start_index:end_index], unique(category_data$group))
  }
  
  # Create a named vector of colors for all groups
  group_colors <- plot_data %>%
    arrange(category) %>%  # Ensure categories are in the desired order
    group_by(category) %>%
    group_map(~ {
      assign_category_colors(.x, which(unique(plot_data$category) == .y$category)[1])
    }) %>%
    unlist()
  
  # Now, create the plot with the custom color scale
  ggplot(plot_data, aes(x = group_total, y = category, color = group)) +
    geom_segment(aes(x = 1, xend = group_total, yend = category), linewidth = 1.5) +
    geom_point(size = 4) +
    geom_text(aes(x = category_total, label = paste("Total:", scales::comma(category_total))), 
              hjust = -0.2, size = 3, fontface = "bold", color = "black") +
    scale_x_log10(breaks = c(1, 10, 100, 1000), 
                  labels = scales::comma_format(),
                  limits = c(1, max(plot_data$category_total) * 1.2)) +
    scale_color_manual(values = group_colors) +  # Use our custom color scale
    labs(title = "Number of References by Group and Category",
         x = "Number of References (log scale)",
         y = NULL,
         color = "Group") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 12, face = "bold"),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.8, "cm"),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title.x = element_text(size = 12, face = "bold"),
      plot.margin = margin(10, 30, 10, 10)
    ) +
    guides(color = guide_legend(ncol = 1))
  