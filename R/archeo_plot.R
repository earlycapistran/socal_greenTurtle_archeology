library(tidyverse)
faunal_data <- readRDS("./data/processed/archaeo_plot_data.rds")

# Prepare the data by generating totals and subtotals
plot_data <- faunal_data %>%
  group_by(category, group) %>%
  mutate(group_total = sum(num_results)) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(category_total = sum(num_results)) %>%
  arrange(category, desc(group_total)) %>%
  ungroup() %>%
  mutate(
    category = factor(category, levels = unique(category)),
    group = factor(group, levels = unique(group))
  )

# Create the lollipop chart ----
ggplot(plot_data, aes(x = group_total, y = category)) +
  # The "stick" of the lollipop
  geom_segment(aes(x = 0, xend = group_total, 
                   y = category, yend = category,
                   color = group),
               linewidth = 1.5) +
  # The "pop" at the end for group totals
  geom_point(aes(color = group), size = 4) +
  # Labels for group totals
  geom_text(aes(label = group_total, color = group), 
            hjust = -0.5, size = 3, fontface = "bold") +
  # Labels for category totals
  geom_text(aes(x = category_total, label = paste("Total:", category_total)), 
            hjust = -0.2, size = 3, fontface = "bold", color = "black") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.y = element_text(angle = 0, hjust = 0),
    strip.background = element_rect(fill = "grey90", color = NA),
    legend.position = "bottom"
  ) + # Create the lollipop chart
  ggplot(plot_data, aes(x = group_total, y = reorder(category, category_total))) +
  # The "stick" of the lollipop
  geom_segment(aes(x = 1, xend = group_total, 
                   y = category, yend = category,
                   color = group),
               linewidth = 1.5) +
  # The "pop" at the end for group totals
  geom_point(aes(color = group), size = 4) +
  # Labels for group totals
  geom_text(aes(label = scales::comma(group_total), color = group), 
            hjust = -0.3, size = 3, fontface = "bold") +
  # Labels for category totals
  geom_text(aes(x = category_total, label = paste("Total:", scales::comma(category_total))), 
            hjust = -0.2, size = 3, fontface = "bold", color = "black") +
  # Add group labels
  geom_text(aes(x = 1, label = group), hjust = 1.1, size = 3, 
            color = "black", fontface = "bold") +
  scale_x_log10(breaks = c(1, 10, 100, 1000), 
                labels = scales::comma_format(),
                limits = c(1, max(plot_data$category_total) * 1.2)) +
  scale_color_viridis_d() +
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
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    plot.margin = margin(10, 30, 10, 10)
  )
  scale_x_log10(breaks = c(1, 10, 100, 1000), labels = scales::comma_format())

  
  #######
  # Create the lollipop chart
  ggplot(plot_data, aes(x = group_total, y = reorder(category, category_total))) +
    # The "stick" of the lollipop
    geom_segment(aes(x = 1, xend = group_total, 
                     y = category, yend = category,
                     color = group),
                 linewidth = 1.5) +
    # The "pop" at the end for group totals
    geom_point(aes(color = group), size = 4) +
    # Labels for group totals
    geom_text(aes(label = scales::comma(group_total), color = group), 
              hjust = -0.3, size = 3, fontface = "bold") +
    # Labels for category totals
    geom_text(aes(x = category_total, label = paste("Total:", scales::comma(category_total))), 
              hjust = -0.2, size = 3, fontface = "bold", color = "black") +
    # Add group labels
    geom_text(aes(x = 1, label = group), hjust = 1.1, size = 3, 
              color = "black", fontface = "bold") +
    scale_x_log10(breaks = c(1, 10, 100, 1000), 
                  labels = scales::comma_format(),
                  limits = c(1, max(plot_data$category_total) * 1.2)) +
    scale_color_viridis_d() +
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
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title.x = element_text(size = 12, face = "bold"),
      plot.margin = margin(10, 30, 10, 10)
    )
  