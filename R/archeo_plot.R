
library(tidyverse)
library(viridisLite)

faunal_data <- readRDS("./data/processed/archaeo_plot_data.rds") %>% 
  na.omit()

# Prepare the data
plot_data <- faunal_data %>%
  group_by(category, subcategory) %>%
  summarise(subcategory_total = sum(num_results), .groups = "drop") %>%
  group_by(category) %>%
  mutate(
    category_total = sum(subcategory_total),
    subcategory_rank = rank(-subcategory_total, ties.method = "first")
  ) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(category_total = sum(subcategory_total)) %>%
  ungroup() %>%
  mutate(
    category_rank = dense_rank(desc(category_total)),
    category = fct_reorder(category, category_rank)
  ) %>%
  # Order subcategories within each category
  mutate(subcategory = fct_reorder2(subcategory, category, -subcategory_total)) %>%
  # Arrange the data for proper ordering in the plot
  arrange(category_rank, subcategory_rank) %>% 
  # Order subcategories within each category
  mutate(subcategory = fct_reorder2(subcategory, category, -subcategory_total)) %>%
  # Arrange the data for proper ordering in the plot
  arrange(category_rank, subcategory_rank)

# Print to check the ranking
print(plot_data %>% 
        select(category, category_total, category_rank) %>% 
        distinct() %>% 
        arrange(category_rank))
# Create color palette
n_subcategories <- n_distinct(plot_data$subcategory)
color_palette <- viridis(n_subcategories, option = "viridis")
names(color_palette) <- levels(plot_data$subcategory)

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
  scale_color_manual(values = color_palette) +
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