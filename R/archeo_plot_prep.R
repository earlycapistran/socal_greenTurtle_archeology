library(tidyverse)

faunal_data <- readRDS("./data/processed/archaeo_plot_data.rds") %>% 
  na.omit()

plot_data <- faunal_data %>%
  group_by(category, subcategory) %>%
  # Calculate results by subcategory
  summarise(subcategory_total = sum(num_results), 
            .groups = "drop") %>%
  #Calculate results by category
  group_by(category) %>%
  mutate(
    category_total = sum(subcategory_total),
    # Order subcategories within each category from largest to smallest
    subcategory_rank = rank(subcategory_total, ties.method = "first")
  ) %>%
  # Sort subcategories from largest to smallest within each category
  arrange(desc(subcategory_total)) %>%
  mutate(
    # Calculate cumulative values from largest to smallest subcategory
    category_cumulative = cumsum(subcategory_total),
    category_previous = lag(category_cumulative, default = 0)
  ) %>%
  ungroup() %>% 
  # Generate ranks for each category from highest values to lowest
  mutate(
    category_rank = dense_rank(category_total)  # Use negative to rank from highest to lowest
  ) %>%
  # Order categories from highest to lowest and maintain subcategory order
  arrange(category_rank, subcategory_rank) %>%
  mutate(
    category = fct_reorder(category, category_total),  # This ensures correct ordering
    subcategory = factor(subcategory, levels = unique(subcategory))
  )

# Identify the subcategory with the highest value in each category
highest_subcategories <- plot_data %>%
  group_by(category) %>%
  slice_max(order_by = subcategory_total, n = 1) %>%
  ungroup() %>%
  select(category, highest_subcategory = subcategory)

# Join this information back to the main dataset
plot_data <- plot_data %>%
  left_join(highest_subcategories, by = "category") %>%
  mutate(is_highest = subcategory == highest_subcategory)

saveRDS(plot_data, "./data/processed/archeo_plot_ranked.rds")
