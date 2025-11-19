library(tidyverse)

faunal_data <- readRDS("./data/processed/archaeo_plot_data.rds") %>% 
  na.omit()

# Prepare the data-----------
plot_data <- faunal_data %>%
  group_by(category, subcategory) %>%
  # Calculate results by subcategory
  summarise(subcategory_total = sum(num_results), 
            .groups = "drop") %>%
  #Calculate results by category
  group_by(category) %>%
  mutate(
    category_total = sum(subcategory_total),
    # Order subcategories within each category
    subcategory_rank = rank(-subcategory_total, 
                            ties.method = "first")
  ) %>%
  ungroup() %>% 
  # Generate ranks for each category from highest values to lowest
  mutate(
    category_rank = dense_rank(desc(category_total))
  ) %>%
  # Order categories and subcategories
  arrange(category_rank, subcategory_rank) %>%
  mutate(
    category = factor(category, levels = unique(category)),
    subcategory = factor(subcategory, levels = unique(subcategory))
  )

saveRDS(plot_data, "./data/processed/archeo_plot_ranked.rds")
