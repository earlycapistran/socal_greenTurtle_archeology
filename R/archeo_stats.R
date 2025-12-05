
library(dplyr)

data <- readRDS("./data/processed/archaeo_plot_data.rds")

total_count <- data %>% 
  filter(vulgar_name == "all") %>% 
  select(num_results)

category_stats <- data %>%
  group_by(category) %>%
  summarise(
    mean_count = mean(num_results),
    median_count = median(num_results)
  )

category_percentages <- data %>%
  group_by(category) %>% 
  summarise(
    perc = (num_results/total_count) * 100
  )


group_stats <- data %>%
  group_by(subcategory) %>%
  summarise(
    mean_count = mean(num_results),
    median_count = median(num_results)
  )

group_percentages <- data %>%
  group_by(subcategory) %>% 
  summarise(
    perc = (num_results/total_count) * 100
  )
