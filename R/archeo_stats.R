
library(dplyr)

data <- readRDS("./data/processed/archaeo_plot_data.rds")

# Data prep --------------------------------------------------

# Get total number of documents
total_count <- data %>% 
  filter(vulgar_name == "all") %>% 
  select(num_results)

# Get total number of mentions
total_mentions <- data %>% 
  # Total minus the number of documents (unspecified taxa)
  summarise(sum(num_results)-total_count)

### Descriptive statistics ----------------------------------

# Calculate descriptive stats and percentages by group
category_stats <- data %>%
  group_by(category) %>%
  summarise(
    mean = mean(num_results),
    sd = sd(num_results),
    # Total mentions per taxa
    total_refs = sum(num_results),
    # Percentage of total mentions
    perc = (total_refs/total_mentions) * 100
  )

# Calculate descriptive stats and percentages by subcategory
group_stats <- data %>%
  group_by(subcategory) %>%
  summarise(
    mean = mean(num_results),
    sd = sd(num_results),
    total_refs = sum(num_results),
    perc = (total_refs/total_mentions) * 100
  )

group_stats <- data %>%
  group_by(subcategory)
