library(tidyverse)

archeo_data <- read.csv("./data/archeo_search_terms_data_20251117.csv")

# Filter
faunal_data <- archeo_data %>% 
  select(species, num_results, taxon)

# Sort by abundance for better readability
faunal_data <- faunal_data %>%
  arrange(num_results) %>%
  mutate(taxon = factor(taxon))  # Lock in order

ggplot(faunal_data, aes(x = num_results, y = taxon)) +
  geom_segment(aes(x = 0.5, xend = num_results, y = taxon, yend = taxon),  # Start at 0.5 for log scale
               linewidth = 0.8, color = "gray40") +
  geom_point(size = 3.5, color = "#2C3E50") +
  scale_x_log10(
    breaks = c(1, 5, 10, 50, 100, 500),
    labels = c("1", "5", "10", "50", "100", "500")
  ) +
  annotation_logticks(sides = "b") +
  labs(
    x = "Number of Identified Specimens (NISP, log scale)",
    y = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(
    axis.text.y = element_text(face = "italic"),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank()
  )
