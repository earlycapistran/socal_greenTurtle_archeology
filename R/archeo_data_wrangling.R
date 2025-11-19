library(dplyr)
library(forcats)
library(stringr)

raw_data <- read.csv("./data/raw/archeo_searchTerms_data_readOnly.csv")

### Make classification scheme ------------------------------
classification <- data.frame(
  vulgar_name = c("swordfish", "tuna", "shark", "barracuda", "rock fish", 
                  "lingcod", "sheephead", "sculpin", "prickleback", "sardine", 
                  "flatfish", "rays", "guadalupe fur seal", "northern fur seal", 
                  "stellar sea lion", "california sea lion", "elephant seal", 
                  "harbor seal", "common dolphin", "risso dolphin",
                  "pacific white-sided dolphin", "northern right whale dolphin", 
                  "bottlenose dolphin", "orca","short-finned pilot whale", 
                  "sperm whale", "sea otter", "seabird", "auklet", "cormorant",
                  "sea turtle", "abalone", "red abalone", 
                  "black abalone", "turban snail", "mussel", "shellfish", 
                  "urchin", "chiton"),
  group = c(rep("Large Pelagic Fish", 4), rep("Nearshore Fish", 5), 
            "Small Pelagic Fish", rep("Benthic Fish", 2), rep("Fur Seals", 2), 
            rep("Sea Lions", 2), rep("True Seals", 2), rep("Small Cetaceans", 5),
            rep("Large Cetaceans", 3), "Sea Otters", rep("Seabirds", 3), 
            "Sea Turtles", rep("Abalone", 3), "Other Gastropods", 
            "Bivalves", "Unclassified Shellfish", "Echinoderms", "Chitons"),
  category = c(rep("Fish", 11), rep("Marine Mammals", 16), rep("Birds", 3), 
               "Reptiles", rep("Invertebrates", 8)),
  scientific_name = c(
    "Xiphias gladius", "Thunnus spp.", "Elasmobranchiia", "Sphyraena spp.", 
    "Sebastes spp.", "Ophiodon elongatus", "Semicossyphus pulcher", "Cottoidea",
    "Stichaeidae", "Sardinops spp", "Pleuronectiformes", "Batoidea",
    "Arctocephalus townsendi", "Callorhinus ursinus", "Eumetopias jubatus",
    "Zalophus californianus", "Mirounga angustirostris", "Phoca vitulina",
    "Delphinus spp.", "Grampus griseus", "Lagenorhynchus obliquidens",
    "Lissodelphis borealis", "Tursiops truncatus", "Orcinus orca",
    "Globicephala macrorhynchus", "Physeter macrocephalus", "Enhydra lutris",
    "Aves (marine)", "Alcidae", "Phalacrocoracidae",
    "Cheloniidae", "Haliotis spp.", "Haliotis rufescens", "Haliotis cracherodii", "Turbinidae",
    "Mytilidae", "Mollusca", "Echinoidea", "Polyplacophora"
  )
)

# Order the factors
classification <- classification %>%
  mutate(
    category = factor(category, levels = c("Fish", "Marine Mammals", "Birds", 
                                           "Reptiles", "Invertebrates")),
    group = factor(group, levels = unique(group))
  )

print(classification)

### ---- Reorganize archeological data with categories
archaeo_plot_data <- raw_data %>% 
  select(species_search_term, num_results) %>% 
  mutate(vulgar_name = tolower(species_search_term)) %>% 
  mutate(vulgar_name = str_replace(vulgar_name, 
                               "sea turtle or marine turtle", 
                               "sea turtle"))

archaeo_plot_data_full <- archaeo_plot_data %>% 
  full_join(classification) %>% 
  select(- species_search_term)

# Save the new dataframe for plotting
saveRDS(archaeo_plot_data_full, "./data/processed/archaeo_plot_data.rds")
write.csv(archaeo_plot_data_full, "./data/processed/archaeo_plot_data_clean.csv")
