library(dplyr)
library(forcats)

raw_data <- read.csv("./data/archeo_search_terms_data_20251117.csv")

### Make classification scheme ------------------------------
classification <- data.frame(
  vulgar_name = c("swordfish", "tuna", "shark", "barracuda", "rock fish", "lingcod", 
            "sheephead", "sculpin", "prickleback",
            "sardine", "flatfish", "rays", "guadalupe fur seal", 
            "northern fur seal", "stellar sea lion",
            "california sea lion", "elephant seal", "harbor seal", "common dolphin", "risso dolphin",
            "pacific white-sided dolphin", "northern right whale dolphin", "bottlenose dolphin", "orca",
            "short-finned pilot whale", "sperm whale", "sea otter", "seabird", "auklet", "cormorant",
            "sea turtle", "marine turtle", "abalone", "red abalone", "black abalone", "turban snail",
            "mussel", "shellfish", "urchin", "chiton"),
  group = c(rep("Large Pelagic Fish", 4), rep("Nearshore Fish", 5), "Small Pelagic Fish", rep("Benthic Fish", 2),
            rep("Fur Seals", 2), rep("Sea Lions", 2), rep("True Seals", 2), rep("Small Cetaceans", 5),
            rep("Large Cetaceans", 3), "Sea Otters", rep("Seabirds", 3), rep("Sea Turtles", 2),
            rep("Abalone", 3), "Other Gastropods", "Bivalves", "Unclassified Shellfish", "Echinoderms", "Chitons"),
  category = c(rep("Fish", 11), rep("Marine Mammals", 16), rep("Birds", 3), rep("Reptiles", 2), rep("Invertebrates", 8)),
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
    "Cheloniidae", "Cheloniidae",
    "Haliotis spp.", "Haliotis rufescens", "Haliotis cracherodii", "Turbinidae",
    "Mytilidae", "Mollusca", "Echinoidea", "Polyplacophora"
  )
)

# Order the factors
classification <- classification %>%
  mutate(
    category = factor(category, levels = c("Fish", "Marine Mammals", "Birds", "Reptiles", "Invertebrates")),
    group = factor(group, levels = unique(group))
  )

print(classification)
