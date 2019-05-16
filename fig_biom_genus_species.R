library(sapfluxnetr)
library(tidyverse)
library(cowplot)

# 0. Run requirements  ----------------------------------

# Quick to run
source('metadata_wrangling.R')


# 1. Biome plot -----------------------------------------------------------

biomes_plot <- sfn_allsites %>%
  arrange(si_biome) %>%
  sapfluxnetQC1::vis_location_biome() +
  
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    title= element_text(size=30),
    axis.title=element_text(size=30),
    axis.text=element_text(size=30),
    legend.title = element_blank(),
    legend.text = element_text(size =24),
    legend.key.size = unit(1.5, 'line'),
    legend.position = c(0.7,0.2)
    
  )

biomes_plot
