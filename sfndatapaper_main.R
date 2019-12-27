library(sapfluxnetr)
library(tidyverse)

# 0. Data needed to obtain figures, tables and results --------------------

# source('read_metadata.R')

# 1. Choose map quality -----------------------------------------------------
# 'high' quality for definitive fig, 'draft' is faster

plot_quality <- 'high'

if (plot_quality=='draft') {
  globforest <- globforest_rec_0_5
  print('Draft quality')
} else if (plot_quality=='high') {
  globforest <- globforest_rec_0_1
  print('High quality')
} else{
  print('Nothing done, choose quality')
}

# 2. Run sources ----------------------------------------------------------

source('metadata_wrangling.R')
source('map_sites.R')
source('genus_species.R')


# 3. Save data ------------------------------------------------------------

save.image('sfn_datapaper_data.RData')
