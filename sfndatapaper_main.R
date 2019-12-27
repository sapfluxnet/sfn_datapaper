library(sapfluxnetr)
library(tidyverse)

# 0. Data needed to obtain figures, tables and results --------------------

# source('read_metadata.R')

# 2. Run sources ----------------------------------------------------------

source('metadata_wrangling.R')

# 0. Choose map quality -----------------------------------------------------
# 'high' quality for definitive fig, 'draft' is faster

plot_quality <- 'draft'

if (plot_quality=='draft') {
  source('map_sites_draft.R')
  print('Draft quality')
} else if (plot_quality=='high') {
  source('map_sites.R')
  print('High quality')
} else{
  print('Nothing done, choose quality')
}




source('genus_species.R')



# 3. Save data ------------------------------------------------------------

save.image('sfn_datapaper_data.RData')
