library(sapfluxnetr)
library(tidyverse)


# devtools::install_github("ropenscilabs/datastorr")
# devtools::install_github("wcornwell/taxonlookup")


# TODO: -------------------------------------------------------------------
# integrate code of genus sp in md file and delete R file?
# separate data and maps


# 0. Data needed to obtain figures, tables and results --------------------

# source('read_metadata.R')

# 2. Run sources ----------------------------------------------------------

source('metadata_wrangling.R')

# 0. Choose map quality -----------------------------------------------------
# 'high' quality for definitive fig, 'draft' is faster

plot_quality <- 'draft'

if (plot_quality=='draft') {
  source('maps_sites_draft.R')
  print('Maps ok')

  save.image('sfn_datapaper_data_draft.RData')
  print('Draft quality')

} else if (plot_quality=='high') {
  source('map_sites.R')
  source('genus_species.R')
  save.image('sfn_datapaper_data.RData')
  print('High quality')
} else{
  print('Nothing done, choose quality')
}

# 3. Save data ------------------------------------------------------------

