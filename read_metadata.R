library(sapfluxnetr)
library(tidyverse)

# 1. Corrections -------------------------------------------------------------
# They only need to be applied once


# Delete ESP_PRA
for (subfolder in c('plant','sapwood','leaf')){
  folder <- file.path('data/0.1.3/RData',subfolder)
 if(file.exists(file.path(folder,'ESP_PRA.RData'))) file.remove(file.path(folder,'ESP_PRA.RData'))
}


# Fix sites with incorrect pl_codes
# Sources data_corrections
for (subfolder in c('plant','sapwood','leaf')){
  folder <- file.path('data/0.1.3/RData',subfolder)
  source('data_corrections.R')
}

# 2. Write metadata -------------------------------------------------------

sfn_metadata_plant <- read_sfn_metadata(folder = 'data/0.1.3/RData/plant', .write_cache = TRUE)
sfn_metadata_sapwood <- read_sfn_metadata(folder = 'data/0.1.3/RData/sapwood', .write_cache = TRUE)
sfn_metadata_leaf <- read_sfn_metadata(folder = 'data/0.1.3/RData/leaf', .write_cache = TRUE)






