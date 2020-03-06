library(sapfluxnetr)
library(tidyverse)

# This scripts only needs to be run once
# If there are changes in the sfn version used, this needs to be changed 
# and the script re-run

# 1. Corrections -------------------------------------------------------------
# v. 0.1.3. 
# commented out if 0.1.4 is used

# # Delete ESP_PRA
# for (subfolder in c('plant','sapwood','leaf')){
#   folder <- file.path('data/0.1.3/RData',subfolder)
#  if(file.exists(file.path(folder,'ESP_PRA.RData'))) file.remove(file.path(folder,'ESP_PRA.RData'))
# }
# 
# 
# # Fix sites with incorrect pl_codes
# # Sources data_corrections
# for (subfolder in c('plant','sapwood','leaf')){
#   folder <- file.path('data/0.1.3/RData',subfolder)
#   source('data_corrections.R')
# }
# 

# v. 0.1.4

# # 2. Write metadata -------------------------------------------------------

sfn_metadata_plant <- read_sfn_metadata(folder = 'data/0.1.4/RData/plant', .write_cache = TRUE)
sfn_metadata_sapwood <- read_sfn_metadata(folder = 'data/0.1.4/RData/sapwood', .write_cache = TRUE)
sfn_metadata_leaf <- read_sfn_metadata(folder = 'data/0.1.4/RData/leaf', .write_cache = TRUE)






