library(tidyverse)
library(magrittr)
library(readxl)

source('metadata_wrangling.R')
library(sapfluxnetQC1)

# 1.Functions -------------------------------------------------------------

clim_to_biome <- function (dataf,temp='si_mat', precip='si_map',
                           merge_deserts=FALSE){
  
  if( is.na(temp) | is.na(precip)) {
    message('no precip or temp, no biome retrieved')
    dataf$biome_site <- NA
  }
  
  
  
  clim_point <- sp::SpatialPoints(data.frame(x = dataf[[precip]], 
                                             y = dataf[[temp]]))
  

  biome <- sp::over(clim_point, 
                    sapfluxnetQC1::qc_get_biomes_spdf(merge_deserts = merge_deserts))[[1]]
  
  dataf$biome_site <- droplevels(as.factor(biome))
  return(dataf)
}


# 2. Read current sfn clim and biome/vegtype data ----------------------------

sfn_allsites %>% 
  select(si_code,si_name,si_lat,si_long,si_elev,si_igbp,si_biome,si_mat,si_map)



# 3.  Biomes from local climate data -----------------------------------------


# Read site climate search (pràctiques Laura Homs)
sfn_climsite<- read_xlsx('resources/temp_precip_table.xlsx') %>% 
  select(si_code,MAT,MAP) %>% 
 modify_at(c('MAT','MAP'),as.double)

# Add biomes retrieved from LOCAL climate data
sfn_climsite %>% 
  filter(!is.na(MAT) & !is.na(MAP))%>%   
  clim_to_biome(temp='MAT',precip='MAP')->sfn_climsite_biomes


# 4. Alternative biome classification from ecorregions (Víctor F) --------

# Read alternative biome classification from ecorregions (Víctor F)
biomes_ecoreg <- read_csv('resources/biome_comparation.csv') %>% 
  dplyr::rename(biome_ecoreg=biome)

# 5. Join -----------------------------------------------------------------

sfn_allsites %>% 
  select(si_code,si_name,si_lat,si_long,si_elev,si_igbp,si_biome,si_mat,si_map) %>% 
  left_join(biomes_ecoreg) %>% 
  left_join(sfn_climsite_biomes) %>% 
  select(si_code,si_name,si_lat,si_long,si_elev,si_igbp,si_mat,si_map,MAT,MAP,si_biome,biome_whittaker,biome_ecoreg,biome_site) ->
    sfn_biomecheck

View(sfn_biomecheck)


write_csv(sfn_biomecheck,'docs/sfn_biomecheck.csv')
