library(sapfluxnetr)
library(purrr)
library(tidyverse)

# Negate in

`%ni%` = Negate(`%in%`)

# Function to get number of trees with measurements per day
# To consider a day with measurements uses a threshold of number
# of timesteps


get_ntrees_day<- function(sfn_data_obj,n_threshold=0){
  sfn_data_obj %>% 
    sfn_metrics(period="1 day",
                .funs = list(~sum(!is.na(.))),
                solar=FALSE,
                interval='general') %>% magrittr::extract2('sapf') %>% 
    dplyr::select(-TIMESTAMP_coll) %>% 
    mutate(n_trees =rowSums(.[-1]>n_threshold),
           si_code=get_si_code(sfn_data_obj)) %>%
    dplyr::select(si_code,TIMESTAMP,n_trees) 
  
}


# TODO:
# average per species or select trees?

sfn_finger<- function(sfn_data_obj,years=c(2011,2012)){
  
  sfn_data_obj %>% 
    get_sapf_data() %>% 
    gather(-TIMESTAMP,key='tree',value='sf')->sfdata
  
  sfn_data_obj %>%  
    get_plant_md() -> plmdata
  
  sfdata %>% 
    full_join(dplyr::select(plmdata,pl_code,pl_species),by=c('tree'='pl_code')) %>% 
    mutate(year=lubridate::year(TIMESTAMP),
           doy = lubridate::yday(TIMESTAMP),
           hour = lubridate::hour(TIMESTAMP)) %>%
    
    ggplot(.,aes(x=hour,y=doy,fill=sf))+
    geom_raster(interpolate=TRUE)+
    # geom_tile(color= "white",size=0.01) + 
    viridis::scale_fill_viridis(name="sf",option ="C")+
    facet_grid(year~pl_species) +
    theme_light() +
    theme(axis.text.x = element_blank(),axis.text.y = element_text(size = 16),
          strip.background = element_rect(fill = 'white', colour = 'darkgray'),
          strip.text = element_text(size = 16, colour = 'black', face = 'bold'))
  
  
}


# sfn per species

sfn_finger_species<- function(sfn_data_obj,
                              years=1990:2020,
                              species= get_species_md(sfn_data_obj) %>% pull(sp_name) %>% unique()){
  
  sfn_data_obj %>% 
    get_sapf_data() %>% 
    gather(-TIMESTAMP,key='tree',value='sf')->sfdata
  
  sfn_data_obj %>%  
    get_plant_md() -> plmdata
  
  sfdata %>% 
    full_join(dplyr::select(plmdata,pl_code,pl_species),by=c('tree'='pl_code')) %>% 
    group_by(pl_species) %>% 
    mutate(year=lubridate::year(TIMESTAMP),
           doy = lubridate::yday(TIMESTAMP),
           hour = lubridate::hour(TIMESTAMP)) %>% 
    dplyr::filter(year%in%years, pl_species%in%species) %>% 
    group_by(pl_species,year,doy,hour) %>% 
    mutate(sf_species=mean(sf,na.rm=TRUE)) %>%
    distinct(pl_species,doy,hour,.keep_all = TRUE) %>%
    
    ggplot(.,aes(x=hour,y=doy,fill=sf_species))+
    geom_raster(interpolate=TRUE)+
    # geom_tile(color= "white",size=0.01) + 
    viridis::scale_fill_viridis(name="Sap flow\ndensity\n[cm3cm-2h-1]",
    option ="C")+
    xlab('Time of day') + ylab('Day of year')+
    facet_grid(year~pl_species)+
    theme_light() +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16),
          strip.background = element_rect(fill = 'white', colour = 'darkgray'),
          strip.text.x = element_text(size = 12, colour = 'black', face = 'italic'),
          strip.text.y = element_text(size = 12, colour = 'black'))
  
  
}


