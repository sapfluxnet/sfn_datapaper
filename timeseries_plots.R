library(sapfluxnetr)
library(purrr)
library(tidyverse)
library(FREddyPro)

# Data paths

path.plant <- file.path('data/0.1.3/RData/plant')
path.sapwood <- file.path('data/0.1.3/RData/sapwood')

# Test using Can Balasc data
sfn_sites_in_folder(path.sapwood)

esp_can <- read_sfn_data('ESP_CAN',folder=path.sapwood)
esp_val_sor <- read_sfn_data('ESP_VAL_SOR',folder=path.sapwood)

foo <- read_sfn_data('CRI_TAM_TOW',folder=path.sapwood)

cowplot::plot_grid(
sfn_finger_species(esp_can),
sfn_finger_species(read_sfn_data('USA_WVF',folder=path.sapwood),years=1998,
                   species=c('Acer saccharum','Prunus serotina','Quercus alba','Quercus rubra')),

sfn_finger_species(read_sfn_data('CRI_TAM_TOW',folder=path.sapwood),years=2015,
                   species=c('Pouteria sp.','Inga sp.',
                                            'Eschweillera sp.','Mortoniodendron anisophyllum')),
ncol=1,labels=c('a)','b)','c)'),rel_heights = c(1,1,1))


sfn_finger_species(esp_can)           
                   

esp_can %>% get_species_md() %>% pull(sp_name) %>% unique()
                   esp_can %>% get_plant_md() %>% pull(pl_species) %>% unique()
# fingerprint plot --------------------------------------------------------

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
    facet_grid(year~pl_species)
  
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
    viridis::scale_fill_viridis(name="sf",option ="C")+
    facet_grid(year~pl_species)
  
}



sfn_finger_species(bra_san)



esp_can%>% 
  get_sapf_data() %>% 
  gather(-TIMESTAMP,key='tree',value='sf')->sfdata


esp_can %>%  
  get_plant_md() -> plmdata

sfdata %>% 
  full_join(dplyr::select(plmdata,pl_code,pl_species),by=c('tree'='pl_code')) %>% 
  mutate(year=lubridate::year(TIMESTAMP),
         doy = lubridate::yday(TIMESTAMP),
         hour = lubridate::hour(TIMESTAMP)) %>% 
  group_by(pl_species,doy,hour) %>% 
  mutate(sf_species=mean(sf,na.rm=TRUE)) %>%
  distinct(pl_species,doy,hour,.keep_all = TRUE)->foo

View(foo)



esp_can  %>% 
  get_sapf_data() %>% 
  gather(-TIMESTAMP,key='tree',value='sf')->foo

esp_can  %>% 
  get_plant_md()-> faa

foo %>% 
  full_join(select(faa,pl_code,pl_species),by=c('tree'='pl_code'))




# ## Close any previously open graphic devices
# graphics.off()
# 
# ## Load the data
# data(fluxes)
# 
# ## Clean the fluxes
# fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,timesList=3,distCor=TRUE,
#                    thresholdList=list(H=c(-100,1000),LE=c(-100,1000)))
# 
# ## Plot the fingerprint plot
# plotFingerprint(fluxes$co2_flux,fluxes$yday,fluxes$hour,step=1,main='CO'[2]~' flux',
#                 key.title=title(main="umol m"^2~"s"^-1~"",adj=0.2))

# esp_can %>% 
#   get_sapf_data() %>% 
#   mutate(year=lubridate::year(TIMESTAMP),
#          doy = lubridate::yday(TIMESTAMP),
#          hour = lubridate::hour(TIMESTAMP)) %>% 
#   ggplot(data=.,aes(x=hour,y=doy,fill=ESP_CAN_Qpu_Js_1))+
#   geom_tile(color= "white",size=0.1) + 
#   viridis::scale_fill_viridis(name="sf",option ="C")+
#   facet_grid(~year)
# 
# esp_can %>% 
#   get_sapf_data() %>% 
#   mutate(year=lubridate::year(TIMESTAMP),
#          doy = lubridate::yday(TIMESTAMP),
#          hour = lubridate::hour(TIMESTAMP)) %>% 
#   ggplot(data=.,aes(x=hour,y=doy,fill=ESP_CAN_Pha_Js_9))+
#   geom_tile(color= "white",size=0.1) + 
#   viridis::scale_fill_viridis(name="sf",option ="C")+
#   facet_grid(~year)
# 
# 
# esp_can %>% 
#   get_env_data() %>% 
#   mutate(year=lubridate::year(TIMESTAMP),
#          doy = lubridate::yday(TIMESTAMP),
#          hour = lubridate::hour(TIMESTAMP)) %>% 
#   ggplot(data=.,aes(x=hour,y=doy,fill=swc_shallow))+
#   geom_tile(color= "white",size=0.1) + 
#   viridis::scale_fill_viridis(name="sf",option ="C")+
#   facet_grid(~year)
