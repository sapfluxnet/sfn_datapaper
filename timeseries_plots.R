library(sapfluxnetr)
library(purrr)
library(tidyverse)
library(FREddyPro)


path.plant <- file.path('data/0.1.3/RData/plant')
path.sapwood <- file.path('data/0.1.3/RData/sapwood')


## Close any previously open graphic devices
graphics.off()

## Load the data
data(fluxes)

## Clean the fluxes
fluxes=cleanFluxes(fluxes,sdCor=TRUE,sdTimes=3,timesList=3,distCor=TRUE,
                   thresholdList=list(H=c(-100,1000),LE=c(-100,1000)))

## Plot the fingerprint plot
plotFingerprint(fluxes$co2_flux,fluxes$yday,fluxes$hour,step=1,main='CO'[2]~' flux',
                key.title=title(main="umol m"^2~"s"^-1~"",adj=0.2))


# Test using Can Balasc data
esp_can <- read_sfn_data('ESP_CAN',folder=path.sapwood)
esp_val_sor <- read_sfn_data('ESP_VAL_SOR',folder=path.sapwood)

plotFingerprint(var, doy, hour, step = 2, xlab = "Hour",
                ylab="Day or Year", ...)



esp_can %>% 
  get_sapf_data() %>% 
  mutate(year=lubridate::year(TIMESTAMP),
         doy = lubridate::yday(TIMESTAMP),
         hour = lubridate::hour(TIMESTAMP)+lubridate::minute(TIMESTAMP)/60) %>% 
  plotFingerprint(var=.$ESP_CAN_Qpu_Js_1,doy=.$doy,hour=.$hour)
         


esp_can %>% 
  get_sapf_data() %>% 
  mutate(year=lubridate::year(TIMESTAMP),
         doy = lubridate::yday(TIMESTAMP),
         hour = lubridate::hour(TIMESTAMP)+lubridate::minute(TIMESTAMP)/60) %>% 
  ggTimeSeries::ggplot_calendar_heatmap(dtDateValue = .,cDateColumnName = 'TIMESTAMP',cValueColumnName ='ESP_CAN_Qpu_Js_1' )



esp_can %>% 
  get_sapf_data() %>% 
  mutate(year=lubridate::year(TIMESTAMP),
         doy = lubridate::yday(TIMESTAMP),
         hour = lubridate::hour(TIMESTAMP)) %>% 
  ggplot(data=.,aes(x=hour,y=doy,fill=ESP_CAN_Qpu_Js_1))+
  geom_tile(color= "white",size=0.1) + 
  viridis::scale_fill_viridis(name="sf",option ="C")+
  facet_grid(~year)

esp_can %>% 
  get_sapf_data() %>% 
  mutate(year=lubridate::year(TIMESTAMP),
         doy = lubridate::yday(TIMESTAMP),
         hour = lubridate::hour(TIMESTAMP)) %>% 
  ggplot(data=.,aes(x=hour,y=doy,fill=ESP_CAN_Pha_Js_9))+
  geom_tile(color= "white",size=0.1) + 
  viridis::scale_fill_viridis(name="sf",option ="C")+
  facet_grid(~year)


esp_can %>% 
  get_env_data() %>% 
  mutate(year=lubridate::year(TIMESTAMP),
         doy = lubridate::yday(TIMESTAMP),
         hour = lubridate::hour(TIMESTAMP)) %>% 
  ggplot(data=.,aes(x=hour,y=doy,fill=swc_shallow))+
  geom_tile(color= "white",size=0.1) + 
  viridis::scale_fill_viridis(name="sf",option ="C")+
  facet_grid(~year)

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
    full_join(select(plmdata,pl_code,pl_species),by=c('tree'='pl_code')) %>% 
    mutate(year=lubridate::year(TIMESTAMP),
           doy = lubridate::yday(TIMESTAMP),
           hour = lubridate::hour(TIMESTAMP)) %>%

  ggplot(.,aes(x=hour,y=doy,fill=sf))+
    geom_tile(color= "white",size=0.1) + 
    viridis::scale_fill_viridis(name="sf",option ="C")+
    facet_grid(year~pl_species)
  
}

sfn_finger(esp_can)

esp_can  %>% 
  get_sapf_data() %>% 
  gather(-TIMESTAMP,key='tree',value='sf')->foo

esp_can  %>% 
  get_plant_md()-> faa

foo %>% 
  full_join(select(faa,pl_code,pl_species),by=c('tree'='pl_code'))
