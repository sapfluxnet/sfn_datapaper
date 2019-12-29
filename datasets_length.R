# loading future package
library(future)

path.plant <- file.path('data/0.1.3/RData/plant')

sfn_sites_in_folder(path.plant)

# Test using Can Balasc data
esp_can <- read_sfn_data('ESP_CAN',folder=path.plant)
esp_val_sor <- read_sfn_data('ESP_VAL_SOR',folder=path.plant)

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

# read all plant sites

plant_sites <- read_sfn_data(sfn_sites_in_folder(path.plant),folder=path.plant)

# setting the plan
plan('sequential')

# get n_trees for all datasets
plant_sites<-map(.x=plant_sites, .f=get_ntrees_day)
  
# transform to data frame
plant_sites %>% 
  map_dfr(~as_tibble(.)) %>% 
  mutate(TIMESTAMP2=lubridate::date(TIMESTAMP))-> plant_sites_df

# View(plant_sites_df)

ggplot(plant_sites_df, aes(TIMESTAMP2, si_code)) +
  geom_raster(aes(fill = n_trees))+
  scale_fill_viridis_c(trans='log', labels=scales::trans_format('identity', function(x) ceiling(x)))
  
  scale_fill_gradientn(colours = rainbow(10),trans = "log",
                       labels=scales::trans_format('identity', function(x) ceiling(x)))+
  theme(axis.text.y=element_text(size=6), axis.text.x=element_text(size=16),axis.title.y=element_text(size=16))+
  labs(y='SAPFLUXNET Datasets (plant level)',x="")

