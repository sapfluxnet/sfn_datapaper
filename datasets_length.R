# loading future package
#library(future)
library(sapfluxnetr)
library(purrr)
library(tidyverse)
library(ggrepel)

path.plant <- file.path('data/0.1.3/RData/plant')
path.sapwood <- file.path('data/0.1.3/RData/sapwood')


#plan('sequential')
# Test using Can Balasc data
#esp_can <- read_sfn_data('ESP_CAN',folder=path.plant)
#esp_val_sor <- read_sfn_data('ESP_VAL_SOR',folder=path.plant)

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


get_ntrees_day2<- function(sfn_data_obj,n_threshold=0){
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
sapwood_sites <- read_sfn_data(sfn_sites_in_folder(path.sapwood),folder=path.sapwood)


sapwood_only_sites<- setdiff(names(sapwood_sites),names(plant_sites))

# setting the plan


# get n_trees for all datasets, palnt and sapwood
plant_sites_duration<-map(.x=plant_sites, .f=get_ntrees_day)
  sapwood_sites_duration<-map(.x=sapwood_sites[sapwood_only_sites], .f=get_ntrees_day)

  # clean memory
  
  rm(plant_sites, sapwood_sites)
  
  
# transform to data frame
# TODO: add date of initiation and end of dataset for easier plotting of labels

plant_sites_duration %>% 
  map_dfr(~as_tibble(.)) %>% 
  mutate(TIMESTAMP2=lubridate::date(TIMESTAMP),
         n_trees_class = case_when(
           n_trees==0 ~ 'no data',
           n_trees>0 & n_trees<4 ~ '< 4',
           n_trees>4 & n_trees<11 ~'4-10',
           n_trees>10 & n_trees<21 ~'11-20',
           n_trees>20 & n_trees<31 ~'21-30',
           n_trees>30 & n_trees<41 ~'31-40',
           n_trees>40 & n_trees<51 ~'41-50',
           TRUE ~ '> 50'
         ),
         country=sapply(strsplit(si_code,"_"),"[[",1),
                num_code = as.integer(factor(si_code))) %>% 
  arrange(si_code,TIMESTAMP) %>% 
  group_by(si_code) %>% 
  add_tally() %>% 
  mutate( ini_date = TIMESTAMP[min(which(!is.na(n_trees) & !is.na(n_trees)))],
          end_date = TIMESTAMP[max(which(!is.na(n_trees) & !is.na(n_trees)))],
          index_days = seq_along(n),
          duration = lubridate::interval(ini_date,end_date)/86400/365) -> plant_sites_df



sapwood_sites_duration %>% 
map_dfr(~as_tibble(.)) %>% 
  mutate(TIMESTAMP2=lubridate::date(TIMESTAMP),
         n_trees_class = case_when(
           n_trees==0 ~ 'no data',
           n_trees>0 & n_trees<4 ~ '< 4',
           n_trees>4 & n_trees<11 ~'4-10',
           n_trees>10 & n_trees<21 ~'11-20',
           n_trees>20 & n_trees<31 ~'21-30',
           n_trees>30 & n_trees<41 ~'31-40',
           n_trees>40 & n_trees<51 ~'41-50',
           TRUE ~ '> 50'
         ),
         country=sapply(strsplit(si_code,"_"),"[[",1),
         num_code = as.integer(factor(si_code))) %>% 
  arrange(si_code,TIMESTAMP) %>% 
  group_by(si_code) %>% 
  add_tally() %>% 
  mutate( ini_date = TIMESTAMP[min(which(!is.na(n_trees) & !is.na(n_trees)))],
          end_date = TIMESTAMP[max(which(!is.na(n_trees) & !is.na(n_trees)))],
          index_days = seq_along(n),
          duration = lubridate::interval(ini_date,end_date)/86400/365) -> sapwood_sites_df



plant_sites_df %>% 
  bind_rows(sapwood_sites_df) %>% 
  arrange(si_code)-> datasets_duration
datasets_duration$n_trees_class_f<- factor(datasets_duration$n_trees_class, 
                                          levels=c('no data','< 4','4-10','11-20','21-30','31-40','41-50','> 50'))


View(plant_sites_duration[['ESP_VAL_SOR']])



# 
# plant_sites_df %>% 
#   arrange(si_code,TIMESTAMP) %>% 
#   group_by(si_code) %>% 
#   mutate( ini_date = TIMESTAMP[min(which(!is.na(n_trees) & !is.na(n_trees)))],
#           end_date = TIMESTAMP[max(which(!is.na(n_trees) & !is.na(n_trees)))],
#           duration = lubridate::interval(ini_date,end_date)/86400/365) %>% 
#           View()
# 


# Figure showing dataset duration, period, n_trees (log_scale)

ggplot(datasets_duration, aes(TIMESTAMP2, si_code)) +
  geom_raster(aes(fill = n_trees))+
  scale_fill_viridis_c(trans='log', labels=scales::trans_format('identity', function(x) ceiling(x)))
  
  scale_fill_gradientn(colours = rainbow(10),trans = "log",
                       labels=scales::trans_format('identity', function(x) ceiling(x)))+
  theme(axis.text.y=element_text(size=6), axis.text.x=element_text(size=16),axis.title.y=element_text(size=16))+
  labs(y='SAPFLUXNET Datasets (plant level)',x="")


  
  # Figure showing dataset duration, period, n_trees (categorised)
  # Annotation test
  
  annotation <- data.frame(
    x = as.Date("2005-01-01"),
    y=unique(plant_sites_df$num_code),
    label=unique(plant_sites_df$si_code)
  )
  
  

  
  datasets_duration %>% 
    distinct(si_code,num_code,duration) %>% 
    arrange(desc(duration))->datasets_duration_annotation
  
  ggplot(datasets_duration, aes(TIMESTAMP2, num_code,label=si_code)) +
    geom_raster(aes(fill = n_trees_class_f))+
    scale_y_continuous(breaks=seq(1,193,by=4))+
    scale_fill_viridis_d(direction=-1)+
    theme_classic()+
    theme(panel.background=element_rect(fill='lightgray',colour=NULL))+labs(x='')
  
  
  datasets_duration %>% 
    filter(si_code=='CAN_TUR_P74') %>% View()
  
  
  
  # Figure showing dataset duration and n_trees
   
  
  treeclass_pal<- c('lightgrey',viridis::viridis_pal(direction=-1)(7))
  
  data_duration_plot <- 
    datasets_duration %>% 
    ggplot(., 
           aes( y=index_days,x=reorder(num_code,n),fill = n_trees_class_f)) +
    geom_tile()+
    coord_flip()+
    scale_fill_manual(values = treeclass_pal)+
    theme(legend.position=c(.6,.3))+
    labs(x='')
  
  
  data_duration_plot_long <- 
    datasets_duration %>% 
    filter(n>=1000) %>% 
    ggplot(., 
           aes( y=index_days,x=reorder(num_code,n),fill = n_trees_class_f)) +
    geom_tile()+
    coord_flip()+
    scale_fill_manual(values = treeclass_pal)+
    theme(legend.position=c(.6,.3))+
    labs(x='')
  
  
  data_duration_plot_short <- 
    datasets_duration %>% 
    filter(n<1000) %>% 
    ggplot(., 
           aes( y=index_days,x=reorder(num_code,n),fill = n_trees_class_f)) +
    geom_tile()+
    coord_flip()+
    scale_fill_viridis_d(direction=-1)+
    theme(legend.position=c(.6,.3))+
    labs(x='')
  
  
  

  
  
data_period_plot <- ggplot(datasets_duration, aes( y=TIMESTAMP2,x=reorder(num_code,n))) +
                        geom_tile(fill = treeclass_pal[7])+
                        coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
guides(fill='none')+labs(x="")

cowplot::plot_grid(data_duration_plot, data_period_plot,labels=c('(a)','(b)'),rel_widths=c(1,0.4))



# TODO: add sapwood data, solve duration? add labels? ---------------------

save.image('dataset_duration_data.RData')
