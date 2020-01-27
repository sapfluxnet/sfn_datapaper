library(sapfluxnetr)
library(purrr)
library(tidyverse)
library(ggrepel)

path.plant <- file.path('data/0.1.3/RData/plant')
path.sapwood <- file.path('data/0.1.3/RData/sapwood')

# 1. Functions ------------------------------------------------------------
source('R/sfn_datapaper_functions.R')

# 2. Read sfn objects and run functions (time, memory) --------------------

# read all plant sites

plant_sites <- read_sfn_data(sfn_sites_in_folder(path.plant),folder=path.plant)
sapwood_sites <- read_sfn_data(sfn_sites_in_folder(path.sapwood),folder=path.sapwood)

# which sites only have sapwood-level data
sapwood_only_sites<- setdiff(names(sapwood_sites),names(plant_sites))

# get n_trees for all datasets, palnt and sapwood
plant_sites_duration<-map(.x=plant_sites, .f=get_ntrees_day)
  sapwood_sites_duration<-map(.x=sapwood_sites[sapwood_only_sites], .f=get_ntrees_day)

# clean memory

rm(plant_sites, sapwood_sites)


# 3. Transform dataset duration to data frame -----------------------------

# plant
plant_sites_duration %>% 
  map_dfr(~as_tibble(.)) %>% 
  mutate(TIMESTAMP2=lubridate::date(TIMESTAMP),
         n_trees_class = case_when(
           n_trees==0 ~ '0',
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


# sapwood
sapwood_sites_duration %>% 
map_dfr(~as_tibble(.)) %>% 
  mutate(TIMESTAMP2=lubridate::date(TIMESTAMP),
         n_trees_class = case_when(
           n_trees==0 ~ '0',
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

# combine plant and sapwood

plant_sites_df %>% 
  bind_rows(sapwood_sites_df) %>% 
  arrange(si_code)-> datasets_duration

# create categorical variable for ntrees
datasets_duration$n_trees_class_f<- factor(datasets_duration$n_trees_class, 
                                          levels=c('0','< 4','4-10','11-20','21-30','31-40','41-50','> 50'))
# Annotation data

datasets_duration %>% 
  ungroup() %>% 
  distinct(si_code,num_code,duration,.keep_all = TRUE) %>% 
  arrange(desc(duration)) %>% 
  mutate(rank=dplyr::row_number(desc(duration)),
         label_pos=0.8*n) -> duration_annotation

# 4. Plots ----------------------------------------------------------------

  # Figure showing dataset duration and n_trees
   
  # Palette for dataset duration plot
  treeclass_pal<- c('lightgrey',viridis::viridis_pal(direction=-1)(7))
  
  data_duration_plot<- 
    datasets_duration %>% 
    ggplot(., 
           aes( y=index_days,x=reorder(num_code,n),fill = n_trees_class_f)) +
    geom_tile()+
    coord_flip()+
    scale_fill_manual(values = treeclass_pal)+
    labs(x='',y='Number of days',fill='Number of trees')+
    theme_light()+
      theme(legend.position=c(.3,.2),
            axis.title.x=element_text(size=14),axis.text.x=element_text(size=12),
            axis.text.y=element_blank())+
            
      
    geom_label_repel(data=duration_annotation %>% 
                filter(rank%in%1:30),aes(x=reorder(num_code,n),y=0.9*n,label=si_code),inherit.aes=FALSE,
                ylim=c(3800,6000),direction='y',segment.color='darkgrey',force=0.65,size=3.4,label.size=0.03,
                
                # WARNING:If size is set to 3, the final plot only shows some, not all, labels ??
                
                nudge_x=duration_annotation %>% filter(rank%in%1:30) %>% arrange(desc(n)) %>% pull(num_code)-50)
    
  
data_period_plot <- ggplot(datasets_duration, aes( y=TIMESTAMP2,x=reorder(num_code,n))) +
                        geom_tile(fill = treeclass_pal[7])+
                        coord_flip()+
  theme_light()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
guides(fill='none')+labs(x="",y='Year')+
  theme(axis.title.x=element_text(size=14),axis.text.x=element_text(size=12))

fooplot<- cowplot::plot_grid(data_duration_plot, data_period_plot,labels=c('(a)','(b)'),rel_widths=c(1,0.4))

ggsave(filename='duration.png',plot=fooplot,units='cm',height=18,width=27)

# TODO: add sapwood data, solve duration? add labels? ---------------------

save.image('dataset_duration_data.RData')
