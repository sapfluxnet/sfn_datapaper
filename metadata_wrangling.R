library(sapfluxnetr)
library(tidyverse)

# 1. Read metadata ---------------------------------------------------------------
# From previously written cache file

sfn_metadata_plant <- read_sfn_metadata(folder = 'data/0.1.3/RData/plant', .write_cache = FALSE)
sfn_metadata_sapwood <- read_sfn_metadata(folder = 'data/0.1.3/RData/sapwood', .write_cache = FALSE)
sfn_metadata_leaf <- read_sfn_metadata(folder = 'data/0.1.3/RData/leaf', .write_cache = FALSE)


# 2. Aggregate all datasets -----------------------------------------------

# Join all metadata regardless of having sap flow per sapwood or per plant


sfn_allsites<- sfn_metadata_plant[['site_md']] %>% 
  full_join(dplyr::select(sfn_metadata_sapwood[['site_md']],-si_remarks))

sfn_allstands<- sfn_metadata_plant[['stand_md']] %>% 
  full_join(sfn_metadata_sapwood[['stand_md']])

sfn_sitespecies<- sfn_metadata_plant[['species_md']] %>% 
  full_join(sfn_metadata_sapwood[['species_md']]) 

# Calculate number of trees and species

sfn_sites_nspecies <- sfn_sitespecies %>% 
  group_by(si_code) %>% 
  summarise(nspecies=length(sp_name),
            ntrees=sum(sp_ntrees)) %>% 
  left_join(sfn_allsites %>% dplyr::select(si_code,si_lat,si_long))

# Number of species
sfn_species<- sfn_sitespecies %>% 
  distinct(sp_name)

sfn_allplants<- sfn_metadata_plant[['plant_md']] %>% 
  full_join(sfn_metadata_sapwood[['plant_md']]) %>% 
  distinct(pl_code,.keep_all = TRUE)

sfn_env <- sfn_metadata_plant[['env_md']] %>% 
  full_join(sfn_metadata_sapwood[['env_md']])


# 3. Measurement type -------------------------------------------------
# plant, sapwood, leaf

sfn_sites_plsw <- sfn_metadata_plant[['site_md']] %>% 
  semi_join( dplyr::select(
    sfn_metadata_sapwood[['site_md']],
    -si_remarks)) %>% 
  mutate(type='plant,sapwood')


sfn_sites_pl <- sfn_metadata_plant[['site_md']] %>% 
  anti_join(dplyr::select(
    sfn_metadata_sapwood[['site_md']],
    -si_remarks)) %>% 
  mutate(type='plant')


sfn_sites_sw <- dplyr::select(sfn_metadata_sapwood[['site_md']],-si_remarks) %>% 
  anti_join(sfn_metadata_plant[['site_md']]) %>% 
  mutate(type='sapwood')

sfn_sites_leaf <- sfn_metadata_leaf[['site_md']] %>% 
  mutate(type='leaf,plant,sapwood')

sfn_sites1 <- sfn_sites_plsw %>% 
  full_join(sfn_sites_pl) %>% 
  full_join(sfn_sites_sw)  


# Measurement type: plant, sapwood, leaf
sfn_sites_type <- sfn_sites1 %>% 
  mutate(
    type=ifelse(si_code%in%sfn_sites_leaf$si_code,
                'leaf,plant,sapwood',type),
    typef=factor(type)
  )



# 4. Percentage basal area ------------------------------------------------

dataset_trees_sp <- sfn_sites_type %>%
  right_join(sfn_allstands) %>% 
  right_join(
    sfn_sitespecies %>%
      group_by(si_code) %>%
      mutate(total_ntrees = sum(sp_ntrees,na.rm=TRUE),
             nspecies = n_distinct(sp_name),
             percab_measured = sum(sp_basal_area_perc,na.rm=TRUE)),
    by='si_code'
  ) %>% 
    mutate(
      typeplant=ifelse(str_detect(type,'plant'),'plant',NA))


# Temperate
# filter out RUS_POG_VAR

species_temperate<- dataset_trees_sp %>% 
  dplyr::filter(si_biome=='Temperate forest') %>% 
  mutate(sp_basal_area_perc=ifelse(si_code=='RUS_POG_VAR',NA,sp_basal_area_perc),
         percab_measured=ifelse(si_code=='RUS_POG_VAR',0,percab_measured))

sites_temperate <- species_temperate %>% 
  distinct(si_code,.keep_all =TRUE)

# Box x species, not optimal

ggplot() + geom_bar(aes(y = sp_basal_area_perc, x = fct_reorder(si_code,percab_measured), fill = sp_name, col=typeplant), 
                   data = species_temperate, stat="identity") +  
  labs(title = 'Temperate forest', x="", y="% of basal area measured with sap flow")+
  ggplot2::ylim(0,125)+
  scale_colour_manual(values=c("red",NA))+
  
  theme(legend.text = element_text(size=12),legend.position="top", legend.box = "horizontal",legend.title=element_blank(),
        
        axis.title.y=element_text(size=20),axis.text.y=element_text(size=16),
        plot.title=element_text(hjust=1,size=20), axis.text.x = element_text(angle = 90, hjust = 1)) +    
  geom_text(data=sites_temperate, aes(y = 115, x = fct_reorder(si_code,percab_measured), label = ceiling(nspecies)), size=4)+
  geom_text(data=sites_temperate, aes(y = 125, x = fct_reorder(si_code,percab_measured), label = ceiling(total_ntrees)), size=4.5,col='blue')+
  guides(colour=FALSE,fill = guide_legend(ncol = 9))
  #annotate('text',label='Number of sap flow trees',x=15,y=175,fontface='bold',size=6,col='blue')+

# Ok but wrong order?


ggplot() +  geom_bar(aes(y = percab_measured, x = fct_reorder(si_code,percab_measured), col = typeplant), 
                     data = species_temperate, stat="identity")+
  geom_bar(aes(y = sp_basal_area_perc, x = fct_reorder(si_code,percab_measured), fill = sp_name), 
                    data = species_temperate, stat="identity") +  
  
  labs(title = 'Temperate forest', x="", y="% of basal area measured with sap flow")+
  ggplot2::ylim(0,125)+
  scale_colour_manual(values=c("black",NA))+
  
  theme(legend.text = element_text(size=12),legend.position="top", legend.box = "horizontal",legend.title=element_blank(),
        
        axis.title.y=element_text(size=20),axis.text.y=element_text(size=16),
        plot.title=element_text(hjust=1,size=20), axis.text.x = element_text(angle = 90, hjust = 1)) +    
  geom_text(data=sites_temperate, aes(y = 115, x = fct_reorder(si_code,percab_measured), label = ceiling(nspecies)), size=4)+
  geom_text(data=sites_temperate, aes(y = 125, x = fct_reorder(si_code,percab_measured), label = ceiling(total_ntrees)), size=4.5,col='blue')+
  guides(colour=FALSE,fill = guide_legend(ncol = 9))


# TODO: Dataset length --------------------------------------

foo<- species_temperate %>% 
  filter(si_code%in%c('BRA_CAM','USA_NWH'))
glimpse(foo)





