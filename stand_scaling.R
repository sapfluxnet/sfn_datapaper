# Figure: ntrees, perc area basal, etc

# Complex figure representing per aggregated biome ------------------------
# representing, per dataset:
# bars: basal area and sp composition in % 
# bar lines: in black if plant level data available
# numbers: n trees and n species per dataset
# site codes and species names
# Idea: 4 panels med, temp, trop-dry, bor-tundra
# Idea: colour of site code according to biome



# Preparing data ----------------------------------------------------------


# Temperate
# filter out RUS_POG_VAR

species_temperate <- dataset_trees_sp %>% 
  dplyr::filter(si_biome=='Temperate forest') %>% 
  mutate(sp_basal_area_perc=ifelse(si_code=='RUS_POG_VAR',NA,sp_basal_area_perc),
         percab_measured=ifelse(si_code=='RUS_POG_VAR',0,percab_measured))

sites_temperate <- species_temperate %>% 
  distinct(si_code,.keep_all =TRUE)


# Mediterranean

species_mediterranean<- dataset_trees_sp %>% 
  dplyr::filter(si_biome=='Mediterranean') 

sites_mediterranean <- species_mediterranean %>% 
  distinct(si_code,.keep_all =TRUE)

## Tropical and dry

trop_dry <- c('Subtropical desert', 'Temperate grassland desert','Tropical forest savanna','Temperate rain forest','Tropical rain forest')

species_dry_trop<- dataset_trees_sp %>% 
  dplyr::filter(si_biome%in%trop_dry)

sites_dry_trop <- species_dry_trop %>% 
  distinct(si_code,.keep_all =TRUE)


## Boreal forest and tundra

bor_tundra <- c('Boreal forest','Tundra')

species_bor_tundra<- dataset_trees_sp %>% 
  dplyr::filter(si_biome%in%bor_tundra)

sites_bor_tundra <- species_bor_tundra %>% 
  distinct(si_code,.keep_all =TRUE)



# Plots ------------------------------------------------------------------
# Temperate - plot
ggplot() +
  # geom_bar(aes(y = percab_measured, x = fct_reorder(si_code,percab_measured), col = typeplant), size=1.5,
  #                    data = sites_temperate, stat="identity")+
  geom_bar(aes(y = sp_basal_area_perc, x = fct_reorder(si_code,sp_basal_area_perc, sum, na.rm = TRUE), fill = sp_name), 
           data = species_temperate, stat="identity") +  
  
  labs(title = 'a) Temperate forest', x="", y="% of basal area measured with sap flow")+
  ggplot2::ylim(0,125)+
  scale_colour_manual(values=c("black",NA))+
  
  theme(legend.text = element_text(size=12),legend.position="top", legend.box = "horizontal",legend.title=element_blank(),
        
        axis.title.y=element_text(size=20),axis.text.y=element_text(size=16),
        plot.title=element_text(hjust=0,size=20), axis.text.x = element_text(angle = 90, hjust = 1)) +    
  geom_text(data=sites_temperate, aes(y = 115, x = fct_reorder(si_code,percab_measured), label = ceiling(nspecies)), size=4)+
  geom_text(data=sites_temperate, aes(y = 125, x = fct_reorder(si_code,percab_measured), label = ceiling(total_ntrees)), size=4.5,col='blue')+
  guides(colour=FALSE,fill = guide_legend(ncol = 10))


# Mediterranean - plot


ggplot() +  geom_bar(aes(y = percab_measured, x = fct_reorder(si_code,percab_measured), col = typeplant), size=1.5,
                     data = sites_mediterranean, stat="identity")+
  # geom_bar(aes(y = sp_basal_area_perc, x = fct_reorder(si_code,percab_measured), fill = sp_name), 
  #          data = species_mediterranean, stat="identity") +  
  # 
  # labs(title = 'b) Mediterranean', x="", y="% of basal area measured with sap flow")+
  # ggplot2::ylim(0,125)+
  # scale_colour_manual(values=c("black",NA))+
  
  theme(legend.text = element_text(size=12),legend.position="top", legend.box = "horizontal",legend.title=element_blank(),
        
        axis.title.y=element_text(size=20),axis.text.y=element_text(size=16),
        plot.title=element_text(hjust=0,size=20), axis.text.x = element_text(angle = 90, hjust = 1)) +    
  geom_text(data=sites_mediterranean, aes(y = 115, x = fct_reorder(si_code,percab_measured), label = ceiling(nspecies)), size=4)+
  geom_text(data=sites_mediterranean, aes(y = 125, x = fct_reorder(si_code,percab_measured), label = ceiling(total_ntrees)), size=4.5,col='blue')+
  guides(colour=FALSE,fill = guide_legend(ncol = 10))



# Tropica and dry - plot


ggplot() +  geom_bar(aes(y = percab_measured, x = fct_reorder(si_code,percab_measured), col = typeplant), size=1.5,
                     data = sites_dry_trop, stat="identity")+
  geom_bar(aes(y = sp_basal_area_perc, x = fct_reorder(si_code,percab_measured), fill = sp_name), 
           data = species_dry_trop, stat="identity") +  
  
  labs(title = 'c) Tropical and dry', x="", y="% of basal area measured with sap flow")+
  ggplot2::ylim(0,125)+
  scale_colour_manual(values=c("black",NA))+
  
  theme(legend.text = element_text(size=11),legend.position="top", legend.box = "horizontal",legend.title=element_blank(),
        
        axis.title.y=element_text(size=20),axis.text.y=element_text(size=16),
        plot.title=element_text(hjust=0,size=20), axis.text.x = element_text(angle = 90, hjust = 1)) +    
  geom_text(data=sites_dry_trop, aes(y = 115, x = fct_reorder(si_code,percab_measured), label = ceiling(nspecies)), size=4)+
  geom_text(data=sites_dry_trop, aes(y = 125, x = fct_reorder(si_code,percab_measured), label = ceiling(total_ntrees)), size=4.5,col='blue')+
  guides(colour=FALSE,fill = guide_legend(ncol = 10))


# Boreal and tundra

ggplot() +  geom_bar(aes(y = percab_measured, x = fct_reorder(si_code,percab_measured), col = typeplant), size=1.5,
                     data = sites_bor_tundra, stat="identity")+
  geom_bar(aes(y = sp_basal_area_perc, x = fct_reorder(si_code,percab_measured), fill = sp_name), 
           data = species_bor_tundra, stat="identity") +  
  
  labs(title = 'd) Boreal forest and Tundra', x="", y="% of basal area measured with sap flow")+
  ggplot2::ylim(0,125)+
  scale_colour_manual(values=c("black",NA))+
  
  theme(legend.text = element_text(size=12),legend.position="top", legend.box = "horizontal",legend.title=element_blank(),
        
        axis.title.y=element_text(size=20),axis.text.y=element_text(size=16),
        plot.title=element_text(hjust=0,size=20), axis.text.x = element_text(angle = 90, hjust = 1)) +    
  geom_text(data=sites_bor_tundra, aes(y = 115, x = fct_reorder(si_code,percab_measured), label = ceiling(nspecies)), size=4)+
  geom_text(data=sites_bor_tundra, aes(y = 125, x = fct_reorder(si_code,percab_measured), label = ceiling(total_ntrees)), size=4.5,col='blue')+
  guides(colour=FALSE,fill = guide_legend(ncol = 10))



