# library(leaflet)
# if(!require(devtools)){install.package("devtools")}
# devtools::install_github("khufkens/MODISTools")
# library("MODISTools")
# 
# # Contributions map
# 
# leaflet(data = sfn_allsites) %>%
#   addTiles(urlTemplate = 'http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
#            attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community',
#            options = tileOptions(noWrap = FALSE)) %>%
#   setView(lng = 15, lat = 35, zoom = 2) %>%
#   clearMarkers() %>%
#   addCircleMarkers(lng = ~si_long, lat = ~si_lat, layerId = ~si_code,labelOptions(noHide = T),
#                    radius = 5,
#                    fillOpacity = 80.7,
#                    fillColor = "#FDE725",
#                    stroke = FALSE)
# 




# 0. Requirements ----------------------------------------------------------

library(raster)
library(ggspatial)
library(ggrepel)
library(tidyverse)
library(cowplot)


plot_quality <- 'draft'

# 1. Crowther forest density map ------------------------------------------


# 1.1. load map -----------------------------------------------------------

file.exists('maps/Crowther_Nature_Files_Revision_01_WGS84_GeoTiff/Crowther_Nature_Biome_Revision_01_WGS84_GeoTiff.tif')
crowther <- raster('maps/Crowther_Nature_Files_Revision_01_WGS84_GeoTiff/Crowther_Nature_Biome_Revision_01_WGS84_GeoTiff.tif')

# 1.2. Reduce raster resolution -------------------------------------------

# Using raster::aggregate, other options available
# Two versions, 0.5 deg and 0.1 deg
# this takes some time
crowther_0_1 <- aggregate(crowther, fact = 0.1/res(crowther)) # aggregate output
crowther_0_5 <- aggregate(crowther, fact = 0.5/res(crowther)) # aggregate output



# 1.3. Reclassification to binary raster, forest vs non-forest ------------


# Use some density threshold. Check paper for units, I think they're trees/pixel
crowther_rec<- reclassify(crowther,c(0,20000,0,20000,Inf,1))
crowther_rec_0_1<- reclassify(crowther_0_1,c(0,20000,0,20000,Inf,1))
crowther_rec_0_1f<- as.factor(round(crowther_rec_0_1))

crowther_rec_0_5f <- aggregate(crowther_rec_0_1f, fact = 0.5/res(crowther_rec_0_1f)) # aggregate output

plot(crowther_rec_0_5f,col=c("white","green"))
plot(crowther_rec_0_1f,col=c("white","green"))
plot(crowther_rec,col=c("white","green"))


# Still unsolved raster as discrete values
rat <- levels(crowther_rec_0_1f)[[1]]
rat$landcover <- c('non-forest','forest')
levels(crowther_rec_0_1f) <- rat

# 
ggplot() +
  layer_spatial(crowther_rec_0_1f) +
  scale_manual_colour()


# 1.4. Create base maps ---------------------------------------------------

# Base reclassified

globforest_rec_0_1 <- ggplot() +
  layer_spatial(crowther_rec_0_1f) +
  scale_fill_gradientn(na.value = "white",colours=c("burlywood","green","darkgreen")) +
  coord_sf()

# Base reclassified 0.5

globforest_rec_0_5 <- ggplot() +
  layer_spatial(crowther_rec_0_5f) +
  scale_fill_gradientn(na.value = "white",colours=c("burlywood","green","darkgreen")) +
  coord_sf()

# Base map 0.1

globforest_0_1 <-ggplot() +
  layer_spatial(crowther_0_1) +
  scale_fill_gradientn(na.value = "white",colours=c("burlywood","green","darkgreen")) +
  coord_sf()

## Base map 0.5

globforest_0_5 <- ggplot() +
  layer_spatial(crowther_0_5,aes(fill=stat(band1))) +
  scale_fill_gradientn(na.value = "white",colours=c("burlywood1","darkgreen")) +
  coord_sf()



# 2. Site labels ----------------------------------------------------------



# 2.1. Select countries to plot -------------------------------------------

countries_sfn <- unique(sapply(strsplit(sfn_allsites$si_code,"_"),"[[",1))
countries_label <- c('ARG','BRA','COL','CRI','CHN','GUF','IDN','ISR','KOR','MDG','MEX','NZL','RUS','SEN','THA','UZB')
countries_europe <- c('AUT','CHE','CZE','DEU','ESP','FIN','FRA','GBR','HUN','ITA','NLD','PRT','SWE')
countries_america <- c('CAN','USA')
countries_austral <- c('AUS')
countries_africa <- c('MDG','ZAF')



# 2.2. Add numeric codes --------------------------------------------------

sfn_allsites_country<- sfn_allsites %>% 
  mutate(country=sapply(strsplit(si_code,"_"),"[[",1),
         num_code = as.integer(factor(si_code)))



# 3. Create maps ----------------------------------------------------------



# 3.1. Choose quality -----------------------------------------------------
# 'high' quality for definitive fig, 'draft' is faster

    plot_quality <- 'draft'
    
    if (plot_quality=='draft') {
      globforest <- globforest_rec_0_5
      print('Draft quality')
    } else if (plot_quality=='high') {
      globforest <- globforest_rec_0_1
      print('High quality')
    } else{
      print('Nothing done, choose quality')
    }
    
    # 3.2. Create maps --------------------------------------------------------
    
    
    # world
    sfnsites_world <- globforest  +
      geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
      geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_label), 
                      aes(si_long, si_lat, label = num_code), size = 3,
                      segment.alpha=0.5,  nudge_x = -0.35,direction = "y",hjust = 1,
                    box.padding = unit(0.1, 'lines'), force = 0.5)+geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)
    
    # europe
    sfnsites_europe<- globforest  +
      geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
      geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_europe), 
                      aes(si_long, si_lat, label = num_code), size = 3,
                      segment.alpha=0.5,  nudge_y = -0.35,direction = "y",hjust = 1,
                      box.padding = unit(0.1, 'lines'), force = 0.5)+
    coord_sf(xlim = c(-20, 40), ylim = c(32, 67), expand = FALSE)+
      geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)
    
    # america
    sfnsites_america<- globforest  +
      geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
      geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_america), 
                      aes(si_long, si_lat, label = num_code), size = 3,
                      segment.alpha=0.5,  nudge_x = -0.35,direction = "y",hjust = 1,
                      box.padding = unit(0.1, 'lines'), force = 0.5)+
      coord_sf(xlim = c(-130, -60), ylim = c(30, 55), expand = FALSE)+
      geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)
    
    # austral
    sfnsites_austral<- globforest  +
      geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
      geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_austral), 
                      aes(si_long, si_lat, label = num_code), size = 3,
                      segment.alpha=0.5,  nudge_x = -0.35,
                      box.padding = unit(0.1, 'lines'), force = 0.5)+
      coord_sf(xlim = c(110, 162), ylim = c(-10, -50), expand = FALSE)+
      geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)
    
    # africa
    sfnsites_africa<- globforest  +
      geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
      geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_africa), aes(si_long, si_lat, label = num_code), size = 3,
                      box.padding = unit(0.1, 'lines'), force = 0.5)+
      coord_sf(xlim = c(10, 50), ylim = c(-10, -40), expand = FALSE)+
      geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)
    
    
    
    # 3.3. Create figure ------------------------------------------------------
    
    
    row_1 <- plot_grid(sfnsites_world, labels='(a)')
    row_2 <- plot_grid(sfnsites_europe, sfnsites_america,labels=c('(b)','(c)'),rel_widths=c(1,1))
    row_3 <- plot_grid(sfnsites_austral, sfnsites_africa, labels=c('(d)','(e)'))
    
    
    sfn_sitesmap <- plot_grid(row_1, row_2,row_3, labels=c('', '',''), ncol=1)


save_plot('./output/figs/sfn_sitesmap.tiff', sfn_sitesmap)
save_plot('./output/figs/sfn_sitesmap.eps', sfn_sitesmap)


# 4. Conditioned maps -----------------------------------------------------

# TODO: set the symbol colour black
# world

# igbp
globforest  +
  geom_point(data=sfn_allsites,
             aes(x=si_long,y=si_lat,col=si_igbp))+
  
  guides(fill='none')+xlab(NULL)+ylab(NULL)
# type
globforest  +
  geom_point(data=sfn_sites_type,
             aes(x=si_long,y=si_lat,col=type))+
  
  guides(fill='none')+xlab(NULL)+ylab(NULL)

globforest  +
  geom_point(data=sfn_sites_nspecies,
             aes(x=si_long,y=si_lat,col=nspecies))+
  
  guides(fill='none')+xlab(NULL)+ylab(NULL)

globforest  +
  geom_point(data=sfn_sites_nspecies,
             aes(x=si_long,y=si_lat,col=ntrees))+
  
  guides(fill='none')+xlab(NULL)+ylab(NULL)


# TODO: alternative, show histograms
