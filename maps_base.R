# 0. Requirements ----------------------------------------------------------

  library(raster)
  library(ggspatial)
  library(ggrepel)
  library(tidyverse)
  library(cowplot)
  
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
  
  
  # Use some density threshold; trial and error so far...
  # Check paper for units, I think they're trees/pixel
  crowther_rec<- reclassify(crowther,c(0,20000,0,20000,Inf,1))
  crowther_rec_0_1<- reclassify(crowther_0_1,c(0,20000,0,20000,Inf,1))
  crowther_rec_0_1f<- as.factor(round(crowther_rec_0_1))
  
  crowther_rec_0_5f <- aggregate(crowther_rec_0_1f, fact = 0.5/res(crowther_rec_0_1f)) # aggregate output
  
  plot(crowther_rec_0_5f,col=c("white","green"))
  plot(crowther_rec_0_1f,col=c("white","green"))
  plot(crowther_rec,col=c("white","green"))
  
  
  # # Still unsolved raster as discrete values
  # rat <- levels(crowther_rec_0_1f)[[1]]
  # rat$landcover <- c('non-forest','forest')
  # levels(crowther_rec_0_1f) <- rat
  # 
  # # 
  # ggplot() +
  #   layer_spatial(crowther_rec_0_1f) +
  #   scale_manual_colour()
  
  
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


# 2. Save maps ------------------------------------------------------------

save.image('maps_base.RData')        
    
 