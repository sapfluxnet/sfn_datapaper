library(raster)
library(ggspatial)
library(ggrepel)
library(tidyverse)
library(cowplot)

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
