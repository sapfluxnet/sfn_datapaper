library(raster)
library(ggspatial)
library(ggrepel)
library(tidyverse)
library(cowplot)

# 0. Run requirements  ----------------------------------

# If changes applied re-run source, ~ 10 min
# source('maps_base.R')

load('maps_base.RData')

# Quick to run
source('metadata_wrangling.R')

# 0. Choose map quality -----------------------------------------------------
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



# 4. Conditioned maps -----------------------------------------------------

# TODO: set the symbol colour black
# world

# igbp
sfn_igbp <- globforest  +
  geom_point(data=sfn_allsites,
             aes(x=si_long,y=si_lat,col=si_igbp))+
  
  guides(fill='none')+xlab(NULL)+ylab(NULL)
# type
sfn_sftype<- globforest  +
  geom_point(data=sfn_sites_type,
             aes(x=si_long,y=si_lat,col=type))+
  
  guides(fill='none')+xlab(NULL)+ylab(NULL)

# n species

sfn_nspecies <- globforest  +
  geom_point(data=sfn_sites_nspecies,
             aes(x=si_long,y=si_lat,col=nspecies))+
  
  guides(fill='none')+xlab(NULL)+ylab(NULL)

# ntrees
sfn_ntrees <- globforest  +
  geom_point(data=sfn_sites_nspecies,
             aes(x=si_long,y=si_lat,col=ntrees))+
  
  guides(fill='none')+xlab(NULL)+ylab(NULL)



sfn_sftypes <- plot_grid(sfn_sftype,sfn_igbp,sfn_nspecies,sfn_ntrees, 
                                   labels=c('a', 'b','c','d'), ncol=2, nrow=2)


<<<<<<< HEAD
# TODO: alternative, show histograms, 
=======
# TODO: alternative, show histograms
>>>>>>> b6c66e55cbb468cc9d27cfa20d2b3fd67264a7dd

# Combination of alluvial and histogram for methods x sftype x group (gymno/angio)?
# https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html
