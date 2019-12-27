library(raster)
library(ggspatial)
library(ggrepel)
library(tidyverse)
library(cowplot)

# 4. Conditioned maps -----------------------------------------------------

# TODO: set the symbol colour black
# world

# igbp
sfn_igbp <- globforest_rec_0_5  +
  geom_point(data=sfn_allsites,
             aes(x=si_long,y=si_lat,col=si_igbp))+
  
  guides(fill='none')+xlab(NULL)+ylab(NULL)
# type
sfn_sftype<- globforest_rec_0_5  +
  geom_point(data=sfn_sites_type,
             aes(x=si_long,y=si_lat,col=type))+
  
  guides(fill='none')+xlab(NULL)+ylab(NULL)

# n species

sfn_nspecies <- globforest_rec_0_5  +
  geom_point(data=sfn_sites_nspecies,
             aes(x=si_long,y=si_lat,col=nspecies))+
  
  guides(fill='none')+xlab(NULL)+ylab(NULL)

# ntrees
sfn_ntrees <- globforest  +
  geom_point(data=sfn_sites_nspecies,
             aes(x=si_long,y=si_lat,col=ntrees))+
  
  guides(fill='none')+xlab(NULL)+ylab(NULL)



sfn_sf_igbbp_sftype <- plot_grid(sfn_sftype,sfn_igbp,
                         labels=c('a)', 'b)'), ncol=1, nrow=2)

sfn_sftypes <- plot_grid(sfn_sftype,sfn_igbp,sfn_nspecies,sfn_ntrees, 
                                   labels=c('a', 'b','c','d'), ncol=2, nrow=2)


# histograms

# Combination of alluvial and histogram for methods x sftype x group (gymno/angio)?
# https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html


method_type <- ggplot(sfn_plants_type,
       aes(y = n, axis1 = pl_sens_meth, axis2 = typef)) +
  geom_alluvium(aes(fill = typef), width = 1/12)+
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", infer.label = TRUE) 




