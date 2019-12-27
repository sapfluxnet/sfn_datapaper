library(raster)
library(ggspatial)
library(ggrepel)
library(cowplot)

# If changes applied re-run source, ~ 10 min
# source('maps_base.R')

load('maps_base.RData')

# Draft versions ----------------------------------------------------------


# 3. Map, numeric codes, ggrepel not aligned ------------------------------------------------------

# d) Create maps 

# world
sfnsites_world_dr <- globforest_rec_0_5  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_label), 
                  aes(si_long, si_lat, label = num_code), size = 3,
                  segment.alpha=0.5,  nudge_x = -0.35,direction = "y",hjust = 1,
                  box.padding = unit(0.1, 'lines'), force = 0.5)+geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)

# europe
sfnsites_europe_dr<- globforest_rec_0_5  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_europe), 
                  aes(si_long, si_lat, label = num_code), size = 3,
                  segment.alpha=0.5,  nudge_y = -0.35,direction = "y",hjust = 1,
                  box.padding = unit(0.1, 'lines'), force = 0.5)+
  coord_sf(xlim = c(-20, 40), ylim = c(32, 67), expand = FALSE)+
  geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)

# america
sfnsites_america_dr<- globforest_rec_0_5  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_america), 
                  aes(si_long, si_lat, label = num_code), size = 3,
                  segment.alpha=0.5,  nudge_x = -0.35,direction = "y",hjust = 1,
                  box.padding = unit(0.1, 'lines'), force = 0.5)+
  coord_sf(xlim = c(-130, -60), ylim = c(30, 55), expand = FALSE)+
  geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)

# austral
sfnsites_austral_dr<- globforest_rec_0_5  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_austral), 
                  aes(si_long, si_lat, label = num_code), size = 3,
                  segment.alpha=0.5,  nudge_x = -0.35,
                  box.padding = unit(0.1, 'lines'), force = 0.5)+
  coord_sf(xlim = c(110, 162), ylim = c(-10, -50), expand = FALSE)+
  geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)

# africa
sfnsites_africa_dr<- globforest_rec_0_5  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_africa), aes(si_long, si_lat, label = num_code), size = 3,
                  box.padding = unit(0.1, 'lines'), force = 0.5)+
  coord_sf(xlim = c(10, 50), ylim = c(-10, -40), expand = FALSE)+
  geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)



# e) Build figure 

sfn_sitesmap_numcodes_dr <- plot_grid(
  # row 1
  plot_grid(sfnsites_world_dr, labels='(a)'),
  # row 2
  plot_grid(sfnsites_europe_dr, sfnsites_america_dr,labels=c('(b)','(c)'),rel_widths=c(1,1)),
  # row3
  plot_grid(sfnsites_austral_dr, sfnsites_africa_dr, labels=c('(d)','(e)')), 
  #
  labels=c('', '',''), ncol=1)

# save_plot('./output/figs/sfn_sitesmap.tiff', sfn_sitesmap)
# save_plot('./output/figs/sfn_sitesmap.eps', sfn_sitesmap)



# 2. Map, no codes ---------------------------------------------

# d) Create maps 

# world
sfnsites_world_nc_dr <- globforest_rec_0_5  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  guides(fill='none')+xlab(NULL)+ylab(NULL)

# europe
sfnsites_europe_nc_dr<- globforest_rec_0_5  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  coord_sf(xlim = c(-20, 40), ylim = c(32, 67), expand = FALSE)+
  guides(fill='none')+xlab(NULL)+ylab(NULL)

# america
sfnsites_america_nc_dr<- globforest_rec_0_5  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  coord_sf(xlim = c(-130, -60), ylim = c(30, 55), expand = FALSE)+
  guides(fill='none')+xlab(NULL)+ylab(NULL)

# austral
sfnsites_austral_nc_dr<- globforest_rec_0_5  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  coord_sf(xlim = c(110, 162), ylim = c(-10, -50), expand = FALSE)+
  guides(fill='none')+xlab(NULL)+ylab(NULL)

# africa
sfnsites_africa_nc_dr<- globforest_rec_0_5  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  coord_sf(xlim = c(10, 50), ylim = c(-10, -40), expand = FALSE)+
  guides(fill='none')+xlab(NULL)+ylab(NULL)



# e) Build figure 

sfn_sitesmap_nocodes_dr <- plot_grid(
  # row 1
  plot_grid(sfnsites_world_nc_dr, labels='(a)'),
  # row 2
  plot_grid(sfnsites_europe_nc_dr, sfnsites_america_nc_dr,labels=c('(b)','(c)'),rel_widths=c(1,1)),
  # row3
  plot_grid(sfnsites_austral_nc_dr, sfnsites_africa_nc_dr, labels=c('(d)','(e)')), 
  #
  labels=c('', '',''), ncol=1)



