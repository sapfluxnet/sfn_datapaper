library(raster)
library(ggspatial)
library(ggrepel)
library(cowplot)

# If changes applied re-run source, ~ 10 min
# source('maps_base.R')

load('maps_base.RData')

# High quality versions ---------------------------------------------------

# 1. Map, numeric codes, ggrepel not aligned ------------------------------------------------------

# a) Site labels 

# b) Select countries to plot 

countries_sfn <- unique(sapply(strsplit(sfn_allsites$si_code,"_"),"[[",1))
countries_label <- c('ARG','BRA','COL','CRI','CHN','GUF','IDN','ISR','KOR','MDG','MEX','NZL','RUS','SEN','THA','UZB')
countries_europe <- c('AUT','CHE','CZE','DEU','ESP','FIN','FRA','GBR','HUN','ITA','NLD','PRT','SWE')
countries_america <- c('CAN','USA')
countries_austral <- c('AUS')
countries_africa <- c('MDG','ZAF')


# c) Add numeric codes 

sfn_allsites_country<- sfn_allsites %>% 
  mutate(country=sapply(strsplit(si_code,"_"),"[[",1),
         num_code = as.integer(factor(si_code)))


# d) Create maps 

# world
sfnsites_world <- globforest_rec_0_1  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_label), 
                  aes(si_long, si_lat, label = num_code), size = 3,
                  segment.alpha=0.5,  nudge_x = -0.35,direction = "y",hjust = 1,
                  box.padding = unit(0.1, 'lines'), force = 0.5)+geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)

# europe
sfnsites_europe<- globforestrec_rec_0_1  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_europe), 
                  aes(si_long, si_lat, label = num_code), size = 3,
                  segment.alpha=0.5,  nudge_y = -0.35,direction = "y",hjust = 1,
                  box.padding = unit(0.1, 'lines'), force = 0.5)+
  coord_sf(xlim = c(-20, 40), ylim = c(32, 67), expand = FALSE)+
  geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)

# america
sfnsites_america<- globforest_rec_0_1  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_america), 
                  aes(si_long, si_lat, label = num_code), size = 3,
                  segment.alpha=0.5,  nudge_x = -0.35,direction = "y",hjust = 1,
                  box.padding = unit(0.1, 'lines'), force = 0.5)+
  coord_sf(xlim = c(-130, -60), ylim = c(30, 55), expand = FALSE)+
  geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)

# austral
sfnsites_austral<- globforest_rec_0_1  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_austral), 
                  aes(si_long, si_lat, label = num_code), size = 3,
                  segment.alpha=0.5,  nudge_x = -0.35,
                  box.padding = unit(0.1, 'lines'), force = 0.5)+
  coord_sf(xlim = c(110, 162), ylim = c(-10, -50), expand = FALSE)+
  geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)

# africa
sfnsites_africa<- globforest_rec_0_1  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_africa), aes(si_long, si_lat, label = num_code), size = 3,
                  box.padding = unit(0.1, 'lines'), force = 0.5)+
  coord_sf(xlim = c(10, 50), ylim = c(-10, -40), expand = FALSE)+
  geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)



# e) Build figure 

sfn_sitesmap_numcodes <- plot_grid(
  # row 1
  plot_grid(sfnsites_world, labels='(a)'),
  # row 2
  plot_grid(sfnsites_europe, sfnsites_america,labels=c('(b)','(c)'),rel_widths=c(1,1)),
  # row3
  plot_grid(sfnsites_austral, sfnsites_africa, labels=c('(d)','(e)')), 
  #
  labels=c('', '',''), ncol=1)

# save_plot('./output/figs/sfn_sitesmap.tiff', sfn_sitesmap)
# save_plot('./output/figs/sfn_sitesmap.eps', sfn_sitesmap)



# 2. Map, no codes ---------------------------------------------

# d) Create maps 

# world
sfnsites_world_nc <- globforest_rec_0_1  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  guides(fill='none')+xlab(NULL)+ylab(NULL)

# europe
sfnsites_europe_nc<- globforest_rec_0_1  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  coord_sf(xlim = c(-20, 40), ylim = c(32, 67), expand = FALSE)+
 guides(fill='none')+xlab(NULL)+ylab(NULL)

# america
sfnsites_america_nc<- globforest_rec_0_1  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  coord_sf(xlim = c(-130, -60), ylim = c(30, 55), expand = FALSE)+
guides(fill='none')+xlab(NULL)+ylab(NULL)

# austral
sfnsites_austral_nc<- globforest_rec_0_1  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  coord_sf(xlim = c(110, 162), ylim = c(-10, -50), expand = FALSE)+
  guides(fill='none')+xlab(NULL)+ylab(NULL)

# africa
sfnsites_africa_nc<- globforest_rec_0_1  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='yellow')+
  coord_sf(xlim = c(10, 50), ylim = c(-10, -40), expand = FALSE)+
  guides(fill='none')+xlab(NULL)+ylab(NULL)



# e) Build figure 

sfn_sitesmap_nocodes <- plot_grid(
  # row 1
  plot_grid(sfnsites_world_nc, labels='(a)'),
  # row 2
  plot_grid(sfnsites_europe_nc, sfnsites_america_nc,labels=c('(b)','(c)'),rel_widths=c(1,1)),
  # row3
  plot_grid(sfnsites_austral_nc, sfnsites_africa_nc, labels=c('(d)','(e)')), 
  #
  labels=c('', '',''), ncol=1)


