library(raster)
library(ggspatial)
library(ggrepel)
library(cowplot)

# If changes applied re-run source, ~ 10 min
# source('maps_base.R')

load('maps_base.RData')

# Draft versions ----------------------------------------------------------

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


# 3. Map, numeric codes, ggrepel not aligned ------------------------------------------------------

# world
sfnsites_world_dr <- globforest_rec_0_5  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='royalblue',alpha=0.5)+
  geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_label), 
                  aes(si_long, si_lat, label = num_code), size = 3,
                  segment.alpha=0.5,  nudge_x = -0.35,direction = "both",
                  box.padding = unit(0.1, 'lines'), force = 0.5)+geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)

# europe
sfnsites_europe_dr<- globforest_rec_0_5  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='royalblue',alpha=0.5)+
  geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_europe), 
                  aes(si_long, si_lat, label = num_code), size = 3,
                  segment.alpha=0.5,  nudge_y = -0.35,direction = "both",
                  box.padding = unit(0.1, 'lines'), force = 0.5)+
  coord_sf(xlim = c(-20, 40), ylim = c(32, 67), expand = FALSE)+
  geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)

# america
sfnsites_america_dr<- globforest_rec_0_5  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='royalblue',alpha=0.5)+
  geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_america), 
                  aes(si_long, si_lat, label = num_code), size = 3,
                  segment.alpha=0.5,  nudge_x = -0.35,direction = "both",
                  box.padding = unit(0.1, 'lines'), force = 0.5)+
  coord_sf(xlim = c(-130, -60), ylim = c(30, 55), expand = FALSE)+
  geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)

# austral
sfnsites_austral_dr<- globforest_rec_0_5  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='royalblue',alpha=0.5)+
  geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_austral), 
                  aes(si_long, si_lat, label = num_code), size = 3,
                  segment.alpha=0.5,  nudge_x = -0.35,
                  box.padding = unit(0.1, 'lines'), force = 0.5)+
  coord_sf(xlim = c(110, 162), ylim = c(-10, -50), expand = FALSE)+
  geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)

# africa
sfnsites_africa_dr<- globforest_rec_0_5  +
  geom_point(data=sfn_allsites,aes(x=si_long,y=si_lat),shape=21,color='black',fill='royalblue',alpha=0.5)+
  geom_text_repel(data = subset(sfn_allsites_country,country%in%countries_africa), aes(si_long, si_lat, label = num_code), size = 3,
                  box.padding = unit(0.1, 'lines'), force = 0.5)+
  coord_sf(xlim = c(10, 50), ylim = c(-10, -40), expand = FALSE)+
  geom_label_repel()+guides(fill='none')+xlab(NULL)+ylab(NULL)



# e) Build figure 

plot_grid(
  plot_grid(sfnsites_world_dr,ncol=1,nrow=1,labels=c('a)')),
  plot_grid(sfnsites_europe_dr,sfnsites_austral_dr,ncol=2,nrow=1,rel_widths=c(1,0.7),labels=c('b)','c)')),
  plot_grid(sfnsites_america_dr,sfnsites_africa_dr,ncol=2,nrow=1,rel_widths=c(1,0.7),labels=c('d)','e)')),
  nrow=3,rel_heights=c(1,1,0.7))




