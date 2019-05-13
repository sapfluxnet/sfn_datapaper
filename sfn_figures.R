
source('maps_base.R')

# 1. Choose map quality -----------------------------------------------------
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
