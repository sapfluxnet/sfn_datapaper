library(sapfluxnetr)
library(purrr)
library(tidyverse)
library(FREddyPro)

# Data paths

path.plant <- file.path('data/0.1.3/RData/plant')
path.sapwood <- file.path('data/0.1.3/RData/sapwood')

# Show sites
sfn_sites_in_folder(path.sapwood)

# Plot
cowplot::plot_grid(
sfn_finger_species(read_sfn_data('ESP_CAN',folder=path.sapwood)),
sfn_finger_species(read_sfn_data('USA_WVF',folder=path.sapwood),years=1998,
                   species=c('Acer saccharum','Prunus serotina','Quercus alba','Quercus rubra')),

sfn_finger_species(read_sfn_data('CRI_TAM_TOW',folder=path.sapwood),years=2015,
                   species=c('Pouteria sp.','Inga sp.',
                                            'Eschweillera sp.','Mortoniodendron anisophyllum')),
ncol=1,labels=c('a)','b)','c)'),rel_heights = c(1,1,1))

# Plot
sfn_finger_species(years=2015,read_sfn_data('CRI_TAM_TOW',folder=path.sapwood))       
                   




