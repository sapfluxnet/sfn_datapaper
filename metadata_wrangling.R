library(sapfluxnetr)
library(tidyverse)
library(magrittr)
library(taxonlookup)


# 1. Read metadata ---------------------------------------------------------------
# From previously written cache file

sfn_metadata_plant <- read_sfn_metadata(folder = 'data/0.1.3/RData/plant', .write_cache = FALSE)
sfn_metadata_sapwood <- read_sfn_metadata(folder = 'data/0.1.3/RData/sapwood', .write_cache = FALSE)
sfn_metadata_leaf <- read_sfn_metadata(folder = 'data/0.1.3/RData/leaf', .write_cache = FALSE)

# 2. Aggregate all datasets -----------------------------------------------

# Join all metadata regardless of having sap flow per sapwood or per plant

sfn_allsites<- sfn_metadata_plant[['site_md']] %>% 
  full_join(dplyr::select(sfn_metadata_sapwood[['site_md']],-si_remarks))

sfn_allstands<- sfn_metadata_plant[['stand_md']] %>% 
  full_join(sfn_metadata_sapwood[['stand_md']])

sfn_sitespecies<- sfn_metadata_plant[['species_md']] %>% 
  full_join(sfn_metadata_sapwood[['species_md']]) 

sfn_allplants<- sfn_metadata_plant[['plant_md']] %>% 
  full_join(sfn_metadata_sapwood[['plant_md']]) %>% 
  distinct(pl_code,.keep_all = TRUE)

sfn_env <- sfn_metadata_plant[['env_md']] %>% 
  full_join(sfn_metadata_sapwood[['env_md']])

# Fix errors and taxonize
# Species level
sfn_sitespecies %>% 
  mutate(sp_name = case_when(
    sp_name == 'Eschweillera sp.' ~'Eschweilera sp.',
    sp_name == 'Vacapoua americana' ~ 'Vouacapoua americana',
    sp_name == 'Brachulaena ramiflora' ~ 'Brachylaena ramiflora',
    sp_name == 'Cryptocaria spp.' ~ 'Cryptocarya sp.',
    TRUE ~ sp_name)) ->  sfn_sitespecies_fix


sfn_sitespecies_fix %>% 
pull(sp_name) %>%
  unique() %>%
  taxonlookup::lookup_table(missing_action = 'NA', by_species = TRUE) %>%
  rownames_to_column('sp_name') %>%
  left_join(sfn_sitespecies_fix, ., by = 'sp_name') -> sfn_sitespecies_tax 

sfn_sitespecies_tax[sfn_sitespecies_fix2$sp_name == 'Myrtaceae sp.',
                     c('sp_name','genus','order','family','group')] <-
  c('Unkwown','Unknown','Myrtaceae','Myrtales','Angiosperms')

# Plant level
sfn_allplants %>% 
  mutate(pl_species = case_when(
    pl_species == 'Eschweillera sp.' ~'Eschweilera sp.',
    pl_species == 'Vacapoua americana' ~ 'Vouacapoua americana',
    pl_species == 'Brachulaena ramiflora' ~ 'Brachylaena ramiflora',
    pl_species == 'Cryptocaria spp.' ~ 'Cryptocarya sp.',
    TRUE ~ pl_species)) ->  sfn_allplants_fix


sfn_allplants_fix %>% 
pull(pl_species) %>%
  unique() %>%
  taxonlookup::lookup_table(missing_action = 'NA', by_species = TRUE) %>%
  rownames_to_column('pl_species') %>%
  left_join(sfn_allplants_fix, ., by = 'pl_species') -> sfn_allplants_tax

sfn_allplants_tax[sfn_allplants_tax$pl_species == 'Myrtaceae sp.',
                    c('pl_species','genus','order','family','group')] <-
  c('Unkwown','Unknown','Myrtaceae','Myrtales','Angiosperms')


# Calculate number of trees and species

sfn_sites_nspecies <- sfn_sitespecies_fix %>% 
  group_by(si_code) %>% 
  summarise(nspecies=length(sp_name),
            ntrees=sum(sp_ntrees)) %>% 
  left_join(sfn_allsites %>% dplyr::select(si_code,si_lat,si_long))

# Number of species

sfn_sitespecies_tax %>% 
  group_by(family) %>% 
  tally()

sfn_sitespecies_tax %>% 
  group_by(genus) %>% 
  tally()


sfn_sitespecies_tax %>% 
  group_by(sp_name) %>% 
  tally()

sfn_species<- sfn_sitespecies_tax %>% 
  distinct(sp_name)

sfn_sitespecies_tax %>% 
  group_by(family,genus,sp_name) %>% 
  tally() %>%  View()

# 3. Measurement type -------------------------------------------------
# plant, sapwood, leaf

sfn_sites_plsw <- sfn_metadata_plant[['site_md']] %>% 
  semi_join( dplyr::select(
    sfn_metadata_sapwood[['site_md']],
    -si_remarks)) %>% 
  mutate(type='plant,sapwood')


sfn_sites_pl <- sfn_metadata_plant[['site_md']] %>% 
  anti_join(dplyr::select(
    sfn_metadata_sapwood[['site_md']],
    -si_remarks)) %>% 
  mutate(type='plant')


sfn_sites_sw <- dplyr::select(sfn_metadata_sapwood[['site_md']],-si_remarks) %>% 
  anti_join(sfn_metadata_plant[['site_md']]) %>% 
  mutate(type='sapwood')

sfn_sites_leaf <- sfn_metadata_leaf[['site_md']] %>% 
  mutate(type='leaf,plant,sapwood')

sfn_sites_alllevels <- sfn_sites_plsw %>% 
  full_join(sfn_sites_pl) %>% 
  full_join(sfn_sites_sw)  


# Measurement type: plant, sapwood, leaf
sfn_sites_type <- sfn_sites_alllevels %>% 
  mutate(
    type=ifelse(si_code%in%sfn_sites_leaf$si_code,
                'leaf,plant,sapwood',type),
    typef=factor(type)
  ) 

# For alluvial plot
sfn_plants_type<- sfn_sites_type %>% 
  dplyr::select(si_code,typef) %>% 
  full_join( sfn_allplants_tax) %>% 
  select(si_code,pl_sens_meth,typef,group) %>% 
  group_by(pl_sens_meth,typef,group) %>% tally()

# 4. Percentage basal area ------------------------------------------------

dataset_trees_sp <- sfn_sites_type %>%
  right_join(sfn_allstands) %>% 
  right_join(
    sfn_sitespecies %>%
      group_by(si_code) %>%
      mutate(total_ntrees = sum(sp_ntrees,na.rm=TRUE),
             nspecies = n_distinct(sp_name),
             percab_measured = sum(sp_basal_area_perc,na.rm=TRUE)),
    by='si_code'
  ) %>% 
    mutate(
      typeplant=ifelse(str_detect(type,'plant'),'plant',NA))


# 5. Gap-fill climate -----------------------------------------------------


# 6. Scaling data ---------------------------------------------------------



# Crap --------------------------------------------------------------------

save.image('sfn_datapaper_data.RData')