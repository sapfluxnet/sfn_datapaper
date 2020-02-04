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

# 3. Fix errors and taxonize ----------------------------------------------

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

sfn_sitespecies_tax[sfn_sitespecies_tax $sp_name == 'Myrtaceae sp.',
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
  tibble(pl_species=rep('Unknown',6),genus=rep('Unknown',6),
         family=rep('Myrtaceae',6),order=rep('Myrtales',6),group=rep('Angiosperms',6))


# 4. Number of trees (per dataset, species, etc.) -------------------------

# number of trees and species per dataset, with coordinates
sfn_sites_nspecies <- sfn_sitespecies_fix %>% 
  group_by(si_code) %>% 
  summarise(nspecies=length(sp_name),
            ntrees=sum(sp_ntrees)) %>% 
  left_join(sfn_allsites %>% dplyr::select(si_code,si_lat,si_long))

# Number of datasets per species
sfn_nspecies_dataset <- sfn_sitespecies_tax %>% 
  group_by(sp_name) %>% 
  tally() %>% 
  rename(n_sites=n) %>% 
  arrange(desc(n_sites)) 


# Number of species, taxonomic detail
sfn_nspecies_taxdetail<- sfn_sitespecies_tax %>% 
  distinct(sp_name,.keep_all=TRUE) %>% 
  dplyr::select(group, order, family,sp_name) 

sfn_nspecies_taxdetail %>% 
  group_by(group) %>% 
  tally()


# Number of trees per species
sfn_species_ntrees<- sfn_allplants_tax %>% 
  group_by(pl_species) %>% 
  mutate(n_trees=n()) %>% 
  distinct(pl_species,.keep_all = TRUE) %>% 
  mutate(species = case_when(
    pl_species=='Unknown' & genus == 'Unknown'~ paste0(family,' fam.'),
    TRUE ~ pl_species)) %>% 
  ungroup() %>% 
  dplyr::select(group,species,n_trees) %>% 
  arrange(desc(n_trees)) 


sfn_groups_ntrees<- sfn_allplants_tax %>% 
  group_by(group) %>% 
  tally()

# Number of trees per genus

sfn_genus_ntrees<- sfn_allplants_tax %>% 
  group_by(genus) %>% 
  mutate(n_trees=n()) %>% 
  distinct(genus,.keep_all = TRUE) %>% 
  mutate(genus_f = case_when(
    genus == 'Unknown'~ paste0(family,' fam.'),
    TRUE ~ genus)) %>% 
  ungroup() %>% 
  dplyr::select(genus_f,n_trees) %>% 
  arrange(desc(n_trees)) 

# 3. Methodologies -------------------------------------------------
# Levels: plant, sapwood, leaf

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


# Measurement method

sfn_method_ntrees <- sfn_allplants_tax %>% 
  group_by(pl_sens_meth) %>% 
  summarise(n_trees = n()) %>% 
  mutate(perc_trees = n_trees/sum(n_trees)*100) %>% 
  dplyr::select(pl_sens_meth,n_trees,perc_trees) %>% 
  arrange(desc(perc_trees))


# For alluvial plot
sfn_plants_type<- sfn_sites_type %>% 
  dplyr::select(si_code,typef) %>% 
  full_join( sfn_allplants_tax) %>% 
  dplyr::select(si_code,pl_sens_meth,typef,group) %>% 
  group_by(pl_sens_meth,typef,group) %>% tally()

sfn_plants_type %>% 
  group_by(typef) %>% 
  summarise(n_trees=sum(n))



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
