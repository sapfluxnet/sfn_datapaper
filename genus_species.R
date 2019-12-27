library(ggbiome)

# 1. Biome plot -----------------------------------------------------------


# TODO: preliminary filtering out climatic NAs
# Use CHELSA to gf 

biomes_plot <- sfn_allsites %>%
  arrange(si_biome) %>%
  filter(!is.na(si_map) | !is.na(si_map)) %>% 
  ggbiome::vis_location_biome(si_lat='si_lat',
                              si_long='si_long',si_mat='si_mat',si_map='si_map') +
  
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    title= element_text(size=30),
    axis.title=element_text(size=30),
    axis.text=element_text(size=30),
    legend.title = element_blank(),
    legend.text = element_text(size =24),
    legend.key.size = unit(1.5, 'line'),
    legend.position = c(0.7,0.2)
    
  )

biomes_plot



# 2. Genera ----------------------------------------------------------------


sp_genus_all <- sfn_sitespecies  %>%
  mutate(sp_genus = str_trim(str_extract(sp_name, '([^\\s]+)'))) %>%
  group_by(sp_genus) %>%
  summarise(n = sum(sp_ntrees))

sp_genus_all %<>%
  mutate(n = as.integer(n))


# 2.1. All genera

sp_genus_plot <-sfn_sitespecies  %>%
  mutate(sp_genus = str_trim(str_extract(sp_name, '([^\\s]+)'))) %>%
  group_by(sp_genus, si_code) %>%
  summarise(n = sum(sp_ntrees)) %>%
  ggplot(aes(x = reorder(sp_genus, n, sum), y = n, fill = si_code)) +
  ggiraph::geom_bar_interactive(aes(tooltip = si_code, data_id = si_code),
                                stat = 'identity') +
  viridis::scale_fill_viridis(discrete = TRUE) +
  labs(x = '', y = 'Number of plants', title = 'Genus') +
  coord_flip() +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = 'none'
  )

# 2.1. All genera ---------------------------------------------------------




# 2.1. Genera with >50 trees ---------------------------------------------------

sp_main_genus_data <- sfn_sitespecies %>%
  filter(!is.na(sp_ntrees)) %>% 
  mutate(sp_genus = str_trim(str_extract(sp_name, '([^\\s]+)'))) %>%
  group_by(sp_genus,si_code) %>%
  mutate(n_si = sum(sp_ntrees)) %>% 
  group_by(sp_genus) %>% 
  mutate(n_all = sum(sp_ntrees)) %>% 
  filter(n_all>50) %>% 
  ggplot(aes(x = reorder(sp_genus, n_all, sum), y = n_si, fill = si_code)) +
  ggiraph::geom_bar_interactive(aes(tooltip = si_code, data_id = si_code),
                       stat = 'identity') +
  viridis::scale_fill_viridis(discrete = TRUE) +
  labs(x = '', y = 'Number of trees',
       title = 'Genera with > 50 trees') +
  coord_flip() +
  theme_bw() +
  theme(
    title= element_text(size=12),
    axis.title=element_text(size=12),
    axis.text=element_text(size=12),
    axis.text.y=element_text(face='italic'),
    legend.title = element_blank(),
    legend.position = 'none'
  )



# 3. Species --------------------------------------------------------------

# barplot of all species

sp_sum_data <- sfn_sitespecies%>%
  filter(!is.na(sp_ntrees)) %>% 
  group_by(sp_name) %>%
  summarise(n = sum(sp_ntrees)) %>% 
  arrange(desc(n))


# Table 

sfn_sitespecies %>% 
  mutate(sp_basal_area_perc = as.integer(sp_basal_area_perc),
         sp_ntrees = as.integer(sp_ntrees)) %>% 
  select(si_code,sp_name,sp_leaf_habit,sp_ntrees,sp_basal_area_perc) %>% 
  flextable::flextable()


flextable::flextable(sfn_sitespecies)


# 30

numbersp_barplot_30 <- sfn_sitespecies%>%
  filter(!is.na(sp_ntrees)) %>% 
  group_by(sp_name, si_code) %>%
  summarise(n = sum(sp_ntrees)) %>%
  group_by(sp_name) %>% 
  mutate(n_all=sum(n)) %>% 
  filter(n_all>30) %>% 
  ggplot(aes(x = reorder(sp_name, n, sum), y = n, fill = si_code)) +
  ggiraph::geom_bar_interactive(aes(tooltip = si_code, data_id = si_code),
                       stat = 'identity') +
  viridis::scale_fill_viridis(discrete = TRUE) +
  labs(x = '', y = 'Number of trees', 
       title = 'Species with >30 trees') +
  coord_flip() +
  theme_bw() +
  theme(
    title= element_text(size=16),
    axis.title=element_text(size=16),
    axis.text=element_text(size=16),
    axis.text.y=element_text(face='italic'),
    legend.title = element_blank(),
    legend.position = 'none'
  )


# Joint plot
# save parameters: h=1000, w=1300

plot_grid(sp_main_genus_data,numbersp_barplot_30, 
          labels=c('a)', 'b)'), label_size= 16,
          ncol=1, nrow=2, align = "v", axis = "b")



