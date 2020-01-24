
# Stand attributes --------------------------------------------------------

sfn_allstands %>%
  dplyr::select(si_code, st_age, st_height, st_density,
                        st_basal_area, st_lai, st_soil_depth) %>%
  pivot_longer(names_to="Var",values_to= "Value", cols=st_age:st_soil_depth)-> sfn_allstands_long

sfn_allstands_long$Varf <- factor(sfn_allstands_long$Var,
                                 levels=c('st_age','st_height','st_density','st_basal_area','st_lai','st_soil_depth'),
                                 labels=c('Age~(years)','Height~(m)','Stem~density~(stems~ha^{"-1"})',
                                          'Basal~area~(m^2~ha^{"-1"})','LAI~(m^2~m^{"-2"})','Soil~depth~(m)'))

sfn_allstands_long %>% 
  ggplot(.,aes(x = Var, y = Value)) +
  geom_violin(fill = '#FDE3A7', alpha = 0.3) +
  geom_point(colour='black',alpha=0.5,
             position = position_jitter(width = 0.2, height = 0))+
  facet_wrap(~Varf,scales='free',labeller=label_parsed)+
  guides(fill = FALSE) +
  labs(x=NULL,y=NULL)+
  theme_light()+
  theme(axis.text.x=element_blank(),axis.text.y=element_text(size=12),
        strip.background = element_rect(fill='white',colour='darkgray'),
        strip.text=element_text(size=12,colour='black',face='bold')) -> stand_violins



#  Plant attributes -------------------------------------------------------

sfn_allplants %>% 
  dplyr::select(pl_code,pl_dbh,pl_height,pl_sapw_area,pl_sapw_depth,pl_leaf_area) %>% 
  pivot_longer(names_to="Var", values_to="Value",cols=pl_dbh:pl_leaf_area)-> sfn_allplants_long

sfn_allplants_long$Varf <- factor(sfn_allplants_long$Var,
                                  levels=c('pl_dbh','pl_height','pl_sapw_area','pl_sapw_depth','pl_leaf_area'),
                                  labels=c('DBH~(cm)','Height~(m)','Sapwood~area~(cm^2)','Sapwood~depth~(cm)','Leaf~area(m^2)'))


sfn_allplants_long%>% 
  ggplot(.,aes(x = Var, y = Value)) +
  geom_violin(fill = '#FDE3A7', alpha = 0.3) +
  geom_point(colour='black',alpha=0.5,
             position = position_jitter(width = 0.2, height = 0))+
  facet_wrap(~Varf,scales='free',labeller=label_parsed)+
  guides(fill = FALSE) +
  labs(x=NULL,y=NULL)+
  theme_light()+
  theme(axis.text.x=element_blank(),axis.text.y=element_text(size=12),
        strip.background = element_rect(fill='white',colour='darkgray'),
        strip.text=element_text(size=12,colour='black',face='bold')) -> plant_violins
