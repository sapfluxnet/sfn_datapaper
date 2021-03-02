library(emmeans)
library(lme4)
library(sapfluxnetr)
library(cowplot)
library(patchwork)
library(viridis)
library(sapflux)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(flextable)


# 0. Code to illustrate uncertainty estimation for SAPFLUXNET data --------
# Rafael Poyatos, Víctor Flo, CREAF, March 2021

# 1. Calibrations data ----------------------------------------------------

# Read data from Zenodo
# https://zenodo.org/record/4559497#.YD4NfnVKiis

subdata <- read.csv("https://zenodo.org/record/4559497/files/Sap_flow_methods_calibrations_database.csv?download=1")

# Calculate range of each calibration
range_df <- subdata %>% group_by(calibration_name) %>%
  dplyr::summarize(min_real=min(actual_SFD),
                   max_real = max(actual_SFD),
                   range_mean = (min_real + max_real)/2)

# Calculate range for each method
range_df_method <- subdata %>% 
  group_by(method)%>%
  summarise(min_method = min(actual_SFD),
            max_method = max(actual_SFD),
  )


# 2. Standard error model ----------------------------------------------------

coefficients <- subdata %>% group_by(method) %>% 
  do(broom.mixed::tidy(lmer( actual_SFD ~ measured_SFD+ (1|calibration_name), data = .))) %>%
  dplyr::filter(!is.na(std.error)) %>% 
  dplyr::select(method, term, std.error) %>%
  tidyr::spread(term, std.error) %>%
  dplyr::rename(beta1 = measured_SFD,
                beta0 = `(Intercept)`)%>%
  left_join(range_df_method) %>% 
  mutate(
    method=as.factor(method),
    method=fct_recode(method,HPTM="T-max",HD="TD",CHD="TTD"))  


# Display model coefficients ----------------------------------------------
# Table B1 in the appendix

coefficients %>% 
  mutate(method=fct_relevel(method,'HD','CHP','HR','HPTM','CHD','HFD')) %>%  
  arrange(method) %>% 
  select(method,beta0,beta1) %>% 
  qflextable() %>%  
  bold( i = 1,  part = "header") %>%
  fontsize(size=10,part='body') %>% 
  colformat_num(digits=2,j=c(2,3)) %>% 
  set_header_labels(
    method='Method',
    beta0='Intercept',
    beta1='Slope') %>% 
  align(part='header',align='left') %>% 
  save_as_docx(path='docs/table_coefs.docx')


# 3. Sapfluxnet daily data ------------------------------------------------


# Path to the folder where the sapfluxnet database is stored
# Download first from Zenodo: https://zenodo.org/record/3971689#.YD4TG3VKiis

sfn_folder <- 'data/0.1.5'
sfn_folder_sw <- file.path(sfn_folder,'RData','sapwood')
sfn_datasets_sw<- sfn_sites_in_folder(sfn_folder_sw)

# This assumes metadata have already been cached, if not, set write_cache = TRUE
sfn_metadata_sw <- read_sfn_metadata(folder=sfn_folder_sw,.write_cache=FALSE)


# Code commented below only needs to be run once to obtain daily summaries
# When run, save results and load when needed

# Read ALL sapwood-area related data from SFN folder
# Creates a large sfn_multi object
# 2 min in a 32 Gb RAM, Intel® Core™ i7-10510U CPU @ 1.80GHz × 8 laptop
# Creates a ~ 4 Gb object

# sfn_data_sw <- read_sfn_data(sfn_datasets_sw,folder=sfn_folder_sw)
# 
# future::plan('sequential')
# 
# # up the limit to 4GB, 1Gb in bytes is 1014*1024^2 
# options('future.globals.maxSize' = 4*(1014*1024^2))
# 
# # Compute daily metrics, including 95% and 99% quantiles
# # ~ takes 45 min 
# # returns a list
# sfn_sw_max <- daily_metrics(sfn_data_sw,
#                             solar=TRUE,
#                             probs=c(0.95,0.99),
#                             tidy=FALSE)
# Save
# save(sfn_sw_max,file='data/sfn_max_list.RData')


# Load previously saved results
load('data/sfn_max_list.RData')

# Convert to data frame
sfn_sw_max_df <- sfn_sw_max %>% 
  metrics_tidyfier(metadata=sfn_metadata_sw,interval='general') 

# Select methods
methods_included <- c('CHD','CHP','HD','HFD','HPTM','HR')

# Process daily data: selection, filtering, join SEcoefs, SE estimation
sfn_daily_all_uncert <- sfn_sw_max_df  %>%
  # select variables
  dplyr::select(TIMESTAMP,si_code,pl_code,sapflow_mean,
         sapflow_q_95,sapflow_q_99,
         sapflow_coverage,vpd_mean)  %>% 
  # join with plant metadata
  left_join(dplyr::select(sfn_metadata_sw$plant_md,pl_code,pl_species,
                          pl_sens_meth,pl_sens_calib),
            by='pl_code') %>%
  dplyr::filter(sapflow_coverage>90 & pl_sens_meth%in%methods_included) %>% 
  # join with method-specific coefs to estimate SE
  left_join(coefficients,by=c('pl_sens_meth'='method')) %>% 
  mutate(
    sapflow_SE = beta0 + beta1* sapflow_mean 
  )


# 4. Compare SE estimation with flow range in calibrations ---------------------------------------

# Get Q95 values across methods
sfn_max <- sfn_daily_all_uncert %>% 
  filter(si_code!='MEX_VER_BSM' & si_code!='RUS_CHE_Y4') %>% 
  group_by(pl_sens_meth) %>% 
  summarise(
    across(contains('q_'),~max(.x,na.rm=TRUE)),
  ) %>% 
  left_join(coefficients,by=c('pl_sens_meth'='method')) %>% 
  mutate(
    sfdq95_SE = beta0 + beta1* sapflow_q_95,
    sfdq99_SE = beta0 + beta1* sapflow_q_99,
    pl_sens_meth=fct_relevel(pl_sens_meth,'HD','CHP','HR','HPTM','CHD','HFD')
  ) 
  


calibSE_plot <-  sfn_max%>% 
    ggplot() + xlim(0,385)+
  geom_segment(aes(colour=pl_sens_meth,
                   x=min_method,xend=max_method,
                   y=beta0+beta1*min_method,yend=beta0+beta1*max_method),size=1.5)+
  scale_colour_viridis_d(option = "D") +
  theme_cowplot()+
  theme(legend.position=c(0.8,0.75),legend.title = element_blank(),
        legend.box.background = element_rect(colour = 1),
        legend.box.margin = margin(t=.25,b=.25,r=.25,l=.25,'cm'))+
  xlab(expression(paste('Sap flow per sapwood area, ',cm^3,cm^-2,h^-1)))+
  ylab(expression(paste('Standard error, ',cm^3,cm^-2,h^-1)))
  

#Calibrations, estimate ranges

calibrange <- sfn_max %>% 
  mutate(
    pl_sens_meth=fct_relevel(pl_sens_meth,'HD','CHP','HR','HPTM','CHD','HFD')
  ) %>% 
  ggplot()+
 xlim(0,385)+
  geom_segment(aes(colour=pl_sens_meth,x=0,xend=sapflow_q_99,
                   y=desc(pl_sens_meth),yend=desc(pl_sens_meth)),
               show.legend = FALSE,size=2,linetype='solid',
               lineend = 'round')+
  scale_colour_viridis_d(option = "D") +
  theme_void()

# Construct plot
calib_uncert_plot<- calibSE_plot + calibrange + plot_layout(nrow=2,heights=c(3,1))

cowplot::save_plot(
  'docs/Fig_B1_calib_uncert.pdf', calib_uncert_plot, nrow = 1, 
  base_height = 21, base_width = 20, units = 'cm'
)

# 5. Function to estimate method uncertainty ---------------------------------

sf_add_uncert <- function(sfn_dataset,table_coefs,nsd=1){
  
  # nsd number of nrmse's 
  # function only for sapwood area expressed datasets
  # Method names modified to match SFN metadata specifications
  
  sfn_dataset %>% 
    get_sapf_data() %>% 
    tidyr::pivot_longer(-TIMESTAMP,names_to='tree',
                        values_to='sfd') %>% 
    # join with necessary plant metadata
    left_join(dplyr::select(get_plant_md(sfn_dataset),pl_code,pl_species,pl_dbh,
                     pl_sens_meth,pl_sens_calib),by=c('tree'='pl_code')) %>% 
    left_join(table_coefs,by=c('pl_sens_meth'='method')) %>% 
    mutate(
      # estimate error
      sfd_error = beta0+beta1*sfd,
      sfd_up=sfd + nsd*sfd_error,
      sfd_lo=sfd - nsd*sfd_error,
      # applies correction for uncalibrated HD data
      sfdcor=if_else(pl_sens_meth=='HD' & is.na(pl_sens_calib),sfd*1.405,sfd),
      sfdcor_error = beta0+beta1*sfdcor,
      sfdcor_up=sfdcor + nsd*sfdcor_error,
      sfdcor_lo=sfdcor - nsd*sfdcor_error,
    )
  
}


# 6. Examples for HD, CHP, HR - subdaily -------------------------------------


# 6.1. Read data ------------------------------------------------------

ESP_VAL_SOR_sw<- read_sfn_data('ESP_VAL_SOR',
                               folder=file.path(sfn_folder,'RData','sapwood'))
AUS_KAR_sw<- read_sfn_data('AUS_KAR',
                           folder=file.path(sfn_folder,'RData','sapwood'))
GBR_DEV_CON_sw<- read_sfn_data('GBR_DEV_CON',
                               folder=file.path(sfn_folder,'RData','sapwood'))


# 6.2. Estimate uncertainty -----------------------------------------------

esp_val_sor_unc<- sf_add_uncert(ESP_VAL_SOR_sw,table_coefs=coefficients,nsd=1) 
aus_kar_unc<- sf_add_uncert(AUS_KAR_sw,coefficients,nsd=1) 
gbr_dev_con_unc<- sf_add_uncert(GBR_DEV_CON_sw,coefficients,nsd=1) 

# Subdaily plot

esp_val_sor <- esp_val_sor_unc %>% 
  dplyr::filter(lubridate::year(TIMESTAMP)==2004) %>% 
  dplyr::filter(lubridate::yday(TIMESTAMP)%in%c(150:160)) %>% 
  dplyr::filter(tree=='ESP_VAL_SOR_Psy_Jt_12') %>% 
  ggplot(aes(x=TIMESTAMP,y=sfdcor))+
  geom_line(alpha=1)+
  geom_ribbon(aes(ymin=sfdcor_lo,ymax=sfdcor_up),alpha=0.3)+
  theme_cowplot()+
  theme(legend.position = 'top')+
  labs(x=NULL,
       y=expression(paste('Sap flow density, ',cm^3,cm^-2,h^-1)),
       title='a) Heat dissipation')

gbr_dev_plot <- gbr_dev_con_unc%>% 
  dplyr::filter(tree%in%'GBR_DEV_CON_Psy_Jt_2') %>% 
  dplyr::filter(lubridate::yday(TIMESTAMP)%in%c(150:160)) %>% 
  ggplot(aes(x=TIMESTAMP,y=sfd))+
  geom_line(alpha=1)+
  geom_ribbon(aes(ymin=sfd_lo,ymax=sfd_up),alpha=0.3)+
  theme_cowplot()+
  theme(legend.position = 'top')+
  labs(x=NULL,
       y=expression(paste('Sap flow density, ',cm^3,cm^-2,h^-1)),
       title='b) Compensation heat pulse')


aus_kar_plot<- aus_kar_unc%>% 
  dplyr::filter(tree%in%'AUS_KAR_Evi_Js_1') %>% 
  dplyr::filter(lubridate::yday(TIMESTAMP)%in%c(160:170)) %>% 
  ggplot(aes(x=TIMESTAMP,y=sfd))+
  geom_line(alpha=1)+
  geom_ribbon(aes(ymin=sfd_lo,ymax=sfd_up),alpha=0.3)+
  theme_cowplot()+
  theme(legend.position = 'top')+
  labs(x=NULL,
       y=expression(paste('Sap flow density, ',cm^3,cm^-2,h^-1)),
       title='c) Heat ratio')

uncert_hd_chp_hr<- esp_val_sor + gbr_dev_plot+ aus_kar_plot +
  plot_layout(nrow=3,heights=c(1,1,1))

cowplot::save_plot(
  'docs/Fig_B2_uncert_methods_subdaily.pdf', uncert_hd_chp_hr, nrow = 1, 
  base_height = 21, base_width = 20, units = 'cm'
)

# Apply bias correction from Flo et al 2019 -----------------------------------------------------
# subdaily

method_uncert_timseries_subd<- esp_val_sor_unc%>% 
  filter(lubridate::year(TIMESTAMP)==2004) %>% 
  filter(lubridate::yday(TIMESTAMP)%in%c(150:160)) %>%
  filter(tree%in%'ESP_VAL_SOR_Psy_Jt_12') %>% 
  ggplot(aes(x=TIMESTAMP,y=sfd))+
  geom_ribbon(aes(ymin=sfd_lo,ymax=sfd_up),fill='black',  colour=NA,alpha=0.4)+
  geom_line(aes(x=TIMESTAMP,y=sfd,col='black'))+
  geom_line(aes(x=TIMESTAMP,y=sfdcor,col='blue'))+
  geom_ribbon(aes(ymin=sfdcor_lo,ymax=sfdcor_up),fill='blue', colour=NA,alpha=0.4)+
  scale_colour_manual(name=NULL,values=c('black','blue'),
                      labels=c('HD uncorrected','HD corrected'))+
  theme_cowplot()+
  theme(legend.position = 'top')+
  xlab(NULL)+
  ylab(expression(paste('Sap flow density, ',cm^3,cm^-2,h^-1)))


cowplot::save_plot(
  'docs/Fig_method_uncert_timeseries_subd.pdf', method_uncert_timseries_subd, nrow = 1, 
  base_height = 21, base_width = 20, units = 'cm'
)

# 7. Sapwood uncertainty -----------------------------------------------------

# 7.1. Estimate sapwood uncertainty ---------------------------------------

# ESP_VAL_SOR P.sylvestris sapwood allometry

# Data
data_sw_ba <- tibble(
  sapwood_area_cm2=c(503.2, 168.3, 181.9, 147.4, 24.6, 313.6, 186.2, 32.9, 98.1, 
                     44.4, 81, 290.8, 321.8, 787.7, 554.5, 471, 961.5, 760.9),
  basal_area_cm2=c(649.2, 203.6, 212.5, 160.6, 28.3, 371.5, 246.1, 44.2, 122.7, 
                   51.5, 105.7, 356.3, 404.7, 956.6, 613.6, 559.9, 1023.5, 989.8)
)
# Model
modsw <- lm(log(sapwood_area_cm2)~log(basal_area_cm2),data=data_sw_ba)

# Calculations
esp_val_sor_unc_sf <- esp_val_sor_unc %>% 
  mutate(pl_basal_area=pi*(pl_dbh/2)^2,
         sw_area_mean=exp(predict(modsw,newdata=data.frame(basal_area_cm2=pl_basal_area),
                                  level=0.67, interval='prediction')[,'fit']),
         sw_area_lo=exp(predict(modsw,newdata=data.frame(basal_area_cm2=pl_basal_area),
                                level=0.67, interval='prediction')[,'lwr']),
         sw_area_up=exp(predict(modsw,newdata=data.frame(basal_area_cm2=pl_basal_area),
                                level=0.67, interval='prediction')[,'upr']),
         # Estimate 1 SE as half the dif between mean and 95% upper bound (~2SE) 
         
         swarea_error=(sw_area_up-sw_area_mean),
         method_sfdcor_error=sfdcor_error,
        
          # Sw area uncertainty
         sf_mean = sfd*sw_area_mean,
         sf_uncsw_lo = sfd*sw_area_lo,
         sf_uncsw_up = sfd*sw_area_up,
         
         # Method uncertainty
         sf_uncmeth_lo = sfd_lo*sw_area_mean,
         sf_uncmeth_up = sfd_up*sw_area_mean,
         
         # Sw area, corrected
         sfcor_mean = sfdcor*sw_area_mean,
         sfcor_uncsw_lo = sfdcor*sw_area_lo,
         sfcor_uncsw_up = sfdcor*sw_area_up,
         
         # Method unc
         sfcor_uncmeth_lo = sfdcor_lo*sw_area_mean,
         sfcor_uncmeth_up = sfdcor_up*sw_area_mean,
         
         # Combined uncertainty
         sfcor_totfracunc=sqrt((method_sfdcor_error/sfdcor)^2+
                              (swarea_error/sw_area_mean)^2),
         sfcor_totunc = sfcor_mean*sfcor_totfracunc,
         sfcor_totunc_up=sfcor_mean+sfcor_totunc,
         sfcor_totunc_lo=sfcor_mean-sfcor_totunc,
         
         sfcor_totunc2 = sqrt(((sfcor_uncmeth_up-sfcor_uncmeth_lo)/2)^2 +
                                ((sfcor_uncsw_up-sfcor_uncsw_lo)/2)^2),
         
         sfcor_totunc2_up=sfcor_mean+sfcor_totunc2,
         sfcor_totunc2_lo=sfcor_mean-sfcor_totunc2,
         
  )

# 7.2. Compare method uncertainty with sapwood uncertainty ---------------------

# Subdaily

# Plot
Sys.setlocale("LC_TIME", "en_US.UTF-8")
esp_val_sor_unc_sf_plot_subd<- esp_val_sor_unc_sf %>%
  filter(lubridate::year(TIMESTAMP)==2004) %>% 
  filter(lubridate::yday(TIMESTAMP)%in%c(150:160)) %>% 
  filter(tree=='ESP_VAL_SOR_Psy_Jt_12') %>% 
  ggplot(aes(x=TIMESTAMP,y=sfcor_mean))+
  geom_line(aes(col='black'))+
  geom_ribbon(aes(ymin=sfcor_uncsw_lo,ymax=sfcor_uncsw_up,
                  fill='red'), colour=NA,alpha=0.7)+
  geom_ribbon(aes(ymin=sfcor_uncmeth_lo,ymax=sfcor_uncmeth_up,
                  fill='blue'), colour=NA,alpha=0.3)+
  scale_colour_manual(name=NULL,values=c('black'),
                      labels=c('HD corrected'))+
  scale_fill_manual(name=NULL,values=c('red','blue'),
                    labels=c('method','sapwood'))+
  theme_cowplot()+
  theme(legend.position = 'top',legend.title=element_blank())+
  labs(x=NULL,
       y=expression(paste('Sap flow, ',cm^3,h^-1)))

# Combined uncertainty

# Subdaily
Sys.setlocale("LC_TIME", "en_US.UTF-8")
esp_val_sor_totunc_sf_plot_subd<- esp_val_sor_unc_sf %>% 
  filter(lubridate::year(TIMESTAMP)==2004) %>% 
  filter(lubridate::yday(TIMESTAMP)%in%c(150:160)) %>% 
  filter(tree=='ESP_VAL_SOR_Psy_Jt_12') %>% 
  ggplot(aes(x=TIMESTAMP,y=sfcor_mean))+
  geom_line(aes(col='black'))+
  geom_ribbon(aes(ymin=sfcor_totunc2_lo,
                  ymax=sfcor_totunc2_up,
                  fill='gray'), colour=NA,alpha=0.5)+
  scale_colour_manual(name=NULL,values=c('black'),
                      labels=c('HD corrected'))+
  scale_fill_manual(name=NULL,values=c('gray'),
                    labels=c('Total uncertainty'))+
  theme_cowplot()+
  theme(legend.position = 'top',legend.title=element_blank())+
  labs(x=NULL,
       y=expression(paste('Sap flow, ',cm^3,h^-1)))


# Check values

esp_val_sor_unc_sf %>% 
  filter(tree=='ESP_VAL_SOR_Psy_Jt_12') %>% 
  filter(TIMESTAMP==as.POSIXct('2004-06-04 11:15:00',tz='GMT') |
           TIMESTAMP==as.POSIXct('2004-05-30 11:15:00',tz='GMT')) %>% View()

esp_val_sor_unc_sf %>% 
  filter(tree=='ESP_VAL_SOR_Psy_Jt_12') %>% 
  filter(lubridate::yday(TIMESTAMP)%in%c(150:160)) %>% 
  select(TIMESTAMP,sfcor_mean,sfcor_totunc) %>% View()

# Compare days with low (20/05/2004) and high flow (4/6/2004)

esp_val_sor_totunc_sf_daily %>% 
  filter(tree=='ESP_VAL_SOR_Psy_Jt_12') %>%  
           filter(day==as.POSIXct('2004-06-04',tz='GMT') |
                    day==as.POSIXct('2004-05-30',tz='GMT')) 

# Construct plot subdaily
Sys.setlocale("LC_TIME", "en_US.UTF-8")
esp_val_sor_plot_subd<- method_uncert_timseries_subd + 
  esp_val_sor_unc_sf_plot_subd + esp_val_sor_totunc_sf_plot_subd+
  plot_layout(nrow=3,heights=c(1,1,1))+
  plot_annotation(tag_levels = list(c('     (a)','     (b)', '     (c)')))

cowplot::save_plot(
  'docs/Fig_B3_uncert_hd_subd.pdf', esp_val_sor_plot_subd, nrow = 1, 
  base_height = 21, base_width = 20, units = 'cm'
)

# 8. A test of radial scaling ---------------------------------------------

# Survey trees for example
# sfn_metadata_sw$plant_md %>% 
#   filter(pl_radial_int=='No radial correction' | is.na(pl_radial_int)) %>% 
#            filter(!is.na(pl_sapw_depth) & !is.na(pl_sens_length)) %>% 
#   select(pl_code,pl_species,pl_sapw_depth,pl_sens_length) %>% View()

# 8.1. Read example data

USA_UMB_CON_sw<- read_sfn_data('USA_UMB_CON',
                               folder=file.path(sfn_folder,'RData','sapwood'))

# Select one year, one species of each type
umb_rad_data<- umb_2013_sp <- USA_UMB_CON_sw %>% 
  get_sapf_data() %>% 
  tidyr::pivot_longer(-TIMESTAMP,names_to='pl_code',
                      values_to='sfd') %>% 
  filter(lubridate::year(TIMESTAMP)==2013) %>% 
  filter(lubridate::yday(TIMESTAMP)%in%c(155:165)) %>% 
  # Join plant metadata
  left_join(get_plant_md(USA_UMB_CON_sw)) %>%
  # Select species
  filter(pl_species%in%c('Populus grandidentata','Pinus strobus',
                         'Quercus rubra')) %>% 
  # Wood type
  mutate(
    wood_type=case_when(
    pl_species=='Populus grandidentata'~'Diffuse-porous',
    pl_species=='Quercus rubra'~'Ring-porous',
    pl_species=='Pinus strobus'~'Tracheid'
  )) 
  


# 8.2. Implement radial upscaling and uncertainty estimation --------------

# See Berdanier et al Tree Phys 2016
# https://github.com/berdaniera/sapflux

umb_rad_uncert <- umb_rad_data %>% 
  #Rowwise to allow qtot calculations
  rowwise() %>% 
  mutate(
    sfd_norad=sfd*pl_sapw_area,
    # Apply radially integration according to wood type
    # Result of qtot is a matrix with n bootstrap estimates
    # 'apply' to get mean and quantiles of boot samples
    # units converted to g m-2 s-1
    sfd_boot=sapflux::qtot(sfd*100/36,a=0,b=pl_sens_length/1000, woodType=wood_type,
                  uncertainty=TRUE,nboot=100,treeRadius=0.5*pl_dbh/100,
                  sapRadius=pl_sapw_depth/100),
    sfdrad_mean=apply(sfd_boot,1,mean,na.rm=TRUE)*3600,
    sfdrad_low=apply(sfd_boot,1,quantile,prob=0.170,na.rm=TRUE)*3600,
    sfdrad_up=apply(sfd_boot,1,quantile,prob=0.830,na.rm=TRUE)*3600,
  ) %>% 
  dplyr::select(-sfd_boot)

# 8.3. Create plot --------------------------------------------------------

f_labels <- data.frame(pl_code = c('USA_UMB_CON_Pst_Js_2',
                               'USA_UMB_CON_Qru_Js_10',
                               'USA_UMB_CON_Pgr_Js_3'), 
                       label = c("(b)", "(a)", "(c)"))


umb_rad_uncert_plot<- umb_rad_uncert %>% 
  arrange(TIMESTAMP,pl_code) %>% 
  filter(pl_code%in%c('USA_UMB_CON_Pst_Js_2',
                       'USA_UMB_CON_Qru_Js_10',
                       'USA_UMB_CON_Pgr_Js_3')) %>% 
  ggplot(aes(x=TIMESTAMP,y=sfd_norad,col='black'))+
  geom_line()+
  geom_line(aes(x=TIMESTAMP,y=sfdrad_mean,col='blue'))+
  geom_ribbon(aes(ymin=sfdrad_low,ymax=sfdrad_up),
              colour=NA,fill='blue',alpha=0.2)+
  geom_label(aes(label=paste(pl_species,wood_type,sep=',')), 
             x = Inf, y = Inf, hjust=1, vjust=1,show.legend = FALSE)+
  geom_text(x=-Inf, y = Inf, aes(label = label), data = f_labels,
            hjust=-0.3,vjust=1,
           size=8, show.legend=FALSE)+
  scale_colour_manual(name=NULL,values=c('black','blue'),
                      labels=c('Sap flow','Sap flow radially integrated'))+
  theme_cowplot()+
  labs(x=NULL,
       y= expression(paste('Sap flow, ',cm^3,h^-1)))+
  facet_grid(rows=vars(pl_code),scales='free_y',as.table=FALSE)+
  theme(strip.text.y = element_text(size=10),legend.position='top')


cowplot::save_plot(
  'docs/Fig_B4_rad_uncert.pdf', umb_rad_uncert_plot, nrow = 1, 
  base_height = 21, base_width = 20, units = 'cm'
)


