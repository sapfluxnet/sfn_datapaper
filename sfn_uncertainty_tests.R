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

Sys.setlocale("LC_TIME", "en_US.UTF-8")

# 1. Calibrations data ----------------------------------------------------

# Read data
subdata <- read.csv("data/subdata.csv")

#calculate range of each calibration
range_df <- subdata %>% group_by(calibrations) %>%
  dplyr::summarize(min_real=min(real),
                   max_real = max(real),
                   range_mean = (min_real + max_real)/2)

# Approach 1. Based on range ----------------------------------------------

#Calculate standard error at range_mean of each calibration 
df <- subdata %>% group_by(calibrations) %>%
  do(broom::tidy(lm( real ~ measured, data = .))) %>%
  dplyr::select(calibrations, term, std.error) %>%
  tidyr::spread(term, std.error) %>%
  dplyr::rename(slope_error=measured,intercept_error=`(Intercept)`)%>%
  right_join(range_df,by = 'calibrations') %>%
  dplyr::mutate(SE_mean = intercept_error + slope_error * range_mean) %>% 
  right_join(dplyr::select(subdata,'study','specie','method','calibrations'), 
             by = 'calibrations') %>%
  unique()


range_df_method <- df %>% 
  group_by(method)%>%
  summarise(min_method = min(min_real),
            max_method = max(max_real)
  )

# Approach 1 --------------------------------------------------------------

#Model standard error
foo_se <- lmer(SE_mean ~  range_mean * method + (1|study) + (1|specie), 
               data=df)

#extract error coefficients
coefficients_1 <- emmeans::emmeans(foo_se,~range_mean * method, 
                                   adjust="tukey",at = list(range_mean = 0)) %>% 
  as_tibble() %>%
  dplyr::select(method,beta0=emmean) %>% 
  left_join(emmeans::emtrends(foo_se,"method",var='range_mean') %>%
              as_tibble() %>% 
              dplyr::select(method,beta1=range_mean.trend),by='method')%>% 
  mutate(method=fct_recode(method,HPTM="T-max",HD="TD",CHD="TTD"))  


# Approach 2 --------------------------------------------------------------

#Calculate standard error for each method and add range_df
# Model with singularity issues - discard

df2 <- subdata %>% group_by(method) %>% 
  do(broom.mixed::tidy(lmer( real ~ measured+ (1|study/calibrations) + 
                               (1|specie), data = .))) %>%
  dplyr::filter(!is.na(std.error)) %>% 
  dplyr::select(method, term, std.error) %>%
  tidyr::spread(term, std.error) %>%
  dplyr::rename(beta1 = measured,
                beta0 = `(Intercept)`)%>%
  left_join(range_df_method) %>% 
  mutate(
    method=as.factor(method),
    method=fct_recode(method,HPTM="T-max",HD="TD",CHD="TTD"))  

# Approach 3 - 

#Calculate standard error for each method and add range_df

df3 <- subdata %>% group_by(method) %>% 
  do(broom.mixed::tidy(lmer( real ~ measured+ (1|calibrations), data = .))) %>%
  dplyr::filter(!is.na(std.error)) %>% 
  dplyr::select(method, term, std.error) %>%
  tidyr::spread(term, std.error) %>%
  dplyr::rename(beta1 = measured,
                beta0 = `(Intercept)`)%>%
  left_join(range_df_method) %>% 
  mutate(
    method=as.factor(method),
    method=fct_recode(method,HPTM="T-max",HD="TD",CHD="TTD"))  


# Choose approach

coefficients <- df3

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
# Model coefficients


# 2. Sapfluxnet daily data ------------------------------------------------

# Load daily data
load('data/sapwood.Rdata')

sfn_folder <- 'data/0.1.5'
sfn_metadata_sw <- read_sfn_metadata(folder=file.path(sfn_folder,'RData','sapwood'))

methods_included <- c('CHD','CHP','HD','HFD','HPTM','HR')

# Process daily data: selection, filtering, join SEcoefs, SE estimation
sfn_daily_all_uncert <- sapwood %>%
  # select variables
  dplyr::select(TIMESTAMP,si_code,pl_code,sapflow_mean,
         sapflow_q_95,sapflow_q_99,
         sapflow_coverage,sapflow_n,vpd_mean) %>%
  # join with plant metadata
  left_join(dplyr::select(sfn_metadata_sw$plant_md,pl_code,pl_species,
                          pl_sens_meth,pl_sens_calib),
            by='pl_code') %>%
  dplyr::filter(sapflow_coverage>90 & pl_sens_meth%in%methods_included) %>% 
  # join with method-sepcific coefs to estimate SE
  left_join(coefficients,by=c('pl_sens_meth'='method')) %>% 
  mutate(
    sapflow_SE = beta0 + beta1* sapflow_mean 
  )



# 3. Compare SE estimation with flow range in calibrations ---------------------------------------

# Get Q95 values across methods
sfn_max <- sfn_daily_all_uncert %>% 
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

# Function to estimate method uncertainty ---------------------------------

sf_add_uncert <- function(sfn_dataset,table_coefs,nsd=1){
  
  # nsd number of nrmse's 
  # TODO: function only for sapwood area expressed datasets
  
  # Method names modified to match SFN metadata specifications
  # TODO: function could also allow user-specified values
  
  # TODO: add a check to make sure that method is included

  
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


# Examples for HD, CHP, HR - subdaily -------------------------------------


# data ------------------------------------------------------

ESP_VAL_SOR_sw<- read_sfn_data('ESP_VAL_SOR',
                               folder=file.path(sfn_folder,'RData','sapwood'))
AUS_KAR_sw<- read_sfn_data('AUS_KAR',
                           folder=file.path(sfn_folder,'RData','sapwood'))
GBR_DEV_CON_sw<- read_sfn_data('GBR_DEV_CON',
                               folder=file.path(sfn_folder,'RData','sapwood'))

esp_val_sor_unc<- sf_add_uncert(ESP_VAL_SOR_sw,table_coefs=coefficients,nsd=1) 
aus_kar_unc<- sf_add_uncert(AUS_KAR_sw,coefficients,nsd=1) 
gbr_dev_con_unc<- sf_add_uncert(GBR_DEV_CON_sw,coefficients,nsd=1) 

# Subdaily

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

# Example application -----------------------------------------------------


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


# daily - new

method_uncert_timseries<- esp_val_sor_unc%>% 
  filter(lubridate::year(TIMESTAMP)==2004) %>% 
  filter(lubridate::yday(TIMESTAMP)%in%c(120:270)) %>% 
  group_by(tree,day=lubridate::floor_date(TIMESTAMP,'day')) %>%
  summarise(
   sfd_day = mean(sfd,na.rm=TRUE),
   sfdcor_day = mean(sfdcor,na.rm=TRUE),
   sfd_err_day = 1/length(sfd_error)*sqrt(sum(sfd_error^2)),
   sfdcor_err_day = 1/length(sfdcor_error)*sqrt(sum(sfdcor_error^2))
    ) %>% filter(tree%in%'ESP_VAL_SOR_Psy_Jt_12')%>% 
  ggplot(aes(x=day,y=sfd_day))+
  geom_ribbon(aes(ymin=sfd_day-sfd_err_day,ymax=sfd_day+sfd_err_day),
              fill='black',  colour=NA,alpha=0.4)+
  geom_line(aes(x=day,y=sfd_day,col='black'))+
  geom_line(aes(x=day,y=sfdcor_day,col='blue'))+
  geom_ribbon(aes(ymin=sfdcor_day-sfdcor_err_day,
                  ymax=sfdcor_day+sfdcor_err_day),
              fill='blue', colour=NA,alpha=0.4)+
  scale_colour_manual(name=NULL,values=c('black','blue'),
                      labels=c('HD uncorrected','HD corrected'))+
  theme_cowplot()+
  theme(legend.position = 'top')+
  xlab(NULL)+
  ylab(expression(paste('Sap flow density, ',cm^3,cm^-2,h^-1)))


cowplot::save_plot(
  'docs/Fig_method_uncert_timeseries_daily.pdf', method_uncert_timseries, nrow = 1, 
  base_height = 21, base_width = 20, units = 'cm'
)


# Sapwood uncertainty -----------------------------------------------------

# Vallcebre P.sylvestris sapwood allometry

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

# Compare method uncertainty with sapwood uncertainty ---------------------

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
                  fill='red'), colour=NA,alpha=0.3)+
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


# Daily

# Plot new
Sys.setlocale("LC_TIME", "en_US.UTF-8")
esp_val_sor_unc_sf_plot2<- esp_val_sor_unc_sf%>% 
  filter(lubridate::year(TIMESTAMP)==2004) %>% 
  filter(lubridate::yday(TIMESTAMP)%in%c(120:270)) %>% 
  group_by(tree,day=lubridate::floor_date(TIMESTAMP,'day')) %>%
  summarise(
    sfd_day = mean(sfd,na.rm=TRUE),
    sfdcor_day = mean(sfdcor,na.rm=TRUE),
    sfd_err_day = 1/length(sfd_error)*sqrt(sum(sfd_error^2)),
    sfdcor_err_day = 1/length(sfdcor_error)*sqrt(sum(sfdcor_error^2))
  ) %>% filter(tree%in%'ESP_VAL_SOR_Psy_Jt_12')%>% 
  ggplot(aes(x=day,y=sfcor_mean*24/1000))+
  geom_line(aes(col='black'))+
  geom_ribbon(aes(ymin=sfcor_uncsw_lo*24/1000,ymax=sfcor_uncsw_up*24/1000,
                  fill='red'), colour=NA,alpha=0.3)+
  geom_ribbon(aes(ymin=sfcor_uncmeth_lo*24/1000,ymax=sfcor_uncmeth_up*24/1000,
                  fill='blue'), colour=NA,alpha=0.3)+
  scale_colour_manual(name=NULL,values=c('black'),
                      labels=c('HD corrected'))+
  scale_fill_manual(name=NULL,values=c('red','blue'),
                    labels=c('method','sapwood'))+
  theme_cowplot()+
  theme(legend.position = 'top',legend.title=element_blank())+
  labs(x=NULL,
       y= expression(paste('Sap flow, kg·', day^-1)))


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
                  fill='gray'), colour=NA,alpha=0.8)+
  scale_colour_manual(name=NULL,values=c('black'),
                      labels=c('HD corrected'))+
  scale_fill_manual(name=NULL,values=c('gray'),
                    labels=c('Total uncertainty'))+
  theme_cowplot()+
  theme(legend.position = 'top',legend.title=element_blank())+
  labs(x=NULL,
       y=expression(paste('Sap flow, ',cm^3,h^-1)))


# Daily

esp_val_sor_totunc_sf_daily<- esp_val_sor_unc_sf %>% 
  group_by(tree,day=lubridate::floor_date(TIMESTAMP,'day')) %>%
  filter(lubridate::year(TIMESTAMP)==2004) %>% 
  filter(lubridate::yday(TIMESTAMP)%in%c(120:270)) %>% 
  summarise(
   
    sfcor_day = mean(sfcor_mean,na.rm=TRUE),
    sfcor_daytotunc = 1/length(sfcor_totunc[!is.na(sfcor_totunc)])*sqrt(sum(sfcor_totunc^2,na.rm=TRUE))
    
  ) 


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


# Plot

Sys.setlocale("LC_TIME", "en_US.UTF-8")
esp_val_sor_totunc_sf_plot_daily <-esp_val_sor_totunc_sf_daily %>%  
  filter(tree=='ESP_VAL_SOR_Psy_Jt_12')%>% 
  ggplot(aes(x=day,y=sfcor_day*24/1000))+
  geom_line(aes(col='black'))+
  geom_ribbon(aes(ymin=(sfcor_day-sfcor_daytotunc)*24/1000,
                  ymax=(sfcor_day+sfcor_daytotunc)*24/1000,
                  fill='gray'), colour=NA,alpha=0.8)+
  scale_colour_manual(name=NULL,values=c('black'),
                      labels=c('HD corrected'))+
  scale_fill_manual(name=NULL,values=c('gray'),
                    labels=c('Total uncertainty'))+
  theme_cowplot()+
  theme(legend.position = 'top',legend.title=element_blank())+
  labs(x=NULL,
       y= expression(paste('Sap flow, kg·', day^-1)))





# Construct plot

esp_val_sor_plot<- method_uncert_timseries + 
  esp_val_sor_unc_sf_plot + esp_val_sor_totunc_sf_plot+
plot_layout(nrow=3,heights=c(1,1,1))+
plot_annotation(tag_levels = list(c('     (a)','     (b)', '     (c)')))

cowplot::save_plot(
  'docs/Fig_B3_uncert_hd.pdf', esp_val_sor_plot, nrow = 1, 
  base_height = 21, base_width = 20, units = 'cm'
)

# Construct plot subdaily
Sys.setlocale("LC_TIME", "en_US.UTF-8")
esp_val_sor_plot_subd<- method_uncert_timseries_subd + 
  esp_val_sor_unc_sf_plot_subd + esp_val_sor_totunc_sf_plot_subd+
  plot_layout(nrow=3,heights=c(1,1,1))+
  plot_annotation(tag_levels = list(c('     (a)','     (b)', '     (c)')))

cowplot::save_plot(
  'docs/Fig_B3_uncert_hd_subd.pdf', esp_val_sor_plot, nrow = 1, 
  base_height = 21, base_width = 20, units = 'cm'
)


# 4. A test of radial scaling ---------------------------------------------

# Survey trees for example
# sfn_metadata_sw$plant_md %>% 
#   filter(pl_radial_int=='No radial correction' | is.na(pl_radial_int)) %>% 
#            filter(!is.na(pl_sapw_depth) & !is.na(pl_sens_length)) %>% 
#   select(pl_code,pl_species,pl_sapw_depth,pl_sens_length) %>% View()

# Read example data

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
  
# Implement radial upscaling and uncertainty estimation in
# Berdanier & Clark, Tree Phys 2015
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


# Create plot

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



# Old code ----------------------------------------------------------------



# # Plot old
# Sys.setlocale("LC_TIME", "en_US.UTF-8")
# esp_val_sor_unc_sf_plot<- esp_val_sor_unc_sf %>% 
#   group_by(tree,day=lubridate::floor_date(TIMESTAMP,'day')) %>%
#   filter(lubridate::year(TIMESTAMP)==2004) %>% 
#   filter(lubridate::yday(TIMESTAMP)%in%c(120:270)) %>% 
#   summarise(
#     across(contains('sf'),mean,na.rm=TRUE)
#   ) %>% 
#   filter(tree=='ESP_VAL_SOR_Psy_Jt_12') %>% 
#   ggplot(aes(x=day,y=sfcor_mean*24/1000))+
#   geom_line(aes(col='black'))+
#   geom_ribbon(aes(ymin=sfcor_uncsw_lo*24/1000,ymax=sfcor_uncsw_up*24/1000,
#                   fill='red'), colour=NA,alpha=0.3)+
#   geom_ribbon(aes(ymin=sfcor_uncmeth_lo*24/1000,ymax=sfcor_uncmeth_up*24/1000,
#                   fill='blue'), colour=NA,alpha=0.3)+
#   scale_colour_manual(name=NULL,values=c('black'),
#                       labels=c('HD corrected'))+
#   scale_fill_manual(name=NULL,values=c('red','blue'),
#                     labels=c('method','sapwood'))+
#   theme_cowplot()+
#   theme(legend.position = 'top',legend.title=element_blank())+
#   labs(x=NULL,
#        y= expression(paste('Sap flow, kg·', day^-1)))
# 



# # daily
# 
# method_uncert_timseries<- esp_val_sor_unc%>% 
#   filter(lubridate::year(TIMESTAMP)==2004) %>% 
#   filter(lubridate::yday(TIMESTAMP)%in%c(120:270)) %>% 
#   group_by(tree,day=lubridate::floor_date(TIMESTAMP,'day')) %>%
#   summarise(
#     across(contains('sfd'),mean,na.rm=TRUE)) %>%
#   filter(tree%in%'ESP_VAL_SOR_Psy_Jt_12') %>% 
#   ggplot(aes(x=day,y=sfd))+
#   geom_ribbon(aes(ymin=sfd_lo,ymax=sfd_up),fill='black',  colour=NA,alpha=0.4)+
#   geom_line(aes(x=day,y=sfd,col='black'))+
#   geom_line(aes(x=day,y=sfdcor,col='blue'))+
#   geom_ribbon(aes(ymin=sfdcor_lo,ymax=sfdcor_up),fill='blue', colour=NA,alpha=0.4)+
#   scale_colour_manual(name=NULL,values=c('black','blue'),
#                       labels=c('HD uncorrected','HD corrected'))+
#   theme_cowplot()+
#   theme(legend.position = 'top')+
#   xlab(NULL)+
#   ylab(expression(paste('Sap flow density, ',cm^3,cm^-2,h^-1)))
# 

