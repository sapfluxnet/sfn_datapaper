library(tidyverse)
library(emmeans)
#library(lsmeans)
library(lme4)
library(sapfluxnetr)
# library(LSD) # this is Beni's forked version
# https://stineb.github.io/post/lsd/
# https://github.com/stineb/LSD
library(ggridges)
library(cowplot)
library(patchwork)
library(viridis)
library(sapflux)
library(merTools)
library(bootpredictlme4)
library(ggeffects)
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# 2. Sapfluxnet daily data ------------------------------------------------

# Load daily data
load('data/sapwood.Rdata')

sfn_folder <- 'data/0.1.5'
sfn_metadata_sw <- read_sfn_metadata(folder=file.path(sfn_folder,'RData','sapwood'))

methods_included <- c('CHD','CHP','HD','HFD','HPTM','HR')

# Process daily data: selection, filtering, join SEcoefs, SE estimation
sfn_daily_all_uncert <- sapwood %>%
  # select variables
  select(TIMESTAMP,si_code,pl_code,sapflow_mean,
         sapflow_q_95,sapflow_q_99,
         sapflow_coverage,sapflow_n,vpd_mean) %>%
  # join with plant metadata
  left_join(select(sfn_metadata_sw$plant_md,pl_code,pl_species,pl_sens_meth,pl_sens_calib),
            by='pl_code') %>%
  filter(sapflow_coverage>90 & pl_sens_meth%in%methods_included) %>% 
  # join with method-sepcific coefs to estimate SE
  left_join(coefficients,by=c('pl_sens_meth'='method')) %>% 
  mutate(
    sapflow_SE = beta0 + beta1* sapflow_mean 
  )

# 1. Calibrations data ----------------------------------------------------

# Read data
subdata <- read.csv("data/subdata.csv")

#calculate range of each calibration
range_df <- subdata %>% group_by(method) %>%
  dplyr::summarize(min_method=min(real),
                   max_method = max(real),
                   range_mean_method = (min_method + max_method)/2)

# Approach 1. Based on range ----------------------------------------------

#Calculate standard error at range_mean of each calibration 
df <- subdata %>% group_by(calibrations) %>%
  do(broom::tidy(lm( real ~ measured, data = .))) %>%
  select(calibrations, term, std.error) %>%
  tidyr::spread(term, std.error) %>%
  dplyr::rename(slope_error=measured,intercept_error=`(Intercept)`)%>%
  right_join(range_df,by = 'calibrations') %>%
  dplyr::mutate(SE_mean = intercept_error + slope_error * range_mean) %>% 
  right_join(select(subdata,'study','specie','method','calibrations'), 
             by = 'calibrations') %>%
  unique()


range_df_method <- df %>% 
  group_by(method)%>%
  summarise(min_method = min(min_real),
            max_method = max(max_real)
  )

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
  do(broom.mixed::tidy(lmer( real ~ measured+ (1|study/calibrations) + (1|specie), data = .))) %>%
  filter(!is.na(std.error)) %>% 
  select(method, term, std.error) %>%
  tidyr::spread(term, std.error) %>%
  dplyr::rename(beta1 = measured,
                beta0 = `(Intercept)`)%>%
  left_join(range_df) %>% 
  mutate(
    method=as.factor(method),
    method=fct_recode(method,HPTM="T-max",HD="TD",CHD="TTD"))  

# Approach 3 - 

#Calculate standard error for each method and add range_df

df3 <- subdata %>% group_by(method) %>% 
  do(broom.mixed::tidy(lmer( real ~ measured+ (1|calibrations), data = .))) %>%
  filter(!is.na(std.error)) %>% 
  select(method, term, std.error) %>%
  tidyr::spread(term, std.error) %>%
  dplyr::rename(beta1 = measured,
                beta0 = `(Intercept)`)%>%
  left_join(range_df) %>% 
  mutate(
    method=as.factor(method),
    method=fct_recode(method,HPTM="T-max",HD="TD",CHD="TTD"))  


# Approach 4 log model


subdata %>% 
  group_by(method) %>% 
  group_map(~lmer( log(real) ~ log(measured)+ (1|calibrations), data = .),
            .keep=TRUE) 


lmmodels <- subdata %>% 
  group_by(method) %>% 
  nest() %>% 
  mutate(
    fit = map(data,~lmer( real ~ measured+ (1|calibrations),data=.))
  ) 

lmmodels$fit %>% 
  map(plot)

logmodels <- subdata %>% 
  group_by(method) %>% 
  nest() %>% 
  mutate(
    fit = map(data,~lmer( log(real) ~ log(measured)+ (1|calibrations),data=.))
  ) 

logmodels$fit %>% 
  map(plot)


# Bootstrap SEs
dfhd <- data.frame(method='TD',measured=seq(1,200,10))
 
selog_HD<- predict(logmodels$fit[[1]],nsim=100,
        newdata=dfhd,re.form=NA,se.fit=TRUE)$se.boot

selog_HD<- predict(lmmodels$fit[[1]],nsim=100,
                   newdata=dfhd,re.form=NA,se.fit=TRUE)$se.fit

data.frame(sfd=seq(1,200,10),se=exp(selog_HD))

# Mannual


err_vic <- function(b0,b1,se_b0,se_b1,sfd){
  
  err_up <- exp(b0)*exp(se_b0)+(exp(b1)*exp(se_b1))^log(sfd)
  err_lo <- exp(b0)/exp(se_b0)+((exp(b1)/exp(se_b1)))^log(sfd)
  return(c(err_up,err_lo))
  
}


err_rf <- function(b0,b1,se_b0,se_b1,sfd){
  
  err_up <- exp(b0)*exp(se_b0)*exp(b1*log(sfd))*exp(se_b1*log(sfd))
  err_lo <- exp(b0)*exp(-se_b0)*exp((b1*log(sfd)))*exp(-se_b1*log(sfd))
  return(c(err_up,err_lo))
  
}


err_vic(0.1,1,0.01,0.01,20)
err_rf(0.1,1,0.01,0.01,20)

err_log_4<- df4 %>% 
  mutate(
    se_up_min = err_vic(b0=beta0,b1=beta1,se_b0=se_beta0,se_b1=se_beta1,
                        sfd=min_method)[1],
    se_lo_min = err_vic(b0=beta0,b1=beta1,se_b0=se_beta0,se_b1=se_beta1,
                        sfd=min_method)[2],
    se_up_max = err_vic(b0=beta0,b1=beta1,se_b0=se_beta0,se_b1=se_beta1,
                        sfd=max_method)[1],
    se_lo_max = err_vic(b0=beta0,b1=beta1,se_b0=se_beta0,se_b1=se_beta1,
                        sfd=max_method)[2],
  ) 


# From predicted CI 
err_log_HD<- sjPlot::get_model_data(logmodels$fit[[2]],
                                    'pred',ci.lvl=0.67) %>% as.data.frame() %>% 
  
  
  ggpredict(logmodels$fit[[2]],ci.lvl=0.67)
  
ggpredict(logmodels$fit[[2]],ci.lvl=0.67,interval='prediction')


df4 <- subdata %>% group_by(method) %>% 
  do(broom.mixed::tidy(lmer( log(real) ~ log(measured)+ (1|calibrations), data = .))) %>%
  filter(!is.na(std.error)) %>% 
  dplyr::select(method, term, std.error,estimate) %>%
  tidyr::pivot_wider(values_from=c(estimate,std.error),
                     names_from=c(term)) %>%
  dplyr::rename(beta0 = 'estimate_(Intercept)',
                beta1 = 'estimate_log(measured)',
                se_beta0= 'std.error_(Intercept)',
                se_beta1= 'std.error_log(measured)')%>%
  left_join(range_df) %>% 
  mutate(
    method=as.factor(method),
    method=fct_recode(method,HPTM="T-max",HD="TD",CHD="TTD"))  







# Choose approach

coefficients <- df3

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
  


# log option
sfn_max_log <- sfn_daily_all_uncert %>% 
  group_by(pl_sens_meth) %>% 
  summarise(
    across(contains('q_'),~max(.x,na.rm=TRUE)),
  ) %>% 
  left_join(df4,by=c('pl_sens_meth'='method')) %>% 
  mutate(
    sfdq95_SE = exp(beta0 + beta1* log(sapflow_q_95)),
    sfdq99_SE = exp(beta0 + beta1* log(sapflow_q_99)),
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
  

calibSE_plot_log <-  sfn_max_log%>% 
  ggplot() + xlim(0,385)+
  geom_segment(aes(colour=pl_sens_meth,
                   x=min_method,xend=max_method,
                   y=exp(beta0+beta1*log(min_method)),
                   yend=exp(beta0+beta1*log(max_method))),size=1.5)+
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
  'docs/Fig_calib_uncert.pdf', calib_uncert_plot, nrow = 1, 
  base_height = 21, base_width = 20, units = 'cm'
)

# Function to estimate method uncertainty ---------------------------------

sf_add_uncert <- function(sfn_dataset,table_coefs,nsd=1){
  
  # nsd number of nrmse's 
  # TODO: function only for sapwood area expressed datasets
  
  # Table 2 from Flo et al 2019
  # Method names modified to match SFN metadata specifications
  # TODO: function could also allow user-specified values
  
  # TODO: add a check to make sure that method is included in
  # nrmse_flo
  
  sfn_dataset %>% 
    get_sapf_data() %>% 
    tidyr::pivot_longer(-TIMESTAMP,names_to='tree',
                        values_to='sfd') %>% 
    # join with necessary plant metadata
    left_join(select(get_plant_md(sfn_dataset),pl_code,pl_species,pl_dbh,
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


logmodels <- subdata %>% 
  group_by(method) %>% 
  nest() %>% 
  mutate(
    fit = map(data,~lmer( log(real) ~ log(measured)+ (1|calibrations),data=.)),
    method=forcats::fct_recode(method,HPTM="T-max",HD="TD",CHD="TTD"))


foo <- map(logmodels[logmodels$method=='HD',"fit"],1)$fit


logmodels[logmodels$method=='HD',"fit"][1]

sf_add_uncert_log <- function(sfn_dataset){
  
  fitmod <- map(logmodels[logmodels$method=='HD',"fit"],1)$fit
  
  print(summary(fitmod))
  
  dat_unc <-  sfn_dataset %>% 
    get_sapf_data() %>% 
    tidyr::pivot_longer(-TIMESTAMP,names_to='tree',
                        values_to='sfd') %>% 
    # join with necessary plant metadata
    left_join(dplyr::select(get_plant_md(sfn_dataset),pl_code,pl_species,pl_dbh,
                     pl_sens_meth,pl_sens_calib),by=c('tree'='pl_code')) 
   
   dat_unc$se_lo <- 
             as.numeric(as.data.frame(ggpredict(fitmod,terms='measured[dat_unc$sfd]',
                     ci.lvl=0.67,interval='prediction'))['conf.low'])
    dat_unc$se_up <-  
             as.numeric(as.data.frame(ggpredict(fitmod,terms='measured[dat_unc$sfd]',
                                     ci.lvl=0.67,interval='prediction'))['conf.high'])
    
    return(dat_unc)
           }


dat_unc <- ESP_VAL_SOR_sw %>% 
  get_sapf_data() %>% 
  tidyr::pivot_longer(-TIMESTAMP,names_to='tree',
                      values_to='sfd') %>% 
  # join with necessary plant metadata
  left_join(dplyr::select(get_plant_md( ESP_VAL_SOR_sw),pl_code,pl_species,pl_dbh,
                          pl_sens_meth,pl_sens_calib),by=c('tree'='pl_code')) 

sf_add_uncert_log(ESP_VAL_SOR_sw)




fitmod <- map(logmodels[logmodels$method=='HD',"fit"],1)$fit
as.numeric(as.data.frame(ggpredict(fitmod,
                                   terms='measured [12]',ci.lvl=0.67,interval='prediction'))['conf.low'])


fii <- c(1:10)

sapply(fii,
       function(x){ as.numeric(
         as.data.frame(ggpredict(fitmod,terms='measured [x]',
                  ci.lvl=0.67,interval='prediction'))['conf.low'])
       }
)


merTools::predictInterval(merMod = fitmod, 
                          newdata = data.frame(measured=dat_unc$sfd),
                          which='fixed',
                level = 0.67, n.sims = 100,
                stat = "median", type="linear.prediction",
                include.resid.var = TRUE)


##Functions for bootMer() and objects
####Return predicted values from bootstrap
mySumm <- function(.) {
  predict(., newdata=data.frame(measured=dat_unc$sfd), re.form=NA)
}
####Collapse bootstrap into median, 95% PI
sumBoot <- function(merBoot) {
  return(
    data.frame(fit = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.5, na.rm=TRUE))),
               lwr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.167, na.rm=TRUE))),
               upr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.833, na.rm=TRUE)))
    )
  )
}

##lme4::bootMer() method 1
 
  boot1 <- lme4::bootMer(fitmod, mySumm, nsim=100, 
                         use.u=FALSE, type="parametric")

PI.boot1 <- sumBoot(boot1)



foo <- sf_add_uncert_log(ESP_VAL_SOR_sw)

View(foo)

# Examples for HD, CHP, HR - subdaily -------------------------------------


# data ------------------------------------------------------

ESP_VAL_SOR_sw<- read_sfn_data('ESP_VAL_SOR',
                               folder=file.path(sfn_folder,'RData','sapwood'))
AUS_KAR_sw<- read_sfn_data('AUS_KAR',
                           folder=file.path(sfn_folder,'RData','sapwood'))
GBR_DEV_CON_sw<- read_sfn_data('GBR_DEV_CON',
                               folder=file.path(sfn_folder,'RData','sapwood'))

esp_val_sor_unc<- sf_add_uncert(ESP_VAL_SOR_sw,table_coefs=coefficients,nsd=1) 
aus_kar_unc<- sf_add_uncert(AUS_KAR_sw,coefficients) 
gbr_dev_con_unc<- sf_add_uncert(GBR_DEV_CON_sw,coefficients) 

# Subdaily

esp_val_sor <- esp_val_sor_unc %>% 
  filter(lubridate::year(TIMESTAMP)==2004) %>% 
  filter(lubridate::yday(TIMESTAMP)%in%c(150:160)) %>% 
  filter(tree=='ESP_VAL_SOR_Psy_Jt_12') %>% 
  ggplot(aes(x=TIMESTAMP,y=sfdcor))+
  geom_line(alpha=1)+
  geom_ribbon(aes(ymin=sfdcor_lo,ymax=sfdcor_up),alpha=0.3)+
  theme_cowplot()+
  theme(legend.position = 'top')+
  labs(x=NULL,
       y=expression(paste('Sap flow density, ',cm^3,cm^-2,h^-1)),
       title='a) Heat dissipation')

gbr_dev_plot <- gbr_dev_con_unc%>% 
  filter(tree%in%'GBR_DEV_CON_Psy_Jt_2') %>% 
  filter(lubridate::yday(TIMESTAMP)%in%c(150:160)) %>% 
  ggplot(aes(x=TIMESTAMP,y=sfd))+
  geom_line(alpha=1)+
  geom_ribbon(aes(ymin=sfd_lo,ymax=sfd_up),alpha=0.3)+
  theme_cowplot()+
  theme(legend.position = 'top')+
  labs(x=NULL,
       y=expression(paste('Sap flow density, ',cm^3,cm^-2,h^-1)),
       title='b) Compensation heat pulse')


aus_kar_plot<- aus_kar_unc%>% 
  filter(tree%in%'AUS_KAR_Evi_Js_1') %>% 
  filter(lubridate::yday(TIMESTAMP)%in%c(160:170)) %>% 
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
  'docs/Fig_uncert_methods_subdaily.pdf', uncert_hd_chp_hr, nrow = 1, 
  base_height = 21, base_width = 20, units = 'cm'
)

# Example application -----------------------------------------------------


# daily

method_uncert_timseries<- esp_val_sor_unc%>% 
  filter(lubridate::year(TIMESTAMP)==2004) %>% 
  filter(lubridate::yday(TIMESTAMP)%in%c(120:270)) %>% 
  group_by(tree,day=lubridate::floor_date(TIMESTAMP,'day')) %>%
   summarise(
  across(contains('sfd'),mean,na.rm=TRUE)) %>%
  filter(tree%in%'ESP_VAL_SOR_Psy_Jt_12') %>% 
  ggplot(aes(x=day,y=sfd))+
  geom_ribbon(aes(ymin=sfd_lo,ymax=sfd_up),fill='black',  colour=NA,alpha=0.4)+
  geom_line(aes(x=day,y=sfd,col='black'))+
  geom_line(aes(x=day,y=sfdcor,col='blue'))+
  geom_ribbon(aes(ymin=sfdcor_lo,ymax=sfdcor_up),fill='blue', colour=NA,alpha=0.4)+
  scale_colour_manual(name=NULL,values=c('black','blue'),
                      labels=c('HD uncorrected','HD corrected'))+
  theme_cowplot()+
  theme(legend.position = 'top')+
  xlab(NULL)+
  ylab(expression(paste('Sap flow density, ',cm^3,cm^-2,h^-1)))


cowplot::save_plot(
  'docs/Fig_method_uncert_timeseries.pdf', method_uncert_timseries, nrow = 1, 
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

# Plot
Sys.setlocale("LC_TIME", "en_US.UTF-8")
esp_val_sor_unc_sf_plot<- esp_val_sor_unc_sf %>% 
  group_by(tree,day=lubridate::floor_date(TIMESTAMP,'day')) %>%
  filter(lubridate::year(TIMESTAMP)==2004) %>% 
  filter(lubridate::yday(TIMESTAMP)%in%c(120:270)) %>% 
  summarise(
    across(contains('sf'),mean,na.rm=TRUE)
  ) %>% 
  filter(tree=='ESP_VAL_SOR_Psy_Jt_12') %>% 
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

# Plot
Sys.setlocale("LC_TIME", "en_US.UTF-8")
esp_val_sor_totunc_sf_plot<- esp_val_sor_unc_sf %>% 
  group_by(tree,day=lubridate::floor_date(TIMESTAMP,'day')) %>%
  filter(lubridate::year(TIMESTAMP)==2004) %>% 
  filter(lubridate::yday(TIMESTAMP)%in%c(120:270)) %>% 
  summarise(
    across(contains('sf'),mean,na.rm=TRUE)
  ) %>% 
  filter(tree=='ESP_VAL_SOR_Psy_Jt_12') %>% 
  ggplot(aes(x=day,y=sfcor_mean*24/1000))+
  geom_line(aes(col='black'))+
  geom_ribbon(aes(ymin=sfcor_totunc2_lo*24/1000,
                  ymax=sfcor_totunc2_up*24/1000,
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
  'docs/Fig_uncert_hd.pdf', esp_val_sor_plot, nrow = 1, 
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
  group_by(pl_code,day=lubridate::floor_date(TIMESTAMP,'day')) %>%
  # Aggregate at daily level
  summarise(
    across(contains('sfd'),mean,na.rm=TRUE)) %>%
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
    sfd_norad=sfd*pl_sapw_area*24/1000,
    # Apply radially integration according to wood type
    # Result of qtot is a matrix with n bootstrap estimates
    # 'apply' to get mean and quantiles of boot samples
    # units converted
    sfd_boot=sapflux::qtot(sfd*100/36,a=0,b=pl_sens_length/1000, woodType=wood_type,
                  uncertainty=TRUE,nboot=100,treeRadius=0.5*pl_dbh/100,
                  sapRadius=pl_sapw_depth/100),
    sfdrad_mean=apply(sfd_boot,1,mean,na.rm=TRUE)*3600*24/1000,
    sfdrad_low=apply(sfd_boot,1,quantile,prob=0.025,na.rm=TRUE)*3600*24/1000,
    sfdrad_up=apply(sfd_boot,1,quantile,prob=0.975,na.rm=TRUE)*3600*24/1000,
  ) %>% 
  select(-sfd_boot)


# Create plot

f_labels <- data.frame(pl_code = c('USA_UMB_CON_Pst_Js_2',
                               'USA_UMB_CON_Qru_Js_10',
                               'USA_UMB_CON_Pgr_Js_3'), 
                       label = c("(b)", "(a)", "(c)"))


umb_rad_uncert_plot<- umb_rad_uncert %>% 
  arrange(day,pl_code) %>% 
  filter(lubridate::yday(day)%in%c(150:270)) %>% 
  filter(pl_code%in%c('USA_UMB_CON_Pst_Js_2',
                       'USA_UMB_CON_Qru_Js_10',
                       'USA_UMB_CON_Pgr_Js_3')) %>% 
  ggplot(aes(x=day,y=sfd_norad,col='black'))+
  geom_line()+
  geom_line(aes(x=day,y=sfdrad_mean,col='blue'))+
  geom_ribbon(aes(ymin=sfdrad_low,ymax=sfdrad_up),
              colour=NA,fill='blue',alpha=0.2)+
  geom_label(aes(label=paste(pl_species,wood_type,sep=',')), 
             x = Inf, y = Inf, hjust=1, vjust=1,show.legend = FALSE)+
  geom_text(x=-Inf, y = Inf, aes(label = label), data = f_labels,
            hjust=-0.3,vjust=1,
           size=8, show.legend=FALSE)+
  scale_colour_manual(name=NULL,values=c('black','blue'),
                      labels=c('SFD','SFD radially integrated'))+
  theme_cowplot()+
  labs(x=NULL,
       y= expression(paste('Daily sap flow, kg·', day^-1)))+
  facet_grid(rows=vars(pl_code),scales='free_y',as.table=FALSE)+
  theme(strip.text.y = element_text(size=10),legend.position='top')


cowplot::save_plot(
  'docs/Fig_rad_uncert.pdf', umb_rad_uncert_plot, nrow = 1, 
  base_height = 21, base_width = 20, units = 'cm'
)



