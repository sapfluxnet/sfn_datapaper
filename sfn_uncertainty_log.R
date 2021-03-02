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

