### INTERFACE SEM WOODSTAKES
### Feb 2022 - AS #######
library(tidyverse) 
library(piecewiseSEM)
library(nlme) 

dat_save <- read.csv("./Risch_etal_Frontiers.csv")

#### interface data
dat <- dat_save %>% filter(Inc.dept==2)%>%
        drop_na(Temp, Precip_log,surf_N.master, surf_C.master,min_pH, min_C, Decomp)%>%
        mutate(Decomp_10_log = log(Decomp+10), Temp_log = log(Temp),
               min_C_log = log(min_C), surf_CN_10=(surf_C.master/surf_N.master)/10,
               AET_log=log(AET))%>%
        mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State"), 
                    list(f=as.factor))

### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")

###################
### Aspen MAIN ####
modlist <- psem(
  lme(surf_CN_10 ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_aspen), 
  lme(min_C_log ~ surf_CN_10 +Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_aspen),
  lme(min_pH ~ surf_CN_10+ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_aspen), 
  lme(Decomp_10_log ~ Temp_log+ Precip_log+ min_C_log+ min_pH+ surf_CN_10, 
      random=~1|subplot.nr_f/Site.code,
      data=dat_aspen),
  min_C_log%~~%min_pH)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


#################
### Pine MAIN ##

modlist <- psem(
  lme(surf_CN_10 ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_pine), 
  lme(min_C_log ~ surf_CN_10 +Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_pine),
  lme(min_pH ~ surf_CN_10+ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_pine), 
  lme(Decomp_10_log ~ Temp_log+ Precip_log+ min_C_log+ min_pH+ surf_CN_10, 
      random=~1|subplot.nr_f/Site.code,
      data=dat_pine),
  min_C_log%~~%min_pH)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)



###########################
##### SI C instead of CN
dat <- dat_save %>% filter(Inc.dept==2)%>%
  drop_na(Temp, Precip_log,surf_C.master,min_pH, min_C, Decomp)%>%
  mutate(Decomp_10_log = log(Decomp+10), Temp_log = log(Temp),
         min_C_log = log(min_C), surf_C_10 = surf_C.master/10)%>%
  mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State"), 
            list(f=as.factor))

### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")

### Aspen ####
modlist <- psem(
  lme(surf_C_10 ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_aspen), 
  lme(min_C_log ~ surf_C_10 +Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_aspen),
  lme(min_pH ~ surf_C_10+ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_aspen), 
  lme(Decomp_10_log ~ Temp_log+ Precip_log+ min_C_log+ min_pH+ surf_C_10, 
      random=~1|subplot.nr_f/Site.code,
      data=dat_aspen),
  min_C_log%~~%min_pH)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)

#### Pine ######
modlist <- psem(
  lme(surf_C_10 ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_pine), 
  lme(min_C_log ~ surf_C_10 +Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_pine),
  lme(min_pH ~ surf_C_10+ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_pine), 
  lme(Decomp_10_log ~ Temp_log+ Precip_log+ min_C_log+ min_pH+ surf_C_10, 
      random=~1|subplot.nr_f/Site.code,
      data=dat_pine),
  min_C_log%~~%min_pH)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


#####################
#### SI with BD #####

dat <- dat_save %>% filter(Inc.dept==2)%>%
  drop_na(Temp, Precip_log,surf_N.master, surf_C.master,min_pH,BD, min_C, Decomp)%>%
  mutate(Decomp_10_log = log(Decomp+10), Temp_log = log(Temp),
         min_C_log = log(min_C), surf_CN_10=(surf_C.master/surf_N.master)/10,
         AET_log=log(AET))%>%
  mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State"), 
            list(f=as.factor))

### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")

### Aspen ###
modlist <- psem(
  lme(surf_CN_10 ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_aspen), 
  lme(min_C_log ~ surf_CN_10 +Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_aspen),
  lme(min_pH ~ surf_CN_10+ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_aspen), 
  lme(BD ~ surf_CN_10+ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_aspen), 
  lme(Decomp_10_log ~ Temp_log+ Precip_log+ min_C_log+ min_pH+ BD+ surf_CN_10, 
      random=~1|subplot.nr_f/Site.code,
      data=dat_aspen),
  min_C_log%~~%min_pH, min_C_log%~~%BD, min_pH%~~%BD)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


#### Pine ###
modlist <- psem(
  lme(surf_CN_10 ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_pine), 
  lme(min_C_log ~ surf_CN_10 +Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_pine),
  lme(min_pH ~ surf_CN_10+ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_pine), 
  lme(BD ~ surf_CN_10+ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code,
      data=dat_pine), 
  lme(Decomp_10_log ~ Temp_log+ Precip_log+ min_C_log+ min_pH+ BD+ surf_CN_10, 
      random=~1|subplot.nr_f/Site.code,
      data=dat_pine),
  min_C_log%~~%min_pH, min_C_log%~~%BD, min_pH%~~%BD)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


#####################
### SI ohne BD + AET
dat <- dat_save %>% filter(Inc.dept==2)%>%
  drop_na(surf_N.master, surf_C.master,min_pH,AET, min_C, Decomp)%>%
  mutate(Decomp_10_log = log(Decomp+10), Temp_log = log(Temp),
         min_C_log = log(min_C), surf_CN_10=(surf_C.master/surf_N.master)/10,
         AET_log=log(AET))%>%
  mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State"), 
            list(f=as.factor))

### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")

### Aspen ####
modlist <- psem(
  lme(surf_CN_10 ~ AET_log, random=~1|subplot.nr_f/Site.code,
      data=dat_aspen), 
  lme(min_C_log ~ surf_CN_10 +AET_log, random=~1|subplot.nr_f/Site.code,
      data=dat_aspen),
  lme(min_pH ~ surf_CN_10+ AET_log, random=~1|subplot.nr_f/Site.code,
      data=dat_aspen), 
  lme(Decomp_10_log ~ AET_log+ min_C_log+ min_pH+ surf_CN_10, 
      random=~1|subplot.nr_f/Site.code,
      data=dat_aspen),
  min_C_log%~~%min_pH)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)

### Pine ####
modlist <- psem(
  lme(surf_CN_10 ~ AET_log, random=~1|subplot.nr_f/Site.code,
      data=dat_pine), 
  lme(min_C_log ~ surf_CN_10 +AET_log, random=~1|subplot.nr_f/Site.code,
      data=dat_pine),
  lme(min_pH ~ surf_CN_10+ AET_log, random=~1|subplot.nr_f/Site.code,
      data=dat_pine), 
  lme(Decomp_10_log ~ AET_log+ min_C_log+ min_pH+ surf_CN_10, 
      random=~1|subplot.nr_f/Site.code,
      data=dat_pine),
  min_C_log%~~%min_pH)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)

############ END
