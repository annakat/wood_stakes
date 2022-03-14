### SURFACE SEM WOODSTAKES
### Feb 2022 - AS #######
library(tidyverse) 
library(piecewiseSEM)
library(nlme) 

dat_save <- read.csv("./Risch_etal_Frontiers.csv")

#### Surface data
dat <- dat_save %>% filter(Inc.dept==1)%>%
  mutate(surf_CN=surf_C.master/surf_N.master)%>%
  drop_na(Temp, Precip,Decomp,surf_C.master, surf_N.master,surf_CN)%>%
        mutate_if(is.factor, as.character)%>%
        mutate(Decomp_10_log = log(Decomp+10))%>%
        mutate(AET_log = log(AET))%>%
        mutate(surf_CN_10 = surf_CN/10)%>%
        mutate(Precip_log = log(Precip))%>%
        mutate(Temp_log = log(Temp))%>%
        mutate(surf_C_10 = surf_C.master/10)%>%
        mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State", "Incub.time"), 
                    list(f=as.factor))

### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")

##################
### Aspen MAIN ###
modlist <- psem(
  lme(surf_CN_10 ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
    data=dat_aspen),
  lme(Decomp_10_log ~ Temp_log + Precip_log + surf_CN_10, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen))

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)

### Aspen SI ####
dat <- dat_save %>% filter(Inc.dept==1)%>%
  drop_na(Temp, Precip,Decomp,surf_C.master)%>%
  mutate_if(is.factor, as.character)%>%
  mutate(Decomp_10_log = log(Decomp+10))%>%
  mutate(Precip_log = log(Precip))%>%
  mutate(Temp_log = log(Temp))%>%
  mutate(surf_C_10 = surf_C.master/10)%>%
  mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State", "Incub.time"), 
            list(f=as.factor))

### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")

modlist <- psem(
  lme(surf_C_10 ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen),
  lme(Decomp_10_log ~ Temp_log + Precip_log + surf_C_10, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen))

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)

#################
### Pine MAIN ###

modlist <- psem(
  lme(surf_CN_10 ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  lme(Decomp_10_log ~ Temp_log + Precip_log + surf_CN_10, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine))

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


### Pine SI ####
modlist <- psem(
  lme(surf_C_10 ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  lme(Decomp_10_log ~ Temp_log + Precip_log + surf_C_10, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine))

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


##### SI ###########
#### AET models ####

dat <- dat_save %>% filter(Inc.dept==1)%>%
  drop_na(Temp, Precip,Decomp,AET, surf_N.master, surf_C.master)%>%
  mutate_if(is.factor, as.character)%>%
  mutate(surf_CN=surf_C.master/surf_N.master)%>%
  mutate(Decomp_26_log = log(Decomp+26))%>%
  mutate(AET_log = log(AET))%>%
  mutate(surf_CN_10 = surf_CN/10)%>%
  mutate(Precip_log = log(Precip))%>%
  mutate(Temp_log = log(Temp))%>%
  mutate(surf_C_10 = surf_C.master/10)%>%
  mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State", "Incub.time"), 
            list(f=as.factor))

### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")

### Aspen ######
modlist <- psem(
  lme(surf_CN_10 ~ AET_log, random=~1|subplot.nr_f/Site.code_f, data=dat_aspen),
  lme(Decomp_26_log ~ AET_log + surf_CN_10, random=~1|subplot.nr_f/Site.code_f, data=dat_aspen))

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


### Pine #######
modlist <- psem(
  lme(surf_CN_10 ~ AET_log, random=~1|subplot.nr_f/Site.code_f, data=dat_pine),
  lme(Decomp_26_log ~ AET_log + surf_CN_10, random=~1|subplot.nr_f/Site.code_f, data=dat_pine))

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


########### END ########
