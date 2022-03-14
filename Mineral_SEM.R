### MINERAL SEM WOODSTAKES
### Feb 2022 - AS #######
library(tidyverse) 
library(piecewiseSEM)
library(nlme) 

dat_save <- read.csv("./Risch_etal_Frontiers.csv")

dat <- dat_save %>% filter(Location=="mineral")%>%
  mutate(surf_CN_10=surf_C.master/surf_N.master/10)%>%
  mutate(min_C_log=log(min_C))%>%
  select(Stake.ID,Site.code, State, Wood.type, subplot.nr, Temp, 
         Precip_log, min_C_log, surf_CN_10, min_pH, Decomp)%>%
  drop_na(Temp, Precip_log,surf_CN_10, min_pH, min_C_log, Decomp)%>%
  group_by(Stake.ID, Site.code, State, Wood.type, subplot.nr) %>%
  summarise(across(c("Temp":"Decomp"), ~ mean(.x, na.rm = TRUE))) %>% # average mineral decomp
  mutate(Decomp_10_log = log(Decomp+10), Temp_log = log(Temp)) %>%
  mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State"), list(f=as.factor))

### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")

####################
### Aspen MAIN #####

modlist <- psem(
  lme(surf_CN_10 ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen),
  lme(min_C_log ~ surf_CN_10 + Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen),
  lme(min_pH ~ surf_CN_10+ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen), 
  lme(Decomp_10_log ~ Temp_log+ Precip_log+ min_C_log+ min_pH+ surf_CN_10,
      random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen),
  min_C_log%~~%min_pH)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


##################
### Pine MAIN #####
modlist <- psem(
  lme(surf_CN_10 ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  lme(min_C_log ~ surf_CN_10 + Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  lme(min_pH ~ surf_CN_10+ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine), 
  lme(Decomp_10_log ~ Temp_log+ Precip_log+ min_C_log+ min_pH+ surf_CN_10,
      random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  min_C_log%~~%min_pH)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


##### SI ##########
### C instead of CN

dat <- dat_save %>% filter(Location=="mineral")%>%
  mutate(min_C_log=log(min_C), surf_C_10 = surf_C.master/10)%>%
  select(Stake.ID,Site.code, State, Wood.type, subplot.nr, Temp, 
         Precip_log, min_C_log, surf_C_10, min_pH, Decomp)%>%
  drop_na(Temp, Precip_log,surf_C_10, min_pH, min_C_log, Decomp)%>%
  group_by(Stake.ID, Site.code, State, Wood.type, subplot.nr) %>%
  summarise(across(c("Temp":"Decomp"), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(Decomp_10_log = log(Decomp+10), Temp_log = log(Temp)) %>%
  mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State"), list(f=as.factor))


### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")

### Aspen #####
modlist <- psem(
  lme(surf_C_10 ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen),
  lme(min_C_log ~ surf_C_10 + Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen),
  lme(min_pH ~ surf_C_10+ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen), 
  lme(Decomp_10_log ~ Temp_log+ Precip_log+ min_C_log+ min_pH+ surf_C_10,
      random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen),
  min_C_log%~~%min_pH)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


### Pine #####
modlist <- psem(
  lme(surf_C_10 ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  lme(min_C_log ~ surf_C_10 + Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  lme(min_pH ~ surf_C_10+ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine), 
  lme(Decomp_10_log ~ Temp_log+ Precip_log+ min_C_log+ min_pH+ surf_C_10,
      random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  min_C_log%~~%min_pH)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


####################
#### with BD #######

dat <- dat_save %>% filter(Location=="mineral")%>%
  mutate(surf_CN_10=surf_C.master/surf_N.master/10)%>%
  mutate(min_C_log=log(min_C))%>%
  select(Stake.ID,Site.code, State, Wood.type, subplot.nr, Temp, AET, 
         Precip_log, min_C_log, surf_CN_10, min_pH, BD, Decomp)%>%
  drop_na(Temp, Precip_log,surf_CN_10, min_pH, min_C_log, Decomp, BD)%>%
  group_by(Stake.ID, Site.code, State, Wood.type, subplot.nr) %>%
  summarise(across(c("Temp":"Decomp"), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(Decomp_10_log = log(Decomp+10), Temp_log = log(Temp)) %>%
  mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State"), list(f=as.factor))

### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")

#### Aspen ###
modlist <- psem(
  lme(surf_CN_10 ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen),
  lme(min_C_log ~ surf_CN_10 + Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen),
  lme(min_pH ~ surf_CN_10+ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen), 
  lme(BD ~ surf_CN_10+ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen), 
  lme(Decomp_10_log ~ Temp_log+ Precip_log+ min_C_log+ min_pH+ BD+surf_CN_10,
      random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen),
  min_C_log%~~%min_pH, BD%~~%min_pH, BD%~~%min_C_log)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


#### Pine ###
modlist <- psem(
  lme(surf_CN_10 ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  lme(min_C_log ~ surf_CN_10 + Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  lme(min_pH ~ surf_CN_10+ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine), 
  lme(BD ~ surf_CN_10+ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine), 
  lme(Decomp_10_log ~ Temp_log+ Precip_log+ min_C_log+ min_pH+ BD+surf_CN_10,
      random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  min_C_log%~~%min_pH, BD%~~%min_pH, BD%~~%min_C_log)


summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)

######################
#### mit AET, ohne BD
dat <- dat_save %>% filter(Location=="mineral")%>%
  mutate(surf_CN_10=surf_C.master/surf_N.master/10)%>%
  mutate(min_C_log=log(min_C))%>%
  select(Stake.ID,Site.code, State, Wood.type, subplot.nr, AET, 
         min_C_log, surf_CN_10, min_pH, Decomp)%>%
  drop_na(AET, surf_CN_10, min_pH, min_C_log, Decomp)%>%
  group_by(Stake.ID, Site.code, State, Wood.type, subplot.nr) %>%
  summarise(across(c("AET":"Decomp"), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(AET_log= log(AET)) %>%
  mutate(Decomp_10_log = log(Decomp+10)) %>%
  mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State"), list(f=as.factor))

ggdensity(dat$AET_log)

### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")

### Aspen ###
modlist <- psem(
  lme(surf_CN_10 ~ AET_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen),
  lme(min_C_log ~ surf_CN_10 + AET_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen),
  lme(min_pH ~ surf_CN_10+ AET_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen), 
  lme(Decomp_10_log ~ AET_log + min_C_log+ min_pH+ surf_CN_10,
      random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen),
  min_C_log%~~%min_pH)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


#### Pine ####
modlist <- psem(
  lme(surf_CN_10 ~ AET_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  lme(min_C_log ~ surf_CN_10 + AET_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  lme(min_pH ~ surf_CN_10+ AET_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine), 
  lme(Decomp_10_log ~ AET_log + min_C_log+ min_pH+ surf_CN_10,
      random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  min_C_log%~~%min_pH)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)

###################
### no surf CN ####

dat <- dat_save %>% filter(Location=="mineral")%>%
  mutate(min_C_log=log(min_C))%>%
  select(Stake.ID,Site.code, State, Wood.type, subplot.nr, Temp,
         Precip_log, min_C_log, min_pH, Decomp)%>%
  drop_na(Temp, Precip_log, min_pH, min_C_log, Decomp)%>%
  group_by(Stake.ID, Site.code, State, Wood.type, subplot.nr) %>%
  summarise(across(c("Temp":"Decomp"), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(Decomp_10_log = log(Decomp+10), Temp_log = log(Temp+1)) %>%
  mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State"), list(f=as.factor))

### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")

#### Aspen
modlist <- psem(
  lme(min_C_log ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen),
  lme(min_pH ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen), 
  lme(Decomp_10_log ~ Temp_log+ Precip_log+ min_C_log+ min_pH,
      random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen),
  min_C_log%~~%min_pH)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


##### Pine ###
modlist <- psem(
  lme(min_C_log ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  lme(min_pH ~ Temp_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine), 
  lme(Decomp_10_log ~ Temp_log+ Precip_log+ min_C_log+ min_pH,
      random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  min_C_log%~~%min_pH)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


#####################
### ohne BD + MATs ##

dat <- dat_save %>% filter(Location=="mineral")%>%
  mutate(surf_CN_10=surf_C.master/surf_N.master/10)%>%
  mutate(min_C_log=log(min_C))%>%
  select(Stake.ID,Site.code, State, Wood.type, subplot.nr, AET, 
         Precip_log, min_C_log, surf_CN_10, min_pH, MATs05,MATs515, Decomp)%>%
  drop_na(AET,Precip_log,surf_CN_10, min_pH, min_C_log, MATs05,MATs515, Decomp)%>%
  group_by(Stake.ID, Site.code, State, Wood.type, subplot.nr) %>%
  summarise(across(c("AET":"Decomp"), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(MATs_mean= mean(MATs05+MATs515)) %>%
  mutate(AET_log= log(AET), MATsmean_log= log(MATs_mean)) %>%
  mutate(Decomp_10_log = log(Decomp+10)) %>%
  mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State"), list(f=as.factor))


### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")

### Aspen ###
modlist <- psem(
  lme(surf_CN_10 ~ MATsmean_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen),
  lme(min_C_log ~ surf_CN_10 + MATsmean_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen),
  lme(min_pH ~ surf_CN_10+ MATsmean_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen), 
  lme(Decomp_10_log ~ MATsmean_log+ Precip_log+ min_C_log+ min_pH+ surf_CN_10,
      random=~1|subplot.nr_f/Site.code_f,
      data=dat_aspen),
  min_C_log%~~%min_pH)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)

### Pine #####
modlist <- psem(
  lme(surf_CN_10 ~ MATsmean_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  lme(min_C_log ~ surf_CN_10 + MATsmean_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  lme(min_pH ~ surf_CN_10+ MATsmean_log + Precip_log, random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine), 
  lme(Decomp_10_log ~ MATsmean_log+ Precip_log+ min_C_log+ min_pH+ surf_CN_10,
      random=~1|subplot.nr_f/Site.code_f,
      data=dat_pine),
  min_C_log%~~%min_pH)

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)

### END ########################

