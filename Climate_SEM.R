### SEM WOODSTAKES - Precip and Temp / AET only
### Feb 2022 - AS ##############################
library(tidyverse) 
library(piecewiseSEM)
library(nlme) 

dat_save <- read.csv("./Risch_etal_Frontiers.csv")

#### Surface data
dat <- dat_save %>% filter(Inc.dept==1)%>%
        mutate_if(is.factor, as.character)%>%
        drop_na(Temp, Precip,Decomp)%>%
        mutate(Decomp_20_log = log(Decomp+20))%>%
        mutate(Temp_log = log(Temp))%>%
        mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State"), 
                    list(f=as.factor))

### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")

#########################
##### Temp and Precip only 
### Aspen ####
modlist <- psem(
  lme(Decomp_20_log ~ Temp_log + Precip_log,random=~1|subplot.nr_f/Site.code,
      data=dat_aspen)) 

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)

#### Pine ####
modlist <- psem(
  lme(Decomp_20_log ~ Temp_log + Precip_log,random=~1|subplot.nr_f/Site.code,
      data=dat_pine)) 

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


################
##### AET ######
dat <- dat_save %>% filter(Inc.dept==1)%>%
  mutate_if(is.factor, as.character)%>%
  drop_na(AET, Decomp)%>%
  mutate(Decomp_20_log = log(Decomp+20))%>%
  mutate(AET_log = log(AET))%>%
  mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State"), 
            list(f=as.factor))

### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")


##### AET only #####
### Aspen ####
modlist <- psem(
  lme(Decomp_20_log ~ AET_log,random=~1|subplot.nr_f/Site.code,
      data=dat_aspen)) 

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


#### Pine ###
modlist <- psem(
  lme(Decomp_20_log ~ AET_log,random=~1|subplot.nr_f/Site.code,
      data=dat_pine)) 

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


###########################
### Interface data ########

dat <- dat_save %>% filter(Inc.dept==2)%>%
  drop_na(Temp, Precip_log,Decomp)%>%
  mutate(Decomp_10_log = log(Decomp+10), Temp_log = log(Temp)) %>%
  mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State"), 
            list(f=as.factor))

### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")

##### Temp and Precip only ###
### Aspen ###
modlist <- psem(
  lme(Decomp_10_log ~ Temp_log + Precip_log,random=~1|subplot.nr_f/Site.code,
      data=dat_aspen)) 

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


#### Pine ####
modlist <- psem(
  lme(Decomp_10_log ~ Temp_log + Precip_log,random=~1|subplot.nr_f/Site.code,
      data=dat_pine)) 

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


##### AET ###
dat <- dat_save %>% filter(Inc.dept==2)%>%
  mutate_if(is.factor, as.character)%>%
  drop_na(AET, Decomp)%>%
  mutate(Decomp_10_log = log(Decomp+10))%>%
  mutate(AET_log = log(AET))%>%
  mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State"), 
            list(f=as.factor))


### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")

#########################
##### AET only 
### Aspen ###
modlist <- psem(
  lme(Decomp_10_log ~ AET_log,random=~1|subplot.nr_f/Site.code,
      data=dat_aspen)) 

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


#### Pine ###
modlist <- psem(
  lme(Decomp_10_log ~ AET_log,random=~1|subplot.nr_f/Site.code,
      data=dat_pine)) 

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


#####################
##### Mineral soil ##

dat <- dat_save %>% filter(Location=="mineral")%>%
  select(Stake.ID, Site.code, State, Wood.type, subplot.nr, Temp, Precip_log, Decomp)%>%
  drop_na(Temp, Precip_log, Decomp)%>%
  group_by(Stake.ID, Site.code, State, Wood.type, subplot.nr) %>%
  summarise(across(c("Temp":"Decomp"), ~ mean(.x, na.rm = TRUE)))%>%
  mutate(Decomp_10_log = log(Decomp+10), Temp_1log = log(Temp+1)) %>%
  mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State"), list(f=as.factor))


### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")

##### Temp and Precip only 
### Aspen ###
modlist <- psem(
  lme(Decomp_10_log ~ Temp_1log + Precip_log,random=~1|subplot.nr_f/Site.code,
      data=dat_aspen)) 

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)

#### Pine ###
modlist <- psem(
  lme(Decomp_10_log ~ Temp_1log + Precip_log,random=~1|subplot.nr_f/Site.code,
      data=dat_pine)) 

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)


##### AET only ###
dat <- dat_save %>% filter(Location=="mineral")%>%
  select(Stake.ID,Site.code, State, Wood.type, subplot.nr, AET, Decomp)%>%
  drop_na(AET, Decomp)%>%
  group_by(Stake.ID, Site.code, State, Wood.type, subplot.nr) %>%
  summarise(across(c("AET":"Decomp"), ~ mean(.x, na.rm = TRUE)))%>%
  mutate(Decomp_10_log = log(Decomp+10), AET_log = log(AET)) %>%
  mutate_at(c("Wood.type", "subplot.nr", "Site.code", "State"), list(f=as.factor))

### split wood types
dat_aspen <- dat %>% filter(Wood.type=="aspen")
dat_pine <- dat %>% filter(Wood.type=="pine")

##### Temp and Precip only 
### Aspen ###
modlist <- psem(
  lme(Decomp_10_log ~ AET_log,random=~1|subplot.nr_f/Site.code,
      data=dat_aspen)) 

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)

#### Pine
modlist <- psem(
  lme(Decomp_10_log ~ AET_log,random=~1|subplot.nr_f/Site.code,
      data=dat_pine)) 

summary(modlist)
(coefs <- summary(modlist)$coefficients)
(rs <- summary(modlist)$R2)

####### END ########



