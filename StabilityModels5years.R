#################################################################################
### Models for H1 and H2 - Trait diversity and 5 year stability relationships ###
#################################################################################

### Hannah White 13.01.2022
### Edited 24.04.2022 to change to 5+ year time series
### Edited 28.04.2022 to add models of effect diversity
### Edited 19.06.2023 add species richness SEM and correct effect trait diversity

library(ggplot2)
library(nlme)
library(spdep)
library(piecewiseSEM)

## Read in data
setwd('D:\\ResponseEffectDiversity\\RomaniaData')

load('Bird_Temp_CV.RData')
load('communityCoV.RData')
load('FDRomaniaSiteLevelRevisedNumeric.RData')

sites <- read.csv('longform_Site_data.csv', header = TRUE)
coords <- sites[,1:3]
rm(sites)

names(coords) <- c('Site', 'xcoord', 'ycoord')
coords <- unique(coords)

## Merge data

# Cut data down to sites with >= 5 year data

bird.long <- Bird_Temp_CV[Bird_Temp_CV$N_years >= 5,]

bird.long <- merge(bird.long, cv.comm, by = 'Site', all.x = TRUE, all.y = FALSE)
bird.long <- merge(bird.long, FDsites.df, by = 'Site', all.x = TRUE, all.y = FALSE)

bird.long <- merge(coords, bird.long, by = 'Site', all.x = FALSE, all.y = TRUE)


### Generalised Least Squares

## Asynchrony
# test different correlation structures
gls.basic <- gls(Com.Asynch ~ response.fdis, data = bird.long)
plot(Variogram(gls.basic, form = ~xcoord + ycoord)) # extract distance (10000) and nugget (0.7)

# bubble diagram to visualise potential autocorrelation
dat<-data.frame(xcoord = bird.long$xcoord,ycoord = bird.long$ycoord,resids=resid(gls.basic))
coordinates(dat)<-c('xcoord','ycoord')
bubble(dat,zcol='resids')


gls.asyn1 <- gls(Com.Asynch ~ response.fdis, data = bird.long,
                 corr = corExp(form = ~ xcoord + ycoord, nugget = TRUE)) # AIC = -139.6169

gls.asyn2 <- gls(Com.Asynch ~ response.fdis, data = bird.long,
                 corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE)) # AIC = -140.4784

gls.asyn3 <- gls(Com.Asynch ~ response.fdis, data = bird.long,
                 corr = corLin(form = ~xcoord + ycoord, nugget = TRUE)) ## false convergence

gls.asyn4 <- gls(Com.Asynch ~ response.fdis, data = bird.long, # AIC = -139.7786
                 corr = corRatio(c(2000, 0.7), form = ~xcoord + ycoord, nugget = TRUE))  #include distance where semivariogram = (1+nugget)/2

gls.asyn5 <- gls(Com.Asynch ~ response.fdis, data = bird.long,
                 corr = corSpher(c(10000, 0.7), form = ~xcoord + ycoord, nugget = TRUE)) # AIC = -140.403


## model with Gaussian correlation structure has lowest AIC but all much of a muchness
## Now need to check adequcy of model

plot(Variogram(gls.asyn2, resType = 'n')) 
plot(gls.asyn2, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.asyn2, type ='n'), breaks = 30) 
qqnorm(gls.asyn2, ~resid(.,type = 'n'))

# bubble diagram to visualise potential autocorrelation
asyn<-data.frame(xcoord = bird.long$xcoord,ycoord = bird.long$ycoord,resids=resid(gls.asyn2))
coordinates(asyn)<-c('xcoord','ycoord')
bubble(asyn,zcol='resids')


summary(gls.asyn2) 

### Community temporal variability
# will stick with Gaussian correlation 

gls.commvar <- gls(Temp_Var ~ response.fdis, data = bird.long,
                   corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))

plot(Variogram(gls.commvar, resType = 'n'))
plot(gls.commvar, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.commvar, type ='n'))
qqnorm(gls.commvar, ~resid(.,type = 'n'))

summary(gls.commvar) 

### Coefficient of variation

gls.coefvar <- gls(CoV ~ response.fdis, data = bird.long,
                   corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))

plot(Variogram(gls.coefvar, resType = 'n'))
plot(gls.coefvar, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.coefvar, type ='n'))
qqnorm(gls.coefvar, ~resid(.,type = 'n')) 

summary(gls.coefvar) 

## Try logging coefficient of variation
gls.coefvar2 <- gls(log(CoV) ~ response.fdis, data = bird.long,
                    corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))

plot(Variogram(gls.coefvar2, resType = 'n'))
plot(gls.coefvar2, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.coefvar2, type ='n'))
qqnorm(gls.coefvar2, ~resid(.,type = 'n')) 

summary(gls.coefvar2)


### Model temporal variability/coefficient of variation as a function of asynchrony

gls.stab <- gls(Temp_Var ~ Com.Asynch, data = bird.long,
                corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))

plot(Variogram(gls.stab, resType = 'n'))
plot(gls.stab, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.stab, type ='n'))
qqnorm(gls.stab, ~resid(.,type = 'n')) 

summary(gls.stab) 

gls.abund <- gls(CoV ~ Com.Asynch, data = bird.long,
                 corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))

plot(Variogram(gls.abund, resType = 'n'))
plot(gls.abund, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.abund, type ='n'))
qqnorm(gls.abund, ~resid(.,type = 'n')) 

summary(gls.abund) 

### Models including species richness as covariate
## 

#Response diversity
gls.srRD <- gls(response.fdis ~ all.SR, data = bird.long,
                corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(Variogram(gls.srRD, resType = 'n'))
plot(gls.srRD, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.srRD, type ='n'))
qqnorm(gls.srRD, ~resid(.,type = 'n')) 

# asynchrony
gls.srasyn <- gls(Com.Asynch ~ response.fdis + all.SR, data = bird.long,
                  corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(Variogram(gls.srasyn, resType = 'n'))
plot(gls.srasyn, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.srasyn, type ='n'))
qqnorm(gls.srasyn, ~resid(.,type = 'n')) 

#CoV (functional variability)
gls.srCoV <- gls(CoV ~ response.fdis + all.SR + Com.Asynch, data = bird.long,
                 corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))

plot(Variogram(gls.srCoV, resType = 'n'))
plot(gls.srCoV, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.srCoV, type ='n'))
qqnorm(gls.srCoV, ~resid(.,type = 'n'))



##################################
### Strucutral Equation Models ###
##################################

### Set up models

sem.rd <- gls.srRD
sem.async <- gls.srasyn
sem.comvar <- gls(Temp_Var ~ response.fdis + Com.Asynch, data = bird.long,
                  corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(sem.comvar, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(sem.comvar, type ='n'))
qqnorm(sem.comvar, ~resid(.,type = 'n')) 

sem.coefvar <- gls.srCoV

sem1 <- psem(sem.rd, sem.async, sem.comvar, sem.coefvar)
summary(sem1)

plot(sem1)


plot(sem1)

############################################
### Adding in models of effect diversity ###
############################################

load('CVeffectdiv.RData')

bird.long <- merge(bird.long, cv.effectdiv, by ='Site', all.x = TRUE, all.y = FALSE)


effect.comm <- gls(log(cv.fdis) ~ Temp_Var, data = bird.long,
                   corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(effect.comm, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(effect.comm, type ='n'))
qqnorm(effect.comm, ~resid(.,type = 'n')) 

summary(effect.comm) 

effect.cov <- gls(log(cv.fdis) ~ CoV, data = bird.long,
                  corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(effect.cov, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(effect.cov, type ='n'))
qqnorm(effect.cov, ~resid(.,type = 'n')) 

summary(effect.cov) 

sem.effect <- gls(log(cv.fdis) ~ Temp_Var + CoV, data = bird.long,
                  corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(sem.effect, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(sem.effect, type ='n'))
qqnorm(sem.effect, ~resid(.,type = 'n')) 

summary(sem.effect)



### Add to the structural equation model

sem2 <- psem(sem.async, sem.comvar, sem.coefvar, sem.effect)
summary(sem2)

## Check tests of directed separation
dSep(sem2)

## Looks like:
## * cv.fdis might also be dependent on response.fdis
## * CoV might be related to Temp_Var
## * Temp_Var might be related to all.SR (but won't include this due to convergence issues)
## Add these to SEM

sem.effect2 <- gls(log(cv.fdis) ~ Temp_Var + CoV + response.fdis, data = bird.long,
                   corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(sem.effect2, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(sem.effect2, type ='n'))
qqnorm(sem.effect2, ~resid(.,type = 'n'))

sem.coefvar2 <- gls(CoV ~ response.fdis + Com.Asynch + Temp_Var + all.SR, data = bird.long,
                    corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(sem.coefvar2, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(sem.coefvar2, type ='n')) # seems like it might be okay unlogged?
qqnorm(sem.coefvar2, ~resid(.,type = 'n'))

sem3 <- psem(sem.rd, sem.async, sem.comvar, sem.coefvar2, sem.effect2)
summary(sem3) # AIC = 78.920
              # Fishers c = 14.92
              # p val for fishers c = 0.021 NOT A VALID MODEL

## Test with additional dSep identified link (temp_var~SR) 

sem.comvar4 <- gls(Temp_Var ~ response.fdis + Com.Asynch + all.SR, data = bird.long,
                   corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))

sem5 <- psem(sem.rd, sem.async, sem.comvar4, sem.coefvar2, sem.effect2)
summary(sem5) # AIC =  69.857
              # Fishers c = 3.857
              # p val for fishers c = 0.426

### SEM without species richness 

sem.asynbasic <- gls.asyn2
sem.comvarbasic <- gls(Temp_Var ~ response.fdis + Com.Asynch, data = bird.long,
                       corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
sem.coefvarbasic <- gls(CoV ~ response.fdis + Com.Asynch + Temp_Var, data = bird.long,
                        corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
sem.effectbasic <- gls(log(cv.fdis) ~ Temp_Var + CoV + response.fdis, data = bird.long,
                       corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))

sem.basic <- psem(sem.asynbasic, sem.comvarbasic, sem.coefvarbasic, sem.effectbasic)
summary(sem.basic) # AIC = 52.867 
                   # Fishers c = 2.867
                   # p val for fishers c =  0.239
