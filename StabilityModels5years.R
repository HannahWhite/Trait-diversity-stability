#################################################################################
### Models for H1 and H2 - Trait diversity and 5 year stability relationships ###
#################################################################################

### Hannah White 13.01.2022
### Edited 24.04.2022 to change to 5+ year time series
### Edited 28.04.2022 to add models of effect diversity
library(ggplot2)
library(nlme)
library(spdep)
library(piecewiseSEM)

## Read in data
setwd('D:\\ResponseEffectDiversity\\RomaniaData')

load('Bird_Temp_CV.RData')
load('communityCoV.RData')
#load('FDRomania.RData')
load('FDRomaniaSiteLevel.RData')

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

# Some quick plots to look at data

p.asynch <- ggplot(bird.long, aes(x = response.fdis, y = Com.Asynch)) + geom_point(aes(colour = Village))
p.commvar <- ggplot(bird.long, aes(x = response.fdis, y = Temp_Var)) + geom_point(aes(colour = Village))
p.coefvar <- ggplot(bird.long, aes(x = response.fdis, y = CoV)) + geom_point(aes(colour = Village))


### Check variograms

m.asynch <- gls(Com.Asynch ~ xcoord + ycoord, data = bird.long)
plot(Variogram(m.asynch, form = ~xcoord+ycoord))

m.commvar <- gls(Temp_Var ~ xcoord + ycoord, data = bird.long)
plot(Variogram(m.commvar, form = ~xcoord + ycoord))

m.coefvar <- gls(CoV ~ xcoord + ycoord, data = bird.long)
plot(Variogram(m.coefvar, form = ~xcoord + ycoord))


### Linear models

lm.asynch <- lm(Com.Asynch ~ response.fdis, data = bird.long)
hist(resid(lm.asynch)) # residuals not normally distributed
plot(lm.asynch)

lm.commvar <- lm(Temp_Var ~ response.fdis, data = bird.long)
hist(resid(lm.commvar)) #residuals are normally distributed
plot(lm.commvar)
summary(lm.commvar) # relationship between response fdis and temporal community variability

lm.coefvar <- lm(CoV ~ response.fdis, data = bird.long)
hist(resid(lm.coefvar)) # residuals are not normally distributed
plot(lm.coefvar)


lm.coefvar2 <- lm(log(CoV) ~ response.fdis, data = bird.long)
hist(resid(lm.coefvar2)) # residuals are not normally distributed
plot(lm.coefvar2)
summary(lm.coefvar2)


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
                 corr = corExp(form = ~ xcoord + ycoord, nugget = TRUE)) # AIC = -129.7835

gls.asyn2 <- gls(Com.Asynch ~ response.fdis, data = bird.long,
                 corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE)) # AIC = -130.089

gls.asyn3 <- gls(Com.Asynch ~ response.fdis, data = bird.long,
                 corr = corLin(form = ~xcoord + ycoord, nugget = TRUE)) ## false convergence

gls.asyn4 <- gls(Com.Asynch ~ response.fdis, data = bird.long, # AIC = -129.7331
                 corr = corRatio(c(2000, 0.7), form = ~xcoord + ycoord, nugget = TRUE))  #include distance where semivariogram = (1+nugget)/2

gls.asyn5 <- gls(Com.Asynch ~ response.fdis, data = bird.long,
                 corr = corSpher(c(10000, 0.7), form = ~xcoord + ycoord, nugget = TRUE)) # AIC = -130.0876


## model with Gaussian correlation structure has lowest AIC but all much of a muchness
## Now need to check adequcy of model

plot(Variogram(gls.asyn2, resType = 'n')) #it hasn't really improved the autocorrelation all that much 
plot(gls.asyn2, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.asyn2, type ='n'), breaks = 30) # not good
qqnorm(gls.asyn2, ~resid(.,type = 'n'))

# bubble diagram to visualise potential autocorrelation
asyn<-data.frame(xcoord = bird.long$xcoord,ycoord = bird.long$ycoord,resids=resid(gls.asyn2))
coordinates(asyn)<-c('xcoord','ycoord')
bubble(asyn,zcol='resids')


summary(gls.asyn2) # response diversity positively affects Aynchrony

### Community temporal variability
# will stick with Gaussian correlation 

gls.commvar <- gls(Temp_Var ~ response.fdis, data = bird.long,
                   corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))

plot(Variogram(gls.commvar, resType = 'n'))
plot(gls.commvar, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.commvar, type ='n'))
qqnorm(gls.commvar, ~resid(.,type = 'n'))

summary(gls.commvar) # positive relationship between temporal variability and response div


### Coefficient of variation

gls.coefvar <- gls(CoV ~ response.fdis, data = bird.long,
                   corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))

plot(Variogram(gls.coefvar, resType = 'n'))
plot(gls.coefvar, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.coefvar, type ='n'))
qqnorm(gls.coefvar, ~resid(.,type = 'n')) # actually this is not great

summary(gls.coefvar) # no relationship between response diversity and CoV of entire community

## Try logging coefficient of variation
gls.coefvar2 <- gls(log(CoV) ~ response.fdis, data = bird.long,
                    corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))

plot(Variogram(gls.coefvar2, resType = 'n'))
plot(gls.coefvar2, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.coefvar2, type ='n'))
qqnorm(gls.coefvar2, ~resid(.,type = 'n')) # actually this is not great

summary(gls.coefvar2)

#### Calculating Moran's i

coord.mat <- matrix(cbind(bird.long$xcoord, bird.long$ycoord), ncol = 2)
coord.knn <- knearneigh(coord.mat, k = 5)

coord.nb <- knn2nb(coord.knn)
coord.lw <- nb2listw(coord.nb, style = 'W')

# asynch
moran(bird.long$Com.Asynch, coord.lw, length(coord.nb), Szero(coord.lw))
#I = 0.321 #K = 9.31
moran.test(bird.long$Com.Asynch, coord.lw, alternative = 'greater') # p<0.001
moran.mc(bird.long$Com.Asynch, coord.lw, nsim = 999, alternative = 'greater') # p = 0.001


# Temp_Var
moran(bird.long$Temp_Var, coord.lw, length(coord.nb), Szero(coord.lw))
#I = 0.494 K = 2.722
moran.test(bird.long$Temp_Var, coord.lw, alternative = 'greater') #  p<0.001
moran.mc(bird.long$Temp_Var, coord.lw, nsim = 999, alternative = 'greater') # p = 0.001


# CoV
moran(bird.long$CoV, coord.lw, length(coord.nb), Szero(coord.lw))
#I = 0.402 K = 10.371
moran.test(bird.long$CoV, coord.lw, alternative = 'greater') # p<0.001
moran.mc(bird.long$CoV, coord.lw, nsim = 999, alternative = 'greater') # p = 0.001

### Model temporal variability/coefficient of variation as a function of asynchrony


#stab.basic <- gls(Temp_Var ~ Com.Asynch, data = bird.long)
#plot(Variogram(stab.basic, form = ~xcoord + ycoord)) # extract distance (8000) and nugget (0.9)

gls.stab <- gls(Temp_Var ~ Com.Asynch, data = bird.long,
                corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))

plot(Variogram(gls.stab, resType = 'n'))
plot(gls.stab, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.stab, type ='n'))
qqnorm(gls.stab, ~resid(.,type = 'n')) 

summary(gls.stab) # no relationship between synchrony and temporal community variability

#abund.basic <- gls(CoV ~ Com.Asynch, data = bird.long)
#plot(Variogram(abund.basic, form = ~xcoord + ycoord))# extract distance (5000) and nugget (0.9)

gls.abund <- gls(CoV ~ Com.Asynch, data = bird.long,
                 corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))

plot(Variogram(gls.abund, resType = 'n'))
plot(gls.abund, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.abund, type ='n'))
qqnorm(gls.abund, ~resid(.,type = 'n')) 

summary(gls.abund) # higher asynchrony leads to lower CoV in community abundance  

# but what about logging like above
#gls.abund2 <- gls(log(CoV) ~ Com.Asynch, data = bird.long,
#                 corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))

#plot(Variogram(gls.abund2, resType = 'n'))
#plot(gls.abund2, resid(., type = 'n') ~ fitted(.), abline = 0)
#hist(resid(gls.abund2, type ='n'))
#qqnorm(gls.abund2, ~resid(.,type = 'n')) 

##################################################
### Strucutral Equation Model for Hypothesis 1 ###
##################################################

### Set up models

sem.async <- gls.asyn2
sem.comvar <- gls(Temp_Var ~ response.fdis + Com.Asynch, data = bird.long,
                  corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(sem.comvar, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(sem.comvar, type ='n'))
qqnorm(sem.comvar, ~resid(.,type = 'n')) 



sem.coefvar <- gls(CoV ~ response.fdis + Com.Asynch, data = bird.long,
                   corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(sem.coefvar, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(sem.coefvar, type ='n')) # seems like it might be okay unlogged?
qqnorm(sem.coefvar, ~resid(.,type = 'n')) 




sem1 <- psem(sem.async, sem.comvar, sem.coefvar)
summary(sem1)

plot(sem1)

############################################
### Adding in models of effect diversity ###
############################################

load('CVeffectdiv.RData')

bird.long <- merge(bird.long, cv.effectdiv, by ='Site', all.x = TRUE, all.y = FALSE)


# check Moran's I

moran(bird.long$cv.fdis, coord.lw, length(coord.nb), Szero(coord.lw))
#I = 0.201 #K = 4.16
moran.test(bird.long$cv.fdis, coord.lw, alternative = 'greater') # p<0.001
moran.mc(bird.long$cv.fdis, coord.lw, nsim = 999, alternative = 'greater') # p = 0.002



effect.comm <- gls(log(cv.fdis) ~ Temp_Var, data = bird.long,
                   corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(effect.comm, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(effect.comm, type ='n'))
qqnorm(effect.comm, ~resid(.,type = 'n')) 

summary(effect.comm) # no relationship between variation in community composition and variability in effect trait diversity

effect.cov <- gls(log(cv.fdis) ~ CoV, data = bird.long,
                  corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(effect.cov, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(effect.cov, type ='n'))
qqnorm(effect.cov, ~resid(.,type = 'n')) 

summary(effect.cov) ## variability in overall variation in total abundance increases variability in effect trait diversity

sem.effect <- gls(log(cv.fdis) ~ Temp_Var + CoV, data = bird.long,
                  corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(sem.effect, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(sem.effect, type ='n'))
qqnorm(sem.effect, ~resid(.,type = 'n')) 

summary(sem.effect)

# quick look at lm
effect.lm <- lm(log(cv.fdis) ~ Temp_Var + CoV, data = bird.long)

### Add to the structural equation model

sem2 <- psem(sem.async, sem.comvar, sem.coefvar, sem.effect)
summary(sem2)

## Check tests of directed separation
dSep(sem2)

## Looks like cv.fdis might also be dependent on response.fdis, and CoV might be related to Temp_Var
## Add these to SEM

sem.effect2 <- gls(log(cv.fdis) ~ Temp_Var + CoV + response.fdis, data = bird.long,
                   corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(sem.effect2, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(sem.effect2, type ='n'))
qqnorm(sem.effect2, ~resid(.,type = 'n'))

sem.coefvar2 <- gls(CoV ~ response.fdis + Com.Asynch + Temp_Var, data = bird.long,
                    corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(sem.coefvar2, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(sem.coefvar2, type ='n')) # seems like it might be okay unlogged?
qqnorm(sem.coefvar2, ~resid(.,type = 'n'))

sem3 <- psem(sem.async, sem.comvar, sem.coefvar2, sem.effect2)
summary(sem3) # so now the only thing that impacts the variability of effect trait diversity is response trait diversity

