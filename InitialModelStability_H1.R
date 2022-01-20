##############################################################################
### Initial models for H1 - Response diversity and stability relationships ###
##############################################################################

### Hannah White 13.01.2022

### This code investigates the relationship between the functional dispersion of response traits, 
### and different components of community stability. 
### It uses gls to build and SEM using piecewiseSEM and investigates different spatial error structures


library(ggplot2)
library(nlme)
library(spdep)
library(piecewiseSEM)

## Read in data
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

# Cut data down to sites with >= 4 year data

bird.long <- Bird_Temp_CV[Bird_Temp_CV$N_years >= 4,]

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
summary(lm.commvar) # no relationship between response fdis and temporal community variability

lm.coefvar <- lm(CoV ~ response.fdis, data = bird.long)
hist(resid(lm.coefvar)) # residuals are not normally distributed
plot(lm.coefvar)


### Generalised Least Squares

## Asynchrony
# test different correlation structures
gls.basic <- gls(Com.Asynch ~ response.fdis, data = bird.long)
plot(Variogram(gls.basic, form = ~xcoord + ycoord)) # extract distance (10000) and nugget (0.7)

gls.asyn1 <- gls(Com.Asynch ~ response.fdis, data = bird.long,
                 corr = corExp(form = ~ xcoord + ycoord, nugget = TRUE))

gls.asyn2 <- gls(Com.Asynch ~ response.fdis, data = bird.long,
                 corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))

gls.asyn3 <- gls(Com.Asynch ~ response.fdis, data = bird.long,
                 corr = corLin(form = ~xcoord + ycoord, nugget = TRUE))

gls.asyn4 <- gls(Com.Asynch ~ response.fdis, data = bird.long,
                 corr = corRatio(c(2000, 0.7), form = ~xcoord + ycoord, nugget = TRUE))  #include distance where semivariogram = (1+nugget)/2

gls.asyn5 <- gls(Com.Asynch ~ response.fdis, data = bird.long,
                 corr = corSpher(c(10000, 0.7), form = ~xcoord + ycoord, nugget = TRUE))


## model with Ratio correlation structure has lowest AIC. 
## Now need to check adequcy of model

plot(Variogram(gls.asyn4, resType = 'n'))
plot(gls.asyn4, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.asyn4, type ='n'))
qqnorm(gls.asyn4, ~resid(.,type = 'n'))

summary(gls.asyn4) # response diversity positively affects Aynchrony

### Community temporal variability
# will stick with Ratio correlation but need to work out nugget

commvar.basic <- gls(Temp_Var ~ response.fdis, data = bird.long)
plot(Variogram(commvar.basic, form = ~xcoord + ycoord)) # extract distance (8000) and nugget (0.9)

gls.commvar <- gls(Temp_Var ~ response.fdis, data = bird.long,
                   corr = corRatio(c(6000, 0.9), form = ~xcoord + ycoord, nugget = TRUE))

plot(Variogram(gls.commvar, resType = 'n'))
plot(gls.commvar, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.commvar, type ='n'))
qqnorm(gls.commvar, ~resid(.,type = 'n'))

summary(gls.commvar) # no relationship between temporal variability and response div


### Coefficient of variation
coefvar.basic <- gls(CoV ~ response.fdis, data = bird.long)
plot(Variogram(coefvar.basic, form = ~xcoord + ycoord)) # extract distance (8000) and nugget (0.8)

gls.coefvar <- gls(CoV ~ response.fdis, data = bird.long,
                   corr = corRatio(c(5000, 0.8), form = ~xcoord + ycoord, nugget = TRUE))

plot(Variogram(gls.coefvar, resType = 'n'))
plot(gls.coefvar, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.coefvar, type ='n'))
qqnorm(gls.coefvar, ~resid(.,type = 'n')) # actually this is not great

summary(gls.coefvar) # higher response diversity leads to lower coef of var of entire


#### Calculating Moran's i

coord.mat <- matrix(cbind(bird.long$xcoord, bird.long$ycoord), ncol = 2)
coord.knn <- knearneigh(coord.mat, k = 5)

coord.nb <- knn2nb(coord.knn)
coord.lw <- nb2listw(coord.nb, style = 'W')

# asynch
moran(bird.long$Com.Asynch, coord.lw, length(coord.nb), Szero(coord.lw))
#I = 0.1999 #K = 5.49
moran.test(bird.long$Com.Asynch, coord.lw, alternative = 'greater') # p<0.001
moran.mc(bird.long$Com.Asynch, coord.lw, nsim = 999, alternative = 'greater') # p = 0.001


# Temp_Var
moran(bird.long$Temp_Var, coord.lw, length(coord.nb), Szero(coord.lw))
#I = 0.338 K = 2.85
moran.test(bird.long$Temp_Var, coord.lw, alternative = 'greater') #  p<0.001
moran.mc(bird.long$Temp_Var, coord.lw, nsim = 999, alternative = 'greater') # p = 0.001


# CoV
moran(bird.long$CoV, coord.lw, length(coord.nb), Szero(coord.lw))
#I = 0.187 K = 6.005
moran.test(bird.long$CoV, coord.lw, alternative = 'greater') # p<0.001
moran.mc(bird.long$CoV, coord.lw, nsim = 999, alternative = 'greater') # p = 0.001

### Model temporal variability/coefficient of variation as a function of asynchrony
# again keep correlation as Ration

stab.basic <- gls(Temp_Var ~ Com.Asynch, data = bird.long)
plot(Variogram(stab.basic, form = ~xcoord + ycoord)) # extract distance (8000) and nugget (0.9)

gls.stab <- gls(Temp_Var ~ Com.Asynch, data = bird.long,
                corr = corRatio(c(3000, 0.9), form = ~xcoord + ycoord, nugget = TRUE))

plot(Variogram(gls.stab, resType = 'n'))
plot(gls.stab, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.stab, type ='n'))
qqnorm(gls.stab, ~resid(.,type = 'n')) 

summary(gls.stab) # higher asynchrony leads to lower temporal community variability

abund.basic <- gls(CoV ~ Com.Asynch, data = bird.long)
plot(Variogram(abund.basic, form = ~xcoord + ycoord))# extract distance (5000) and nugget (0.9)

gls.abund <- gls(CoV ~ Com.Asynch, data = bird.long,
                 corr = corRatio(c(4000, 0.9), form = ~xcoord + ycoord, nugget = TRUE))

plot(Variogram(gls.abund, resType = 'n'))
plot(gls.abund, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.abund, type ='n'))
qqnorm(gls.abund, ~resid(.,type = 'n')) 

summary(gls.abund) # higher asynchrony leads to lower CoV in community abundance

##################################################
### Strucutral Equation Model for Hypothesis 1 ###
##################################################

### Set up models

sem.async <- gls.asyn4
sem.comvar <- gls(Temp_Var ~ response.fdis + Com.Asynch, data = bird.long,
                  corr = corRatio(c(6000, 0.9), form = ~xcoord + ycoord, nugget = TRUE))
sem.coefvar <- gls(CoV ~ response.fdis + Com.Asynch, data = bird.long,
                   corr = corRatio(c(5000, 0.9), form = ~xcoord + ycoord, nugget = TRUE))


sem1 <- psem(sem.async, sem.comvar, sem.coefvar)
summary(sem1)


