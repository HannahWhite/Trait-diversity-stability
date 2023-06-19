#################################################################
### Impact of land cover on diversity-stability relationships ###
#################################################################

### Hannah White 24.05.2022
library(nlme)
library(multcomp)
library(lsmeans)

## Read in data
setwd('RomaniaData')

load('bird.long.RData')
load('CVeffectdivExtraTraits.RData')
load('landcover.RData')


### Merge data

bird.long <- merge(bird.long, cv.effectdiv, by ='Site', all.x = TRUE, all.y = FALSE)
bird.long <- merge(bird.long, lc.agg, by.x = 'Site', by.y = 'Site_ID', all = TRUE)

bird.long$majority <- as.factor(bird.long$majority)
levels(bird.long$majority) <- c('1' = 'urban', '2' = 'agri', '3' = 'forest')
## 11 are urban, 62 are agri and 25 are forest

### Comparisons between land cover at local scale (majority landcover in 100m buffer)

## response trait diversity
plot(response.fdis ~ majority, data = bird.long)

hist(bird.long[which(bird.long$majority=='urban'),'response.fdis'])

hist(bird.long[which(bird.long$majority=='agri'),'response.fdis'])
hist(bird.long[which(bird.long$majority=='forest'),'response.fdis'])

aov.response <- aov(response.fdis ~ majority, data = bird.long) 
gls.response <- gls(response.fdis ~ majority, data = bird.long,
                    corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(gls.response, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.response, type ='n')) 

# multiple comparisons - need to set up gls method
model.matrix.gls <- function(object, ...) {
  model.matrix(terms(object), data = getData(object), ...)
}
model.frame.gls <- function(object, ...) {
  model.frame(formula(object), data = getData(object), ...)
}
terms.gls <- function(object, ...) {
  terms(model.frame(object), ...)
} 

mc.response <- glht(gls.response, linfct = mcp(majority = "Tukey")) 
ci.response <- confint(mc.response) # differences between urban and the other two. no difference between forest and agriculture

# glht method now not working - try lsmeans
ls.response <- lsmeans(gls.response, list(pairwise ~ majority), adjust = 'Tukey')

## effect trait diversity
plot(effect.fdis ~ majority, data = bird.long)
aov.effect <- aov(effect.fdis ~ majority, data = bird.long) 
gls.effect <- gls(effect.fdis ~ majority, data = bird.long,
                  corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(gls.effect, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.effect, type ='n')) # 

ci.effect <- confint(glht(gls.effect, linfct = mcp(majority = "Tukey"))) 
ls.effect <- lsmeans(gls.effect, list(pairwise ~ majority), adjust = 'Tukey')

## effect trait variability
plot(cv.fdis ~ majority, data = bird.long)
aov.effectvar <- aov(cv.fdis ~ majority, data = bird.long) 
gls.effectvar <- gls(cv.fdis ~ majority, data = bird.long,
                     corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(gls.effectvar, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.effectvar, type ='n')) # 

ci.effectvar <- confint(glht(gls.effectvar, linfct = mcp(majority = "Tukey"))) 
ls.effectvar <- lsmeans(gls.effectvar, list(pairwise ~ majority), adjust = 'Tukey')

## asynchrony
plot(Com.Asynch ~ majority, data = bird.long)
aov.asyn <- aov(Com.Asynch ~ majority, data = bird.long) 

gls.asyn <- gls(Com.Asynch ~ majority, data = bird.long,
                corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(gls.asyn, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.asyn, type ='n')) # 

ci.asyn <- confint(glht(gls.asyn, linfct = mcp(majority = "Tukey"))) 

ls.asyn <- lsmeans(gls.asyn, list(pairwise ~ majority), adjust = 'Tukey')

## temporal variance
plot(Temp_Var ~ majority, data = bird.long)
aov.temp <- aov(Temp_Var ~ majority, data = bird.long) 

gls.temp <- gls(Temp_Var ~ majority, data = bird.long,
                corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(gls.temp, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.temp, type ='n')) # not too bad

ci.temp <- confint(glht(gls.temp, linfct = mcp(majority = "Tukey"))) 
ls.temp <- lsmeans(gls.temp, list(pairwise ~ majority), adjust = 'Tukey')

## CoV
plot(CoV ~ majority, data = bird.long)
aov.cov <- aov(CoV ~ majority, data = bird.long) # p = 0.00626

gls.cov <- gls(CoV ~ majority, data = bird.long,
               corr = corGaus(form = ~xcoord + ycoord, nugget = TRUE))
plot(gls.cov, resid(., type = 'n') ~ fitted(.), abline = 0)
hist(resid(gls.cov, type ='n')) # not too bad

ci.cov <- confint(glht(gls.cov, linfct = mcp(majority = "Tukey"))) 
ls.cov <- lsmeans(gls.cov, list(pairwise ~ majority), adjust = 'Tukey')

#### Interaction Models

## Model of asynchrony
# check of nugget is needed
asyn.nug <- gls(Com.Asynch ~ response.fdis, data = bird.long)
plot(Variogram(asyn.nug, form = ~xcoord + ycoord)) # nugget ~ 0.6

# AICs with nugget  = FALSE 
#asyn1 <- gls(Com.Asynch ~ response.fdis, data = bird.long,
#             corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC =-133.9201


asyn2 <- gls(Com.Asynch ~ response.fdis*majority, data = bird.long,
             corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC = -140.7454

#asyn3 <- gls(Com.Asynch ~ response.fdis + majority, data = bird.long,
#             corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC = -132.2538

## Model of temporal variance

comvar1 <- gls(Temp_Var ~ response.fdis, data = bird.long,
               corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC = -383.0039

comvar2 <- gls(Temp_Var ~ response.fdis*majority, data = bird.long,
               corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC = -383.7301

#comvar3 <- gls(Temp_Var ~ response.fdis + majority, data = bird.long,
#               corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC = -376.7198



## Model of coefficient of variation

#cov1 <- gls(CoV ~ response.fdis, data = bird.long,
#            corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC = 3.668154

cov2 <- gls(CoV ~ response.fdis*majority, data = bird.long,
            corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC = -14.82104

#cov3 <- gls(CoV ~ response.fdis + majority, data = bird.long,
#            corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC = -2.686709



## marginal means
library(emmeans)
mmasyn2 <- emtrends(asyn2, pairwise ~ majority, var = 'response.fdis', mode = "df.error")

### Estimate average main effect for reporting
contrasts(bird.long$majority) <- c(-1, 0, 1)
asyn2cont <- gls(Com.Asynch ~ response.fdis*majority, data = bird.long,
                 corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC = - 137.1619 


## Functional variability
plot_model(cov2, type = 'int')

mmcov2 <- emtrends(cov2, pairwise ~ majority, var = 'response.fdis', mode = 'df.error')

#emmip(cov2, majority ~ response.fdis, cov.reduce = range)

contrasts(bird.long$majority) <- c(-1, 0, 1)
cov2cont <- gls(CoV ~ response.fdis*majority, data = bird.long,
                corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC = -11.2375



### Models of effect trait variability

# model of temporal variance
# have changed nugget to false to help with convergence (to be fair, it's only about 0.6 anyway)
#effect.comvar1 <- gls(log(cv.fdis) ~ Temp_Var, data = bird.long,
#                     corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC = 158.2844
#plot(effect.comvar1, resid(., type = 'n') ~ fitted(.), abline = 0)
#hist(resid(effect.comvar1, type ='n'))
#qqnorm(effect.comvar1, ~resid(.,type = 'n')) 

effect.comvar2 <- gls(log(cv.fdis) ~ Temp_Var*majority, data = bird.long,
                      corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC = 155.3662


#effect.comvar3 <- gls(log(cv.fdis) ~ Temp_Var + majority, data = bird.long,
#                      corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC = 161.2631


# model of coefficient of variation
effect.cov1 <- gls(log(cv.fdis) ~ CoV, data = bird.long,
                   corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC = 159.7452

#effect.cov2 <- gls(log(cv.fdis) ~ CoV*majority, data = bird.long,
#                   corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC = 167.1927

#effect.cov3 <- gls(log(cv.fdis) ~ CoV + majority, data = bird.long,
#                   corr = corGaus(form = ~xcoord + ycoord, nugget = FALSE)) # AIC = 165.8566
