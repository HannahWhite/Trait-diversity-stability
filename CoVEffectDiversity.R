####################################################
### Coefficient of Variation of effect diversity ###
####################################################

### Hannah White 25.04.2022
### Edited 19.06.2023

# load functional diversity measures at each time point
load('D:\\ResponseEffectDiversity\\RomaniaData\\FDRomaniaRevisedNumeric.RData')

##### Calculate coefficient of variances #####

## Cov Fdis
mean.fdis <- aggregate(effect.fdis ~ Site, data = FD.df, FUN = function(x) mean(x, na.rm = TRUE))
sd.fdis <- aggregate(effect.fdis ~ Site, data = FD.df, FUN = function(x) sd(x, na.rm = TRUE))

cv.fdis <- sd.fdis$effect.fdis/mean.fdis$effect.fdis

## CoV raoq
mean.raoq <- aggregate(effect.raoq ~ Site, data = FD.df, FUN = function(x) mean(x, na.rm = TRUE))
sd.raoq <- aggregate(effect.raoq ~ Site, data = FD.df, FUN = function(x) sd(x, na.rm = TRUE))

cv.raoq <- sd.raoq$effect.raoq/mean.raoq$effect.raoq

## Cov functional redundancy
mean.fred <- aggregate(effect.fred ~ Site, data = FD.df, FUN = function(x) mean(x, na.rm = TRUE))
sd.fred <- aggregate(effect.fred ~ Site, data = FD.df, FUN = function(x) sd(x, na.rm = TRUE))

cv.fred <- sd.fred$effect.fred/mean.fred$effect.fred


cv.effectdiv <- data.frame(Site = mean.fdis$Site, cv.fdis, cv.raoq, cv.fred)

save(cv.effectdiv, file = 'D:\\ResponseEffectDiversity\\RomaniaData\\CVeffectdivRevisedNumeric.RData')
