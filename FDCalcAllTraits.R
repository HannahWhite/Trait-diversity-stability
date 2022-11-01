##############################################
### FD calculations with additional traits ###
##############################################

### Hannah white 27.06.2022
### FD calculation includes morphology traits from AVONET

library(FD)
library(vegan)

### Read in and clean data
#load('TraitsRom.RData')

######
#bird.comm <- read.csv('2013-19_birds.csv', header = TRUE)

# remove columns starting pa, DM and check
col.pa <- grep('^pa_.*', names(bird.comm))
bird.comm <- bird.comm[,-col.pa]

col.dm <- grep('^DM.*', names(bird.comm))
bird.comm <- bird.comm[,-col.dm]

col.check <- grep('^CHECK.*', names(bird.comm))
bird.comm <- bird.comm[,-col.check]

names(bird.comm) <- gsub('Columba_livia_domest', 'Columba_livia', names(bird.comm))
names(bird.comm) <- gsub('Buteo_buteo_vulpinus', 'Buteo_buteo', names(bird.comm))
names(bird.comm) <- gsub('Dendrocopos__leucotos', 'Dendrocopos_leucotos', names(bird.comm))
names(bird.comm) <- gsub('Dendrocopus_medius', 'Dendrocopos_medius', names(bird.comm))
names(bird.comm) <- gsub('Delichon_urbica', 'Delichon_urbicum', names(bird.comm))
names(bird.comm) <- gsub('Ardea_Purpurea', 'Ardea_purpurea', names(bird.comm))
names(bird.comm) <- gsub('Columba_palumbas', 'Columba_palumbus', names(bird.comm))
names(bird.comm) <- gsub('Cyanistes_caeruleus', 'Parus_caeruleus', names(bird.comm))
names(bird.comm) <- gsub('Periparus_ater', 'Parus_ater', names(bird.comm))
names(bird.comm) <- gsub('Emberiza_calandra', 'Miliaria_calandra', names(bird.comm))
names(bird.comm) <- gsub('Chloris_chloris', 'Carduelis_chloris', names(bird.comm))
names(bird.comm) <- gsub('Poecile_palustris', 'Parus_palustris', names(bird.comm))
names(bird.comm) <- gsub('Iduna_pallida', 'Hippolais_pallida', names(bird.comm))
names(bird.comm) <- gsub('Saxicola_rubetrus', 'Saxicola_rubetra', names(bird.comm))
names(bird.comm) <- gsub('Saxocola_torquatus', 'Saxicola_torquatus', names(bird.comm))
names(bird.comm) <- gsub('Circus_circus', 'Circus_cyaneus', names(bird.comm))
names(bird.comm) <- gsub('Fulcia_atra', 'Fulica_atra', names(bird.comm))


### Find columns where species is has not been identified to sp level

not.sp <- grep('_sp', names(bird.comm))
bird.notsp <- bird.comm[,not.sp]

range(bird.notsp)
apply(bird.notsp, 2, FUN = function(x) length(which(x > 0)))

fam <- grep('idae', names(bird.comm))
bird.fam <- bird.comm[,fam]

range(bird.fam)
apply(bird.fam, 2, FUN = function(x) length(which(x > 0)))

sp.rm <- c(not.sp, fam)

### Take these species out for now but may add back in

bird.comm <- bird.comm[ ,-sp.rm]

### Combine Buteo columns and other corrected bird species names
bird.comm$Buteo_buteo <- bird.comm$Buteo_buteo + bird.comm$Buteo_buteo.1
bird.comm <- subset(bird.comm, select = -c(Buteo_buteo.1))

bird.comm$Saxicola_rubetra <- bird.comm$Saxicola_rubetra + bird.comm$Saxicola_rubetra.1
bird.comm <- subset(bird.comm, select = -c(Saxicola_rubetra.1))

bird.comm$Saxicola_torquatus <- bird.comm$Saxicola_torquatus + bird.comm$Saxicola_torquatus.1
bird.comm <- subset(bird.comm, select = -c(Saxicola_torquatus.1))

sp <- names(bird.comm)[6:121]

#bird.missing <- sp[!sp %in% traits.rom$species]


### clean traits up to those want to use (body mass, clutch size, and the three plasticity measures)

traits.all <- traits.rom[c(1, 22:26, 42:43, 46)]

# set species name to row name
row.names(traits.all) <- traits.all[,1]
traits.all <- traits.all[,-1]

traits.response <- traits.all[,c(1, 4:6)]
traits.effect <- traits.all[,c(1:3, 7:8)]

############################################################################
### Functional diversity at each site in each village at each time point ###
############################################################################

# order bird.comm columns alphabetically

abund <- bird.comm[,5:120]
bird.alpha <- abund[,order(names(abund))]

### All traits
all.dist <- gowdis(traits.all) # make sure gowdis distances are used so that traits are scaled
all.FD <- dbFD(all.dist, bird.alpha)

## Response traits
response.dist <- gowdis(traits.response)
response.FD <- dbFD(response.dist, bird.alpha, corr = 'lingoes')

## Effect traits 
effect.dist <- gowdis(traits.effect)
effect.FD <- dbFD(effect.dist, bird.alpha)

### Calculate functional redundancy as in de Bello et al. 2007

### calculate simpsons
simp <- diversity(bird.alpha, index = 'simpson')

### All traits
fred.all <- simp - all.FD$RaoQ

### Response traits
fred.response <- simp - response.FD$RaoQ

### Effect traits
fred.effect <- simp - effect.FD$RaoQ

######## Create dataframe of functional diversity measures at separate time points

FD.df <- data.frame(bird.comm[,1:4], all.SR = all.FD$nbsp, all.fdis = all.FD$FDis, all.raoq = all.FD$RaoQ, all.fred = fred.all,
                    response.SR = response.FD$nbsp, response.fdis = response.FD$FDis, response.raoq = response.FD$RaoQ, response.fred = fred.response,
                    effect.SR = effect.FD$nbsp, effect.fdis = effect.FD$FDis, effect.raoq = effect.FD$RaoQ, effect.fred = fred.effect)


#save(FD.df, file = 'FDRomaniaExtraTraits.RData')


#####################################################
##### Overall functional diversity at each site #####
#####################################################

bird.site <- bird.comm[,c(2, 3, 5:120)] # extracts village, site and species columns
bird.mean <- aggregate(. ~ Village + Site, data = bird.site, mean)

mean.abund <- bird.mean[,3:118]
mean.alpha <- mean.abund[, order(names(mean.abund))]



### All traits
#all.dist <- gowdis(traits.all) # make sure gowdis distances are used so that traits are scaled
all.FDbysite <- dbFD(all.dist, mean.alpha)

## Response traits
#response.dist <- gowdis(traits.response)
response.FDbysite <- dbFD(response.dist, mean.alpha, corr = 'lingoes')

## Effect traits 
#effect.dist <- gowdis(traits.effect)
effect.FDbysite <- dbFD(effect.dist, mean.alpha)

### calculate simpsons
simp.bysite <- diversity(mean.alpha, index = 'simpson')

### All traits
fred.all.bysite <- simp.bysite - all.FDbysite$RaoQ

### Response traits
fred.response.bysite <- simp.bysite - response.FDbysite$RaoQ

### Effect traits
fred.effect.bysite <- simp.bysite - effect.FDbysite$RaoQ

FDsites.df <- data.frame(bird.mean[,1:2], all.SR = all.FDbysite$nbsp, all.fdis = all.FDbysite$FDis, all.raoq = all.FDbysite$RaoQ, all.fred = fred.all.bysite,
                         response.SR = response.FDbysite$nbsp, response.fdis = response.FDbysite$FDis, response.raoq = response.FDbysite$RaoQ, response.fred = fred.response.bysite,
                         effect.SR = effect.FDbysite$nbsp, effect.fdis = effect.FDbysite$FDis, effect.raoq = effect.FDbysite$RaoQ, effect.fred = fred.effect.bysite)

#save(FDsites.df, file = 'FDRomaniaSiteLevelExtraTraits.RData')

