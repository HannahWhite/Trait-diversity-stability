###########################################################################
### Calculate coefficient of variation for overall community abundances ###
###########################################################################

### Hannah White 02.11.2021

setwd('D:/ResponseEffectDiversity/RomaniaData')
Bird_comp<-read.csv(file = '2013-19_birds.csv',header = TRUE)

Site_data<-read.csv(file = 'longform_Site_data.csv',header = TRUE)

## Get hellinger transformed and bird compositions for sites have environmental data for
load('hellinger_sites.RData')

bird.comm<-Bird_comp[Bird_comp$Site %in% Site_data$Site_ID,]

rm(Bird_comp)

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


##### Calculate coefficient of variances #####

### sum abundances

## raw
comm <- rowSums(bird.comm[,5:120])
comm.sites <- data.frame(bird.comm[, 1:4], comm)

mean.comm <- aggregate(comm ~ Site, data = comm.sites, FUN = function (x) mean(x, na.rm = TRUE))
sd.comm <- aggregate(comm ~ Site, data = comm.sites, FUN = function (x) sd(x, na.rm = TRUE))

cv.comm <- data.frame(Site = sd.comm$Site, CoV = sd.comm$comm/mean.comm$comm)


##hellinger
hell <- rowSums(t.all2[,5:120])
hell.sites <- data.frame(t.all2[,1:4], hell)

mean.hell <- aggregate(hell ~ Site, data = hell.sites, FUN = function (x) mean(x, na.rm = TRUE))
sd.hell <- aggregate(hell ~ Site, data = hell.sites, FUN = function (x) sd(x, na.rm = TRUE))

cv.hell <- data.frame(Site = sd.hell$Site, CoV = sd.hell$hell/mean.hell$hell)

save(cv.comm, cv.hell, file = 'communityCoV.RData')


