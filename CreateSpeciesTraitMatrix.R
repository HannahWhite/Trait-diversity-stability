##########################################################################
### Extract traits from Elton traits and create species x trait natrix ###
##########################################################################

#### Hannah White 04.05.2021
#### Edited 10.05 to match new species list
#### Edited 14.06.2022 to add Avonet traits
### Edited 19.06.2023 to change trait classifications and fix trait types

### Gets data from Elton Traits and extracts relevant traits for species in the 
### Romania trait dataset. Some synonyms have to be changed.  
### Also extracts clutch size from the European Bird Trait Database
### Additionally, diet and foraging plasticity is calculated as the 'richness' 
### of recorded diets and foraging strategies, respectively

### species in Romania
birds <- read.csv('BirdSpecies.csv', header = TRUE)

birds$species <- gsub('Columba_livia_domest', 'Columba_livia', birds$species)
birds$species <- gsub('Buteo_buteo_vulpinus', 'Buteo_buteo', birds$species)
birds$species <- gsub('Dendrocopos__leucotos', 'Dendrocopos_leucotos', birds$species)
birds$species <- gsub('Dendrocopus_medius', 'Dendrocopos_medius', birds$species)
#birds$species <- gsub('Dendrocopos_minor', 'Dryobates_minor', birds$species)
#birds$species <- gsub('Anas_strepera', 'Mareca_strepera', birds$species)
birds$species <- gsub('Delichon_urbica', 'Delichon_urbicum', birds$species)
#birds$species <- gsub('Aquila_pomarina', 'Clanga_pomarina', birds$species)
#birds$species <- gsub('Carduelis_cannabina', 'Linaria_cannabina', birds$species)
birds$species <- gsub('Ardea_Purpurea', 'Ardea_purpurea', birds$species)
birds$species <- gsub('Columba_palumbas', 'Columba_palumbus', birds$species)
birds$species <- gsub('Cyanistes_caeruleus', 'Parus_caeruleus', birds$species)
birds$species <- gsub('Periparus_ater', 'Parus_ater', birds$species)
birds$species <- gsub('Emberiza_calandra', 'Miliaria_calandra', birds$species)
birds$species <- gsub('Chloris_chloris', 'Carduelis_chloris', birds$species)
birds$species <- gsub('Poecile_palustris', 'Parus_palustris', birds$species)
birds$species <- gsub('Iduna_pallida', 'Hippolais_pallida', birds$species)
birds$species <- gsub('Saxicola_rubetrus', 'Saxicola_rubetra', birds$species)
birds$species <- gsub('Saxocola_torquatus', 'Saxicola_torquatus', birds$species)
birds$species <- gsub('Circus_circus', 'Circus_cyaneus', birds$species)
birds$species <- gsub('Fulcia_atra', 'Fulica_atra', birds$species)


#### Traits

## read in Elton traits
traits <- read.delim("BirdFuncDat.txt", header = TRUE)
traits$Scientific <- gsub(' ', '_', traits$Scientific)

## add row for hooded crow with same traits as carrion crow
c.corone <- traits[which(traits$Scientific == 'Corvus_corone'),]
c.cornix <- gsub('corone', 'cornix', c.corone)
traits <- rbind(traits, c.cornix)

traits.rom <- traits[traits$Scientific %in% birds$species,]
birds.missing <- birds$species[!birds$species %in% traits.rom$Scientific] # checks if any birds are missing

traits.rom <- traits.rom[,c(8, 9:20, 24:31, 36)]

### Calculate plasticity traits

traits.rom[,c(3:12, 14:22)] <- apply(traits.rom[,c(3:12, 14:22)], 2, as.numeric)

traits.rom$diet.plasticity <- apply(traits.rom[,3:12], 1, function(x) length(which(x>0)))
traits.rom$foraging.plasticity <- apply(traits.rom[,14:21], 1, function(x) length(which(x>0)))

#### Bring in clutch size from European bird trait database

## European bird trait database
traits.eu <- read.delim("EuropeanBirdTraitDatabase/EuropeanBirdsLHT.txt")
traits.eu <- traits.eu[-500,-86]

traits.eu$Species <- gsub(' ', '_', traits.eu$Species)

## add row for hooded crow with same traits as carrion crow
c.corone <- traits.eu[which(traits.eu$Species == 'Corvus_corone'),]
c.cornix <- gsub('corone', 'cornix', c.corone)
c.cornix[1] <- 500

traits.eu <- rbind(traits.eu, c.cornix)

# extract just species name and clutch size
traits.eu <- traits.eu[, c(4, 24, 53:67)]
traits.eu[,2:17] <- apply(traits.eu[,2:17], 2, as.numeric)
names(traits.eu)[1] <- 'species'

traits.eu$species <- gsub('Leiopicus_medius', 'Dendrocopos_medius', traits.eu$species)
traits.eu$species <- gsub('Dryobates_minor', 'Dendrocopos_minor',  traits.eu$species)
traits.eu$species <- gsub('Mareca_strepera', 'Anas_strepera',  traits.eu$species)
traits.eu$species <- gsub('Clanga_pomarina', 'Aquila_pomarina',traits.eu$species)
traits.eu$species <- gsub('Linaria_cannabina', 'Carduelis_cannabina',  traits.eu$species)
traits.eu$species <- gsub('Columba_palumbas', 'Columba_palumbus', traits.eu$species)
traits.eu$species <- gsub('Cyanistes_caeruleus', 'Parus_caeruleus', traits.eu$species)
traits.eu$species <- gsub('Periparus_ater', 'Parus_ater', traits.eu$species)
traits.eu$species <- gsub('Emberiza_calandra', 'Miliaria_calandra', traits.eu$species)
traits.eu$species <- gsub('Chloris_chloris', 'Carduelis_chloris', traits.eu$species)
traits.eu$species <- gsub('Poecile_palustris', 'Parus_palustris', traits.eu$species)
traits.eu$species <- gsub('Iduna_pallida', 'Hippolais_pallida', traits.eu$species)

## calculate habitat plasticity
traits.eu$habitat.plasticity <- rowSums(traits.eu[,3:17])

# subset to just romanian birds
clutch.rom <- traits.eu[traits.eu$species %in% traits.rom$Scientific,]

# merge European Bird Trait Database and Elton traits data
traits.rom <- merge(traits.rom, clutch.rom, by.x = 'Scientific', by.y = 'species', all= TRUE)

names(traits.rom)[c(1, 25)] <- c('species', 'clutch.mean')

### Add traits from Avonet
### read in trait data - AVONET_Birdlife1 sheet from Avonet supplementary information
avo <- read.csv('avonetBL.csv', header = TRUE)
avo$Species1 <- gsub(' ', '_', avo$Species1)

# remove unnecessary columns to make data easier to handle
avo <- avo [, c(2, 11:20)]

## add row for hooded crow with same traits as carrion crow
c.corone <- avo[which(avo$Species1 == 'Corvus_corone'),]
c.cornix <- gsub('corone', 'cornix', c.corone)
avo <- rbind(avo, c.cornix)

avo$Species1 <- gsub('Leiopicus_medius', 'Dendrocopos_medius', avo$Species1)
avo$Species1 <- gsub('Dryobates_minor', 'Dendrocopos_minor',  avo$Species1)
avo$Species1 <- gsub('Mareca_strepera', 'Anas_strepera',  avo$Species1)
avo$Species1 <- gsub('Clanga_pomarina', 'Aquila_pomarina',avo$Species1)
avo$Species1 <- gsub('Linaria_cannabina', 'Carduelis_cannabina',  avo$Species1)
avo$Species1 <- gsub('Cyanistes_caeruleus', 'Parus_caeruleus', avo$Species1)
avo$Species1 <- gsub('Periparus_ater', 'Parus_ater', avo$Species1)
avo$Species1 <- gsub('Emberiza_calandra', 'Miliaria_calandra', avo$Species1)
avo$Species1 <- gsub('Chloris_chloris', 'Carduelis_chloris', avo$Species1)
avo$Species1 <- gsub('Poecile_palustris', 'Parus_palustris', avo$Species1)
avo$Species1 <- gsub('Iduna_pallida', 'Hippolais_pallida', avo$Species1)

# get the species from the trait matrix that are in Romania
avo.rom <- avo[avo$Species1 %in% birds$species,]

traits.rom <- merge(traits.rom, avo.rom, by.x = 'species', by.y = 'Species1', all= TRUE)

##### Calculate extra diet 

## calculate same diet classes as Ausprey (except splitting out scavenger from other vertivore diets)
traits.rom$diet.invert <- traits.rom$Diet.Inv
traits.rom$diet.frugi <- traits.rom$Diet.Fruit
traits.rom$diet.nect <- traits.rom$Diet.Nect
traits.rom$diet.gran <- traits.rom$Diet.Seed
traits.rom$diet.herb <- traits.rom$Diet.PlantO
traits.rom$diet.carn <- rowSums(traits.rom[,c(4:7)])
traits.rom$diet.scav <- traits.rom$Diet.Scav

traits.rom$diet.breadth <- apply(traits.rom[,52:58], 1, function(x) length(which(x>0)))

### PCA to get species level diet classification
#pca <- prcomp(traits.rom[,53:59], center = TRUE, scale. = TRUE)

library(vegan)
pca.vegan <- rda(traits.rom[,52:58], scale = TRUE)
summary(pca.vegan)

traits.rom$diet.pc1 <- scores(pca.vegan)$sites[,1]
traits.rom$diet.pc2 <- scores(pca.vegan)$sites[,2]

## redefine foraging as pres/abs
foraging <- ifelse(traits.rom[,14:21] > 0, 1, 0)
colnames(foraging) <- c('foraging.wbs', 'foraging.was', 'foraging.ground', 'foraging.understory', 
                        'foraging.midhigh', 'foraging.canopy', 'foraging.aerial', 'foraging.pelagic')

traits.rom <- data.frame(traits.rom, foraging)


save(traits.rom, file = 'TraitsRom.RData')

