##########################################################################
### Extract traits from Elton traits and create species x trait natrix ###
##########################################################################

#### Hannah White 04.05.2021

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

traits.rom$diet.plasticity <- rowSums(traits.rom[,3:12])
traits.rom$foraging.plasticity <- rowSums(traits.rom[,14:22])

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
traits.eu <- traits.eu[, c(4, 24)]
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

clutch.rom <- traits.eu[traits.eu$species %in% traits.rom$Scientific,]

traits.rom <- merge(traits.rom, clutch.rom, by.x = 'Scientific', by.y = 'species', all= TRUE)

names(traits.rom)[c(1, 25)] <- c('species', 'clutch.mean')

save(traits.rom, file = 'TraitsRom.RData')


