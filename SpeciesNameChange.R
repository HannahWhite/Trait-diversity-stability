############################################################
### Change synonyms and sub-species in bird species list ###
############################################################

### Hannah White 04.05.2021

## This will match species in European Bird Trait Database - different for Elton Traits

### species in Romania
birds <- read.csv('BirdSpecies.csv', header = TRUE) # bird species list

birds$species <- gsub('Columba_livia_domest', 'Columba_livia', birds$species)
birds$species <- gsub('Buteo_buteo_vulpinus', 'Buteo_buteo', birds$species)
birds$species <- gsub('Dendrocopos__leucotos', 'Dendrocopos_leucotos', birds$species)
birds$species <- gsub('Dendrocopus_medius', 'Leiopicus_medius', birds$species)
birds$species <- gsub('Dendrocopos_minor', 'Dryobates_minor', birds$species)
birds$species <- gsub('Anas_strepera', 'Mareca_strepera', birds$species)
birds$species <- gsub('Delichon_urbica', 'Delichon_urbicum', birds$species)
birds$species <- gsub('Aquila_pomarina', 'Clanga_pomarina', birds$species)
birds$species <- gsub('Carduelis_cannabina', 'Linaria_cannabina', birds$species)
birds$species <- gsub('Ardea_Purpurea', 'Ardea_purpurea', birds$species)
birds$species <- gsub('Columba_palumbas', 'Columba_palumbus', birds$species)

### Remember to combine Buteo buteo records in data