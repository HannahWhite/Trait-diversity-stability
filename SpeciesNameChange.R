############################################################
### Change synonyms and sub-species in bird species list ###
############################################################

### Hannah White 04.05.2021

## This will match species in the species x trait matrix created in CreateSpeciesTraitMatrix.R

### species in Romania
birds <- read.csv('BirdSpecies.csv', header = TRUE) # bird species list

birds$species <- gsub('Columba_livia_domest', 'Columba_livia', birds$species)
birds$species <- gsub('Buteo_buteo_vulpinus', 'Buteo_buteo', birds$species)
birds$species <- gsub('Dendrocopos__leucotos', 'Dendrocopos_leucotos', birds$species)
birds$species <- gsub('Dendrocopus_medius', 'Dendrocopos_medius', birds$species)
birds$species <- gsub('Delichon_urbica', 'Delichon_urbicum', birds$species)
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


### Remember to combine Buteo buteo records in data