# Background process ------------------------------------------------------
library(jsonlite)
library(openxlsx)
library(dplyr)

fieldwork_json = fromJSON("https://v2-api.npolar.no/biology/fielddata/?page=..&includeData=true")
lab_json = fromJSON("https://v2-api.npolar.no/biology/fielddata/_all_/ecotox/?page=..&includeData=true")
fieldwork_df = fieldwork_json$items$data
field_df_flat = flatten(fieldwork_df, recursive = TRUE)
lab_df = lab_json$items$data
lab_df_flat = flatten(lab_df, recursive = TRUE)
lab_df_flat <- lab_df_flat %>% rename(fieldNumberLab = fieldNumber)
lab_df_flat <- lab_df_flat %>% rename(rightsholderLab = rightsholder)
lab_df_flat <- lab_df_flat %>% rename(scientificNameLab = scientificName)
lab_df_flat <- lab_df_flat %>% rename(lifestageLab = lifestage)
lab_df_flat <- lab_df_flat %>% rename(sexLab = sex)
lab_df_flat <- lab_df_flat %>% rename(dynamicProperties.matrixLab = dynamicProperties.matrix)
lab_df_flat <- lab_df_flat %>% rename(dynamicProperties.responsibleLab = dynamicProperties.responsible)
fieldLab_df <- merge(field_df_flat, lab_df_flat, by = "eventID")

# Simple format -----------------------------------------------------------
UrsMar <- fieldLab_df[fieldLab_df$scientificName == "Ursus maritimus",]
UrsMar <- UrsMar[complete.cases(UrsMar[ , "eventID"]), ]

oldCOLname_simple <- c("fieldNumber", "scientificName",
                       "eventDate", "locality",
                       "dynamicProperties.matrix", "measurementUnit", "dynamicProperties.fatPercentage",
                       "dynamicProperties.measurementCategory", "measurementType", "measurementValue")
newCOLname_simple <- c("ID.field", "species",
                       "date_field", "locality",
                       "matrix", "unit", "EOM.%",
                       "group", "compound", "concentration")
UrsMar_long_simple <- UrsMar %>% select(8, 13,
                                        6, 10,
                                        19, 46, 54,
                                        56, 45, 47) %>% rename_with(~ newCOLname_simple, all_of(oldCOLname_simple))

write.xlsx(UrsMar_long_simple, file = "UrsMar - All data - simple.xlsx")


# Extended field format ---------------------------------------------------
UrsMar <- fieldLab_df[fieldLab_df$scientificName == "Ursus maritimus",]
UrsMar <- UrsMar[complete.cases(UrsMar[ , "eventID"]), ]

oldCOLname_extF <- c("fieldNumber", "scientificName",
                     "eventDate", "locality", "decimalLatitude", "decimalLongitude",
                     "lifestage", "dynamicProperties.age", "sex", "dynamicProperties.weightInGrams",
                     "rightsholder", "dynamicProperties.responsible", "samplingProtocol",
                     "dynamicProperties.matrix", "measurementUnit", "dynamicProperties.fatPercentage",
                     "dynamicProperties.measurementCategory", "measurementType", "measurementValue")
newCOLname_extF <- c("ID.field", "species",
                     "date_field", "locality", "latitude", "longitude",
                     "maturity", "age", "sex", "mass.gram",
                     "rightsholder_field", "responsible_field", "protocol_field",
                     "matrix", "unit", "EOM.%",
                     "group", "compound", "concentration")
UrsMar_long_extF <- UrsMar %>% select(8, 13,
                                      6, 10, 4, 5, 
                                      15, 18, 17, 27,
                                      12, 21, 16,
                                      19, 46, 54,
                                      56, 45, 47) %>% rename_with(~ newCOLname_extF, all_of(oldCOLname_extF))

write.xlsx(UrsMar_long_extF, file = "UrsMar - All data - extended field.xlsx")

# Extended lab format -----------------------------------------------------
UrsMar <- fieldLab_df[fieldLab_df$scientificName == "Ursus maritimus",]
UrsMar <- UrsMar[complete.cases(UrsMar[ , "eventID"]), ]

oldCOLname_extL <- c("fieldNumber", "scientificName",
                     "eventDate", "locality",
                     "dynamicProperties.matrix", "measurementUnit", "dynamicProperties.fatPercentage",
                     "dynamicProperties.measurementCategory", "measurementType", "measurementValue",
                     "rightsholderLab", "measurementDeterminedBy", "dynamicProperties.responsibleLab", "measurementDeterminedDate",
                     "dynamicProperties.detectionLimit", "dynamicProperties.levelOfQuantification", "dynamicProperties.percentRecovery")
newCOLname_extL <- c("ID.field", "species",
                     "date_field", "locality",
                     "matrix", "unit", "EOM.%",
                     "group", "compound", "concentration",
                     "rightsholder_lab", "lab", "responsible_lab", "date_lab",
                     "LOD", "LOQ", "recovery")
UrsMar_long_extL <- UrsMar %>% select(8, 13,
                                      6, 10,
                                      19, 46, 54,
                                      56, 45, 47,
                                      49, 41, 58, 42,
                                      53, 61, 60) %>% rename_with(~ newCOLname_extL, all_of(oldCOLname_extL))

write.xlsx(UrsMar_long_extL, file = "UrsMar - All data - extended lab.xlsx")


# Extended field and lab format -------------------------------------------
UrsMar <- fieldLab_df[fieldLab_df$scientificName == "Ursus maritimus",]
UrsMar <- UrsMar[complete.cases(UrsMar[ , "eventID"]), ]

oldCOLname_extFL <- c("fieldNumber", "scientificName",
                      "eventDate", "locality", "decimalLatitude", "decimalLongitude",
                      "lifestage", "dynamicProperties.age", "sex", "dynamicProperties.weightInGrams",
                      "rightsholder", "dynamicProperties.responsible", "samplingProtocol",
                      "dynamicProperties.matrix", "measurementUnit", "dynamicProperties.fatPercentage",
                      "dynamicProperties.measurementCategory", "measurementType", "measurementValue",
                      "rightsholderLab", "measurementDeterminedBy", "dynamicProperties.responsibleLab", "measurementDeterminedDate",
                      "dynamicProperties.detectionLimit", "dynamicProperties.levelOfQuantification", "dynamicProperties.percentRecovery")
newCOLname_extFL <- c("ID.field", "species",
                      "date_field", "locality", "latitude", "longitude",
                      "maturity", "age", "sex", "mass.gram",
                      "rightsholder_field", "responsible_field", "protocol_field",
                      "matrix", "unit", "EOM.%",
                      "group", "compound", "concentration",
                      "rightsholder_lab", "lab", "responsible_lab", "date_lab",
                      "LOD", "LOQ", "recovery")
UrsMar_long_extFL <- UrsMar %>% select(8, 13,
                                       6, 10, 4, 5, 
                                       15, 18, 17, 27,
                                       12, 21, 16,
                                       19, 46, 54,
                                       56, 45, 47,
                                       49, 41, 58, 42,
                                       53, 61, 60) %>% rename_with(~ newCOLname_extFL, all_of(oldCOLname_extFL))

write.xlsx(UrsMar_long_extFL, file = "UrsMar - All data - extended field and lab.xlsx")
