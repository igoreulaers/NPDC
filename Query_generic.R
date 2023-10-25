# Housekeeping ------------------------------------------------------------
library(jsonlite)
library(openxlsx)
library(dplyr)

# Data --------------------------------------------------------------------
fieldwork_json = fromJSON("https://v2-api.npolar.no/biology/fielddata/?page=..&includeData=true")
lab_json = fromJSON("https://v2-api.npolar.no/biology/fielddata/_all_/ecotox/?page=..&includeData=true")


# Extract database from JSON ----------------------------------------------
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

# Database properties ----------------------------------------------------------
str(fieldLab_df)

unique(fieldLab_df$dynamicProperties.measurementCategory)
unique(fieldLab_df$dynamicProperties.matrixLab )

# Database query ----------------------------------------------------------
query <- fieldLab_df[fieldLab_df$scientificName == "Larus hyperboreus" & fieldLab_df$dynamicProperties.matrixLab == "egg",]
query <- query[complete.cases(query[ , "eventID"]), ]

oldCOLname <- c("fieldNumber", "scientificName", "eventDate", "locality", "dynamicProperties.matrix", "measurementUnit", "dynamicProperties.fatPercentage", "dynamicProperties.measurementCategory", "measurementType", "measurementValue")
newCOLname <- c("ID.field", "species", "date", "locality", "matrix", "unit", "EOM", "group", "compound", "concentration")
query_long <- query %>% select(8, 13, 6, 10, 19, 46, 54, 56, 45, 47) %>% rename_with(~ newCOLname, all_of(oldCOLname))

write.xlsx(query_long, file = "Query.xlsx")
