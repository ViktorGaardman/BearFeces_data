library('tidyverse')

metadata <- read.csv("BearFecesData_Dungbeetles_Metadata.csv", sep = ";")

beetledata <- read.csv("BearFecesData_OhterInsects.csv", sep = ";")

#1. Data cleaning

#Define factors, dates, and numbers
metadata$days_old <- as.numeric(metadata$days_old)

metadata <- metadata %>%
mutate(across(c("Sample", "Bear"), as.factor))

beetledata$Sample <- as.factor(beetledata$Sample)

beetledata <- beetledata[,1:4]

#Turn all species columns into long format
long_metadata <- metadata %>%
  pivot_longer(
    cols = c(G._stercorosus, A._fimetarius, A._rufipes,
              A._depressus, A._fasciatus, A._piceus),
    names_to = "Species",
    values_to = "Count"
  ) %>%
  filter(!is.na(Count))

#Extract metadata
metadata_only <- long_metadata %>%
  select(-c(Species, Count)) %>%  # adjust metadata columns
  distinct()

#Attach metadata to beetledata
beetle_with_meta <- beetledata %>%
  left_join(metadata_only, by = "Sample")

#Combine with remaining dungbeetle data
full_df <- bind_rows(long_metadata, beetle_with_meta)

write_csv(full_df, "combined_dataset_bearfeces.csv")
