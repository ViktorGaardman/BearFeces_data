library(tidyverse)
library(vegan)
library(ggrepel)
library(car)
library(MASS)
library(DHARMa)
library(ggeffects)
library(patchwork)

rm(list=ls())

options(contrasts = c("contr.sum", "contr.poly"))

set.seed(123)

df <- read.csv("clean_df.csv", sep = ";")

df <- df %>%
  mutate(across(c("Sample", "Bear"), as.factor))

df <- df %>%
  mutate(Season = case_when(
    doy >= 137 & doy < 237  ~ "Early season",   #Samples from 1st June - 31st July
    #    doy >= 197 & doy < 243 ~ "Late_summer", #Samples from 16th of July-1st of sep
    doy >= 237 & doy <= 284 ~ "Late season", #Samples from 1st of sep-11th of October
    TRUE ~ NA_character_
  ))

#Make months numeric
df$new_month <- as.integer(format(as.Date(df$date), "%m")) - 4

#remove rows with no species
df <- df %>%
  mutate(across(26:129, ~ replace_na(.x, 0)))
df$rowsum <- rowSums(df[,26:129])
df <- df[df$rowsum != 0, ]

df$North <- as.numeric(df$North)
df$Season <- as.factor(df$Season)
df$doy <- as.numeric(df$doy)

df_clean <- df %>%
  dplyr::select(-rowsum) %>% 
  filter(!is.na(days_old_avg),
         !is.na(North),
         !is.na(doy),
         !is.na(Season))

#keep only columns with species occurrences

keep <- colSums(df_clean[, 26:129]) > 0

df_clean <- df_clean[, c(c(1:25, 130:137), which(keep) + 25)]

write.csv(df_clean, "df_prepped.csv")

#Calculate species richness and abundance per sample
df_long <-  df_clean %>% 
  pivot_longer(
    cols = -c(1:33),
    names_to = "Species",
    values_to = "Count"
  ) %>% 
  filter(!Count %in% "0")

df_richness <- df_long %>%
  group_by(Sample, days_old_avg, North, Season) %>%
  summarize(
    sp_richness = n_distinct(Species),
    abundance = sum(Count)
  )

family_lookup <- tibble(
  Genus = c("Geotrupes", "Sciodrepoides", "Catops", "Nargus", "Aphodius", "Sphaeridium",
            "Megasternum",  "Anacaena", 
            "Cercyon", "Cryptopleurum", "Pteryx", "Ptiliolum", "Baeocrara", "Acrotrichis", "Atheta", "Acrotona", "Aleochara",
            "Omalium", "Anopleta", "Anotylus", "Anthobium", "Anthophagus", "Autalia",
            "Bisnius", "Deliphrum", "Euaesthetus", "Gabrius", "Gyrohypnus",
            "Liogluta", "Megarthrus", "Oxypoda", "Oxytelus", "Pachyatheta",
            "Philonthus", "Platystethus", "Proteinus", "Scaphisoma",
            "Tachinus", "Xylodromus"
  ),
  Family = c("Geotrupidae", "Leiodidae", "Leiodidae", "Leiodidae", "Scarabaeidae", "Hydrophilidae",
             "Hydrophilidae", "Hydrophilidae", "Hydrophilidae", "Hydrophilidae",
             "Ptilidae", "Ptilidae", "Ptilidae",
             "Ptilidae", "Staphylinidae", "Staphylinidae", "Staphylinidae",
             "Staphylinidae", "Staphylinidae",  "Staphylinidae", "Staphylinidae", "Staphylinidae",
             "Staphylinidae", "Staphylinidae",  "Staphylinidae", "Staphylinidae", "Staphylinidae",
             "Staphylinidae", "Staphylinidae",  "Staphylinidae", "Staphylinidae", "Staphylinidae",
             "Staphylinidae", "Staphylinidae",  "Staphylinidae", "Staphylinidae", "Staphylinidae",
             "Staphylinidae", "Staphylinidae")
)

df_long <- df_long %>%
  mutate(Genus = word(Species, 1, sep = "\\_")) %>%
  left_join(family_lookup, by = "Genus")

write.csv(df_long, "clean_bear_long.csv")
