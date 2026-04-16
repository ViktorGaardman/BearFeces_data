library('tidyverse')

df <- read.csv("combined_dataset_bearfeces.csv")

df <- df %>%
  mutate(across(c("Sample", "Bear", "Species"), as.factor))

#Calculate species richness and abundance per sample
df_richness <- df %>%
  group_by(Sample) %>%
  summarize(
  sp_richness = n_distinct(Species),
  abundance = sum(Count)
    )

#Quickplot

ggplot(df_richness, aes(x = Sample, y = abundance)) +
  geom_point()
