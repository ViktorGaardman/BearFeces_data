library(tidyverse)
library(vegan)

df <- read.csv("combined_dataset_bearfeces.csv")

df <- df %>%
  mutate(across(c("Sample", "Bear", "Species"), as.factor))

#Summary plots
#divide Date_2 into one column for DOY
#and another for year

df$date_parsed <- as.Date(df$Date_2, format = "%d.%m.%y")
df$year <- as.numeric(format(df$date_parsed, "%Y"))
df$doy  <- as.numeric(format(df$date_parsed, "%j"))
df$North <- as.numeric(df$North)

df$date_parsed_start <- as.Date(df$Date_1, format = "%d.%m.%y")
df$doy_start  <- as.numeric(format(df$date_parsed_start, "%j"))

df$dung_age <- df$doy - df$doy_start

df$dung_age[is.na(df$dung_age)] <- df$days_old[is.na(df$dung_age)]

df <- df %>%
  mutate(
    herbivory = rowSums(across(c(Diet_Oats, Diet_Blueberries, Diet_Apple,
                                 Diet_Green, Diet_Lingonberries, Diet_Cloudberries,
                                 Diet_Sorbus)), na.rm = TRUE),
    carnivory = rowSums(across(c(Diet_Ants, Diet_Meat, Diet_Pork, Diet_Eggs,
                                 Diet_Moose, Diet_Bees)), na.rm = TRUE)
  )

df$HerbivoreDiet <- ifelse(df$herbivory > 50, 1, 0)

df_bears <- df %>%
  filter(!is.na(Bear), Bear != "")

df_bears$Bear[df_bears$Bear == "Topi/K3"] <- "Topi"

df_bears$HerbivoreDiet <- as.factor(df_bears$HerbivoreDiet) 

data_summary <- ggplot(df_bears, aes(x = doy, y = North, color = Bear)) +
  geom_point(aes(shape = HerbivoreDiet), size = 3) +
  labs(
    x = "Day of Year",
    y = "Latitude"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    legend.text = element_text(size = 12),     
    legend.title = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line = element_line(color = "black"))

ggsave(data_summary, filename = "bear_datasum.png",
       width = 6.5, height = 5.26 , dpi = 300)

data_summary2 <- ggplot(df_bears, aes(x = days_old, y = North, color = Bear)) +
  geom_point(aes(shape = HerbivoreDiet), size = 3) +
  labs(
    x = "Age of dung",
    y = "Latitude"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    legend.text = element_text(size = 12),     
    legend.title = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line = element_line(color = "black"))

ggsave(data_summary2, filename = "bear_datasum2.png",
       width = 6.5, height = 5.26 , dpi = 300)

df_summary <- df %>%
  dplyr::group_by(doy, dung_age, North) %>%
  dplyr::summarise(n = dplyr::n_distinct(Sample))

data_summary3 <- ggplot(df_summary, aes(x = dung_age, y = North)) +
  geom_point(aes(size = n)) +
  scale_size_continuous(
    range = c(2, 12),
    breaks = c(1, 2, 3)
  ) +
  labs(
    x = "Age of dung",
    y = "Latitude",
    size = "Number of samples"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    legend.text = element_text(size = 12),     
    legend.title = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(610000, 640000))

data_summary3

ggsave(data_summary3, filename = "bear_datasum3.png",
       width = 6.5, height = 5.26 , dpi = 300)


#Calculate species richness and abundance per sample
df_richness <- df %>%
  group_by(Sample, dung_age, North, doy) %>%
  summarize(
  sp_richness = n_distinct(Species),
  abundance = sum(Count)
    )

#Quickplot

ggplot(df_richness, aes(x = doy, y = sp_richness, color = dung_age)) +
  geom_point() +
  labs(
    x = "Day of Year",
    y = "Species richness",
    color = "Age of dung"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    legend.text = element_text(size = 12),     
    legend.title = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line = element_line(color = "black"))

ggplot(df_richness, aes(x = North, y = sp_richness, color = dung_age)) +
  geom_point() +
  labs(
    x = "Latitude",
    y = "Species richness",
    color = "Age of dung"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    legend.text = element_text(size = 12),     
    legend.title = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line = element_line(color = "black"))

### Delete non-dung species

df <- df %>% 
  mutate(Species = str_trim(Species)) %>% 
  filter(!Species %in% c(
    "Apion_simile",
    "Cyphon punctipennis",
    "Deporaus betulae",
    "Epuraea sp.",
    "Glischrochilus quadripunctatus",
    "EI KUORIAISIA",
    "Malthodes fuscus",
    "Phloeonomus sjobergi",
    "Strophosoma capitatum",
    "Zyras humeralis",
    "Nicrophorus investigator",
    "Nicrophorus vespilloides",
    "Pterostichus diligens"
  ))
  
#Split doy into spring, summer, autumn ish.
#Split dung age into fresh (1-2 days), intermediate (3-5 days), old (5-8),
#very old 8+

df <- df %>%
  mutate(Season = case_when(
    doy >= 152 & doy < 196  ~ "Early_summer",   #Samples from 1st June - 15th July
    doy >= 197 & doy < 243 ~ "Late_summer", #Samples from 16th of July-1st of sep
    doy >= 244 & doy <= 284 ~ "Early_autumn", #Samples from 1st of sep-11th of October
    TRUE ~ NA_character_
  ))


##Create wide-format df

df_wide <- df %>% 
  select(Sample, North, doy, Season, dung_age, 
         Species, Count) %>% 
  pivot_wider(
    names_from = Species,
    values_from = Count,
    values_fn = sum
  )

#remove rows with no species
df_wide <- df_wide %>%
  mutate(across(5:110, ~ replace_na(.x, 0)))
df_wide$rowsum <- rowSums(df_wide[,5:109])
df_wide <- df_wide[df_wide$rowsum != 0, ]

####permanova
df_clean <- df_wide %>%
  select(-rowsum) %>% 
  filter(!is.na(dung_age),
         !is.na(North),
         !is.na(doy),
         !is.na(Season))

meta_df <- df_clean[,1:5]
species_df <- df_clean[,6:110] 

##Permanova
dist_matrix <- vegdist(species_df, method = "jaccard", binary = TRUE)

Permanova_mod <- adonis2(species_df ~ dung_age + North * Season, 
                         data=df_clean,
                         permutations=999,
                         method = "jaccard",
                         binary = TRUE)

#Check assumption of homogeneity of multivariate dispersion
anova(betadisper(dist_matrix, df_clean$dung_age))

Permanova_mod

NMDS_mod <- metaMDS(species_df, distance = "jaccard", k = 2, trymax = 1000)
# Run twice to avoid local optimum.
NMDS_mod <- metaMDS(species_df, distance = "jaccard", k = 2, trymax = 1000, 
                    previous.best = NMDS_mod)

#extract the site scores
datascores = as.data.frame(scores(NMDS_mod)$sites)  
datascores$doy = df_clean$doy
datascores$Season = df_clean$Season
datascores$North = df_clean$North
datascores$dung_age = df_clean$dung_age

#Add environmental variables
env <- envfit(NMDS_mod,
              df_clean[, c("Season", "North")],
              permutations = 999)

ef <- as.data.frame(env$vectors$arrows * sqrt(env$vectors$r))
ef$variable <- rownames(ef)

nmds_plot <- ggplot(datascores, aes(NMDS1, NMDS2, color = Season)) +
  geom_point() +
  geom_segment(data = ef,
               aes(x = 0, y = 0,
                   xend = NMDS1,
                   yend = NMDS2),
               inherit.aes = FALSE,
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_text(data = ef,
            aes(x = NMDS1, y = NMDS2, label = variable),
            inherit.aes = FALSE,
            vjust = -0.5) +
  theme_bw() +
  theme(legend.position="right",
        legend.text=element_text(size=20),
        legend.title=element_text(size=22),
        legend.direction='vertical',
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 16),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) + 
  annotate("text", x = max(datascores$NMDS1), 
           y = min(datascores$NMDS2), 
           label = paste("Stress =", round(NMDS_mod$stress, 3)), 
           hjust = 0.8, vjust = 0.5, size = 6)

ggsave(nmds_plot, filename = "nmds_plot.png", dpi = 300,
       height = 5.26, width = 6.5)

