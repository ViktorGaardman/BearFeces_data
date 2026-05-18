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

df <- read.csv("clean_bear_long.csv", sep = ",")

#Define variables

#Add information on trophic level


copro_names <- c("Aphodius", "Geotrupes", "Aploderus",
                 "Oxytelus", "Platystethus", "Proteinus",
                 "Megarthrus", "Cryptopleurum", "Megasternum", 
                 "Cercyon", "Sphaeridium", "Helophorus", "Acrotrichis",
                 "Anotylus", "Sciodrepoides", "Scaphisoma",
                 "Nargus", "Catops", "Anacaena", "Pteryx",
                 "Ptiliolum", "Baeocrara")

carni_names <- c("Paralister", "Hister", "Atholus",
                 "Sphaerites", "Acrolocha", "Omalium",
                 "Deliphrum", "Arpedium", "Stenus", "Lithocharis",
                 "Stilicus", "Lathrobium", "Gyrohypnus", "Xantholinus",
                 "Othius", "Philonthus", "Ontholestes", "Quedius",
                 "Mycetoporus", "Bolitobius", "Tachyporus", "Tachinus",
                 "Lecuoparyphus", "Placusa", "Autalia",
                 "Falagria", "Cordalia", "Amischa", "Sipalia",
                 "Atheta","Acrostiba", "Oxypoda", "Aleochara",
                 "Tinotus", "Bisnius", "Liogluta", 
                 "Euaesthetus", "Anthobium", "Anthophagus",
                 "Xylodromus", "Acrotona", "Anopleta", 
                 "Pachyatheta", "Gabrius")

df <- df %>%
  mutate(TrophicGroup = case_when(
    Genus %in% carni_names ~ "P",
    Genus %in% copro_names ~ "C",
    TRUE ~ NA_character_
  ))

unique(df$Genus[is.na(df$TrophicGroup)]) #All genera included!

#Octave analysis

#First calculate total abundance per species
df_abu <- df %>%
  group_by(Species, TrophicGroup) %>%
  summarize(
    abundance = sum(Count)
  )

#Next divide into octaves
make_octaves <- function(df_abu, abundance, TrophicGroup) {
  
  df_abu %>%
    ungroup() %>%
    mutate(
      abundance = as.numeric({{ abundance }}),
      octave = floor(log2(abundance)) + 1
    ) %>%
    filter(!is.na(octave), abundance > 0) %>%
    
    group_by({{TrophicGroup}}, octave) %>%
    
    count(name = "n_species") %>%
    
    ungroup() %>%
    
    complete(
      {{TrophicGroup}},
      octave = full_seq(octave, 1),
      fill = list(n_species = 0)
    ) %>%
    arrange({{TrophicGroup}}, octave)
}

# Ensure ungrouped
df_abu <- df_abu %>% ungroup()

octaves_bear <- make_octaves(df_abu, abundance, TrophicGroup)

#Repeat for Hanski data

Hanski_df <- read.csv("Hanski_data.csv", sep = ";")

Hanski_df$TrophicGroup <- as.factor(Hanski_df$TrophicGroup)

make_hanski_octaves <- function(Hanski_df, Abundance, TrophicGroup) {
  
  Hanski_df %>%
    ungroup() %>%
    mutate(
      abundance = as.numeric({{ Abundance }}),
      octave = floor(log2(abundance)) + 1
    ) %>%
    filter(!is.na(octave), abundance > 0) %>%
    
    group_by({{TrophicGroup}}, octave) %>%
    
    count(name = "n_species") %>%
    
    ungroup() %>%
    
    complete(
      {{TrophicGroup}},
      octave = full_seq(octave, 1),
      fill = list(n_species = 0)
    ) %>%
    arrange({{TrophicGroup}}, octave)
}

octaves_cow <- make_hanski_octaves(Hanski_df, Abundance, TrophicGroup)

bear_plot1 <- ggplot(octaves_bear, aes(x = octave, y = n_species)) +
  geom_col() +
  facet_wrap(~TrophicGroup) +
  theme_classic() +
  scale_y_continuous(limits = c(0,25)) +
  scale_x_continuous(limits = c(0,11), breaks = scales::pretty_breaks(n = 6)) +
  labs(
    title = "Bear feces",
    x = "Octave",
    y = "Species count"
  ) +
theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    title = element_text(size = 14, vjust = 0.5)
  )

bear_plot1

cow_plot1 <- ggplot(octaves_cow, aes(x = octave, y = n_species)) +
  geom_col() +
  facet_wrap(~TrophicGroup) +
  scale_x_continuous(limits = c(0,15), breaks = scales::pretty_breaks(n = 8)) +
  theme_classic() +
  labs(
    title = "Cow feces",
    x = "Octave",
    y = "Species count"
  ) +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12),
    title = element_text(size = 14, vjust = 0.5)
  )

octaveplots <- bear_plot1 + cow_plot1

octaveplots

#Using Hanski's niche width

#Calculate for all speices for coprophages, 
#but only if above 5 individuals for carnivores

#Define environmental variables
df_wide <- read.csv("df_prepped.csv", sep = ",")

species_df <- df_wide[,35:137] 

species_mat <- as.matrix(species_df)
storage.mode(species_mat) <- "numeric"

dung_age <- as.numeric(df_clean$days_old_avg)
Month <- as.numeric(df_clean$new_month)

succession <- colSums(species_mat  * dung_age) / colSums(species_mat )
Wsucc <- colSums(
  species_mat * (outer(dung_age, rep(1, ncol(species_mat))) - 
                   matrix(succession, nrow = nrow(species_mat), ncol = ncol(species_mat),
                          byrow = TRUE))^2
) / colSums(species_mat)


Season <- colSums(species_mat  * Month) / colSums(species_mat )
Wseas <- colSums(
  species_mat * (outer(Month, rep(1, ncol(species_mat))) - 
                   matrix(Season, nrow = nrow(species_mat), ncol = ncol(species_mat),
                          byrow = TRUE))^2
) / colSums(species_mat)


dung_data <- data.frame(
  species = colnames(species_df),
  succession = succession,
  Wsucc_b = Wsucc,
  Season = Season,
  Wseas_b = Wseas
)

#scale w values to 0-1 scale
scale_01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

dung_data$Wseas   <- scale_01(dung_data$Wseas_b)
dung_data$Wsucc <- scale_01(dung_data$Wsucc_b)

dung_data <- dung_data %>%
  mutate(Genus = word(species, 1, sep = "_")) %>%
  mutate(TrophicGroup = case_when(
    Genus %in% carni_names ~ "P",
    Genus %in% copro_names ~ "C",
    TRUE ~ NA_character_
  )) %>% 
  mutate(succession_int = round(succession))

bear_suc <- ggplot(dung_data, aes(x = succession_int)) +
  geom_bar() +
  facet_wrap(~TrophicGroup) +
  scale_x_continuous(limits = c(0,25), breaks = scales::pretty_breaks(n = 6)) +
  theme_classic() +
  labs(
    x = "Dung age",
    y = "Species count"
  ) +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
  )

cow_suc <- ggplot(Hanski_df, aes(x = round(Succession))) +
  geom_bar() +
  facet_wrap(~TrophicGroup) +
  scale_x_continuous(limits = c(0,25), breaks = scales::pretty_breaks(n = 6)) +
  theme_classic() +
  labs(
    x = "Dung age",
    y = "Species count"
  ) +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12),
  )

suc_plots <- bear_suc + cow_suc

suc_plots

#Niche width

#Extract mean values per succession day

Hanski_df$Succession_int <- round(Hanski_df$Succession)

niche_means <- Hanski_df %>%
  group_by(Succession_int, TrophicGroup) %>% 
  summarize(
    niche_mean = mean(Wsucc)
  ) %>% 
  na.omit()

cow_width <- ggplot(niche_means, aes(x = Succession_int, y = niche_mean)) +
  geom_point() +
  facet_wrap(~TrophicGroup) +
  theme_classic() +
  labs(
    x = "Dung age",
    y = "Species count"
  ) +
  scale_y_continuous(limits= c(0,1), n.breaks = 6) +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12)
  )

cow_width

niche_means_b <- dung_data %>%
  group_by(succession_int, TrophicGroup) %>% 
  summarize(
    niche_mean = mean(Wsucc)
  ) %>% 
  na.omit()

bear_width <- ggplot(niche_means_b, aes(x = succession_int, y = niche_mean)) +
  geom_point() +
  facet_wrap(~TrophicGroup) +
  theme_classic() +
  scale_y_continuous(limits= c(0,1), n.breaks = 6) +
  labs(
    x = "Dung age",
    y = "Mean niche width"
  ) +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

bear_width

width_plots <- bear_width + cow_width

all_plots <-  octaveplots / suc_plots / width_plots

all_plots

ggsave(all_plots, filename = "trial_plots.png", dpi = 300,
       width = 10.56, height = 13)

#Add family ID
dung_data <- dung_data %>%
  
  left_join(family_lookup, by = "Genus")

#Add abundances
total_abu <- df_long %>% 
  group_by(Species) %>% 
  summarize(
    Total = sum(Count)
  )

names(total_abu) <- c("species", "Abundance")

dung_df <-merge(dung_data, total_abu, by = "species")

#Combine with Hanski cow data

cow_df <- read.csv("Hanski_data.csv", sep = ";")








##RDA

meta_df <- df_clean[,1:33]
species_df <- df_clean[,34:136] 

x <- decostand(species_df, "hellinger")
rda_mod <- rda(x ~ days_old_avg * Season + North, data = df_clean)

#test significance
anova.cca(rda_mod) #Model is significant
anova.cca(rda_mod, by = "axis") #Axis 1-4 are significant
anova(rda_mod, by = "margin")

vif.cca(rda_mod)

summary(rda_mod)
coef(rda_mod)[c("days_old_avg:Season1", "Season1:North"), ]

#The interactions are


new_early <- subset(df_clean, Season == "Early season")
new_late  <- subset(df_clean, Season == "Late season")

early_scores <- predict(rda_mod, newdata = new_early, type = "wa")
late_scores  <- predict(rda_mod, newdata = new_late, type = "wa")

get_arrow <- function(rda_mod, term, season, data) {
  
  cf <- coef(rda_mod)
  
  # find reference level (first level of factor)
  ref_level <- levels(data$Season)[1]
  
  # base = reference level
  base <- cf[term, , drop = FALSE]
  
  # if not reference → add interaction
  if (season != ref_level) {
    
    # find correct dummy name (e.g. Season1)
    dummy_name <- paste0("Season", which(levels(data$Season) == season) - 1)
    
    inter1 <- paste0(term, ":", dummy_name)
    inter2 <- paste0(dummy_name, ":", term)
    
    if (inter1 %in% rownames(cf)) {
      base <- base + cf[inter1, , drop = FALSE]
    } else if (inter2 %in% rownames(cf)) {
      base <- base + cf[inter2, , drop = FALSE]
    }
  }
  
  as.numeric(base)
}

seasons <- c("Early season", "Late season")

arrows <- do.call(rbind, lapply(seasons, function(s) {
  
  data.frame(
    Season = s,
    Dung_RDA1 = get_arrow(rda_mod, "days_old_avg", s, df_clean)[1],
    Dung_RDA2 = get_arrow(rda_mod, "days_old_avg", s, df_clean)[2]
  )
}))


site_scores <- as.data.frame(scores(rda_mod, display = "sites"))
site_scores$Season <- df_clean$Season

ggplot(site_scores, aes(RDA1, RDA2)) +
  
  # sites
  geom_point(aes(color = Season), size = 3, alpha = 0.8) +
  
  # Dung age arrows per season
  geom_segment(data = arrows,
               aes(x = 0, y = 0,
                   xend = Dung_RDA1,
                   yend = Dung_RDA2,
                   linetype = Season),
               color = "darkgreen", linewidth = 1) +
  
  theme_minimal() +
  facet_wrap(~Season)+
  theme(panel.grid = element_blank()) +
  labs(title = "Season-specific environmental effects (from interaction RDA)") 



##No interactions model
x <- decostand(species_df, "hellinger")
rda_mod_base <- rda(x ~ days_old_avg + Season + North, data = df_clean)

#test significance
anova.cca(rda_mod_base) #Model is significant
anova.cca(rda_mod_base, by = "axis") #Axis 1-3 are significant
anova(rda_mod_base, by = "margin") #Term significance

vif.cca(rda_mod_base)

RsquareAdj(rda_mod_base)

#check if db-RDA gives different results
dbrda_mod <- capscale(species_df ~ days_old_avg + Season + North,
                      data = df_clean,
                      distance = "bray")
summary(dbrda_mod)
anova(dbrda_mod, by = "margin")


#plot
site_scores <- as.data.frame(scores(rda_mod_base, display = "sites"))
species_scores <- as.data.frame(scores(rda_mod_base, display = "species"))
env_scores <- as.data.frame(scores(rda_mod_base, display = "bp"))  # environmental arrows
env_scores <- env_scores[!rownames(env_scores) %in% grep("^Season", rownames(env_scores), value = TRUE), ]
row.names(env_scores) <- c("Dung age", "Latitude")

site_scores$Season <- df_clean$Season
site_scores$North <- df_clean$North
site_scores$days_old_avg <- df_clean$days_old_avg

#Plot only most influencal species
species_scores$species <- rownames(species_scores)

# pick most extreme species (longest vectors)
species_scores$mag <- sqrt(species_scores$RDA1^2 + species_scores$RDA2^2)

top_species <- species_scores[order(-species_scores$mag), ][1:10, ]

rda_plot <- ggplot(site_scores, aes(RDA1, RDA2)) +
  # sites
  geom_point(aes(color = Season), size = 3, alpha = 0.6) +
  scale_color_manual(values = c("#0072B2", "#009E73")) +
  # species (ONLY top ones)
  geom_segment(data = top_species,
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               color = "grey60", alpha = 0.6) +
  geom_segment(data = env_scores,
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               color = "#CC79A7", linewidth = 1) +
  geom_text_repel(data = top_species,
                  aes(label = species),
                  size = 3,
                  max.overlaps = Inf) +
  geom_text(data = env_scores,
                  aes(label = rownames(env_scores)),
                  color = "black", size = 4) +
  theme_classic() +
  labs(
    x = "RDA1 (82.3%)",
    y = "RDA2 (10.4%)"
  ) +
  theme(legend.position="right",
                legend.text=element_text(size=12),
                legend.title=element_text(size=14),
                legend.direction='vertical',
                axis.title.x = element_text(size = 14),
                axis.title.y = element_text(size = 14),
                axis.text = element_text(size = 12),
                panel.grid.minor = element_blank(), 
                panel.grid.major = element_blank())


ggsave(rda_plot, filename = "RDA_bearfeces.png", dpi = 300, height = 5.26,
       width = 7.5)

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

#Summary graphs
sum(df_richness$abundance)
n_distinct(df_long$Species)
nrow(df_richness)
range(df_richness$abundance)
range(df_richness$sp_richness)

abu_rich <- ggplot(df_richness, aes(x = abundance, y = sp_richness)) +
  geom_point() +
  theme_classic()+
  labs(
    x = "Abundance",
    y = "Species richness"
  ) +
  theme(legend.position="right",
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.direction='vertical',
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())

ggsave(abu_rich, filename = "abundanceRichness_bearfeces.png", dpi = 300, height = 5.26,
       width = 7.5)

df_sum <- df_long %>% 
  group_by(Family) %>%
  summarize(
    fam_richness = n_distinct(Species),
    fam_abundance = sum(Count)
  )
  

rich_plot <- ggplot(df_sum, aes(x = Family.y, y = fam_richness)) +
  geom_col(width = 0.5) +
  labs(
    y = "Species richness"
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position="right",
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.direction='vertical',
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12, angle = 25, hjust = 1),
        axis.text.y = element_text(size = 12),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())

abu_plot <- ggplot(df_sum, aes(x = Family.y, y = log10(fam_abundance))) +
  geom_col(width = 0.5) +
  labs(
    y = "log10(Abundance)"
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position="right",
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.direction='vertical',
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12, angle = 25, hjust = 1),
        axis.text.y = element_text(size = 12),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())

rawdata <- ggplot(df_long, aes(x = days_old_avg, y = North, color = Season)) +
  geom_point(size = 2, alpha = 0.6, aes(color = Season))+
  scale_color_manual(values = c("#0072B2", "#009E73")) +
  labs(
    x = "Dung age",
    y ="Latitude"
  ) +
  theme_classic() +
  theme(legend.position= c(0.8, 0.8),
        legend.text=element_text(size=12),
        legend.title=element_blank(),
        legend.direction='vertical',
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())
 
rawdata

Fig1 <- (rawdata | abu_rich) / (abu_plot | rich_plot)+ 
  plot_annotation(tag_levels = "A")

Fig1 + 
  plot_annotation(tag_levels = "A")

ggsave(Fig1, filename = "Fig1.png", dpi = 300,
       width = 6.5, height = 5.26)

#GLM

glm_rich <- glm.nb(
  sp_richness ~  Season + North + days_old_avg,
  data = df_richness)

Anova(glm_rich, type = c('3'))

deviance(glm_rich) / df.residual(glm_rich) 
sim_res <- simulateResiduals(glm_rich)
plot(sim_res)
testZeroInflation(sim_res)
#Modle is great!

#predict
pred <- ggpredict(glm_rich, terms = c("Season"))
pred <- as.data.frame(ggpredict(glm_rich, terms = "Season"))

richness_plot <- ggplot(df_richness, aes(x = Season, y = sp_richness)) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 3, aes(color = Season)) +
  geom_point(data = pred, aes(y = predicted, x = x), color = "black", size = 6,
             inherit.aes = FALSE) +
  labs(y = "Species richness") +
  theme_classic() +
  scale_color_manual(values = c("#0072B2", "#009E73")) +
  geom_errorbar(
    data = pred,
    aes(ymin = conf.low, ymax = conf.high, x = x),
    width = 0.2,
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  theme(legend.position="none",
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.direction='vertical',
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())
  
richness_plot

ggsave(richness_plot, filename ="richness_plot.png", 
       dpi =300, width = 6, height = 5)

glm_abu <- glm.nb(
  abundance ~  North + Season  + days_old_avg,
  data = df_richness)

Anova(glm_abu, type = c('3'))

deviance(glm_abu) / df.residual(glm_abu) 
sim_res <- simulateResiduals(glm_abu)
plot(sim_res)
testZeroInflation(sim_res)
#Model is decent. Nothing influences abundances


####Comparison to Hanski data
