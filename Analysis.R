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

#keep only columns with species occurences

df_clean <- df_clean[, colSums(df_clean[,26:129]) > 0]

meta_df <- df_clean[,c(1:24,128:135)]
species_df <- df_clean[,25:127] 


##RDA

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
    cols = -c(1:25,130:136),
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
  mutate(Genus = word(Species, 1, sep = "\\.")) %>%
  left_join(family_lookup, by = "Genus")

df_sum <- df_long %>% 
  group_by(Family.y) %>%
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


##Permanova (add random effect of year)
dist_matrix <- vegdist(species_df, method = "bray")

Permanova_mod <- adonis2(species_df ~ days_old_avg * Season + Season * North, 
                         data=df_clean,
                         permutations=999,
                         method = "bray",
                         #         binary = TRUE,
                         by = "margin")


Permanova_mod

#Check assumption of homogeneity of multivariate dispersion
anova(betadisper(dist_matrix, df_clean$days_old_avg))

NMDS_mod <- metaMDS(species_df, distance = "bray", k = 2, trymax = 5000)
# Run twice to avoid local optimum.
NMDS_mod <- metaMDS(species_df, distance = "bray", k = 2, trymax = 1000, 
                    previous.best = NMDS_mod)

#extract the site scores
datascores = as.data.frame(scores(NMDS_mod)$sites)  
datascores$doy = df_clean$doy
datascores$Season = df_clean$Season
datascores$North = df_clean$North
datascores$days_old_avg = df_clean$days_old_avg

#Add environmental variables
env <- envfit(NMDS_mod,
              df_clean[, c("North", "days_old_avg")],
              permutations = 999)

ef <- as.data.frame(env$vectors$arrows * sqrt(env$vectors$r))
ef$variable <- rownames(ef)

datascores <- datascores %>%
  mutate(Season = fct_recode(Season,
                             "Late summer" = "Late_summer",
                             "Early summer" = "Early_summer",
                             "Early autumn" = "Early_autumn"
  ))


datascores <- datascores %>%
  mutate(Season = fct_relevel(Season,
                              "Early summer", "Late summer", "Early autumn"
  ))

nmds_plot <- ggplot(datascores, aes(NMDS1, NMDS2, color = Season)) +
  geom_point(size = 3) +
  geom_segment(data = ef,
               aes(x = 0, y = 0,
                   xend = NMDS1,
                   yend = NMDS2),
               inherit.aes = FALSE,
               arrow = arrow(length = unit(0.4, "cm")),
               linewidth = 1.5) +
  geom_text(data = ef,
            aes(x = NMDS1, y = NMDS2, label = variable),
            inherit.aes = FALSE,
            vjust = -0.5,
            size = 6) +
  scale_color_manual(values = c("#0072B2", "#CC79A7", "#D55E00")) +
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

nmds_plot

ggsave(nmds_plot, filename = "nmds_plot.png", dpi = 300,
       height = 5.26, width = 6.5)

ggplot(df_clean, aes(x = dung_age_avg, y = North, color = Season)) +
  geom_point()



####Comparison to Hanski data
#Define environmental variables

species_mat <- as.matrix(species_df)
storage.mode(species_mat) <- "numeric"

dung_age <- as.numeric(df_clean$days_old_avg)
Month <- as.numeric(df_clean$new_month)

mu_age <- colSums(species_mat  * dung_age) / colSums(species_mat )
Wsu_age <- colSums(
  species_mat * (outer(dung_age, rep(1, ncol(species_mat))) - 
                   matrix(mu_age, nrow = nrow(species_mat), ncol = ncol(species_mat),
                          byrow = TRUE))^2
) / colSums(species_mat)


mu_month <- colSums(species_mat  * Month) / colSums(species_mat )
Wsu_month <- colSums(
  species_mat * (outer(Month, rep(1, ncol(species_mat))) - 
                   matrix(mu_month, nrow = nrow(species_mat), ncol = ncol(species_mat),
                          byrow = TRUE))^2
) / colSums(species_mat)

#cleaner wsu (chekc if it gives the same results)
Wsu_age2 <- apply(species_mat, 2, function(x) {
  m <- sum(x * dung_age) / sum(x)
  sum(x * (dung_age - m)^2) / sum(x)
})

res <-data.frame(
  species = colnames(species_df),
  Wsu_age = Wsu_age,
  wsu_age2 = Wsu_age2)

result <- data.frame(
  species = colnames(species_df),
  mu_age = mu_age,
  Wsu_age = Wsu_age,
  mu_month = mu_month,
  Wsu_month = Wsu_month
)

#Norwegian version

#Combine with Hanski data


