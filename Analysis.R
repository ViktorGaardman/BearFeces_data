library('tidyverse')

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
  group_by(Sample) %>%
  summarize(
  sp_richness = n_distinct(Species),
  abundance = sum(Count)
    )

#Quickplot

ggplot(df_richness, aes(x = Sample, y = abundance)) +
  geom_point()
