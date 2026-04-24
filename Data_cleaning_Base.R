#1. Read datasets
rm(list=ls())

species <- read.csv("C:/Users/vikto/OneDrive/Skrivbord/Vetenskapliga studier/Björnbajs/BearFecesData_OhterInsects.csv",
                    sep = ";")

metadata <- read.csv("C:/Users/vikto/OneDrive/Skrivbord/Vetenskapliga studier/Björnbajs/BearFecesData_Dungbeetles_Metadata.csv",
                     sep = ";")

#2. Combine datasets through "Sample" (ID)

#Species data is in long format, let's get it to wide format

species_min <- species[,1:3] #Remove unused columns

species_xtab <- xtabs(Count ~ Sample + Species, data=species_min)

species_wide <- as.data.frame.matrix(species_xtab)                     

#add back the sampleID column
species_wide$Sample <- rownames(species_wide)

#Now we add it to the metadata df

combined_df <- merge(metadata, species_wide, by = "Sample", all.x = TRUE)

#3. Clean out species that do not live in dung

#Look at species names in dataset
print(colnames(combined_df[,25:141]))

#Create a vector with non-dung species names
exclude_sp <- c(
  "Apion simile",
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
)

#Drop these columns from the dataset

clean_df <- combined_df[,!colnames(combined_df) %in% exclude_sp]

#Replace all NA in species columns with 0
clean_df[,25:128][is.na(clean_df[,25:128])] <- 0

#4. Clean up the age of dung column
#Sometimes >XX
#somtimes XX_to_YY

#We start with XX_to_YY columns.
#Here we could take the integer of the average
#between XX and YY as a simple value

#Pick out the rows with "_" inside
# Extract the two numbers
nums <- do.call(
  rbind,
  strsplit(clean_df$days_old, "_to_")
) #strsplit is really cool!

# Convert to numeric
nums <- apply(nums, 2, as.numeric)

# Take row-wise mean
clean_df$days_old_avg <- round(rowMeans(nums, na.rm = TRUE))

#Now, we have only >XX numbers left to fix

gt_idx <- grepl(">", clean_df$days_old)

vals <- as.numeric(sub(">", "", clean_df$days_old[gt_idx]))

clean_df$days_old_avg[gt_idx] <- vals + 1


#5. Simplify diet into percent herbivory

clean_df$Herbivory <- rowSums(
  clean_df[, c(
    "Diet_Oats",
    "Diet_Green",
    "Diet_Blueberries",
    "Diet_Lingonberries",
    "Diet_Cloudberries",
    "Diet_Sorbus",
    "Diet_Apple"
  )],
  na.rm = TRUE
)

clean_df$HerbivoreDiet <- ifelse(clean_df$Herbivory > 50, 1, 0)

#6. Fix dates to be numeric

clean_df$date_parsed <- as.Date(clean_df$Date_2, format = "%d.%m.%y")
clean_df$year <- as.numeric(format(clean_df$date_parsed, "%Y"))
clean_df$doy  <- as.numeric(format(clean_df$date_parsed, "%j"))

write.csv(clean_df, "clean_df.csv")
