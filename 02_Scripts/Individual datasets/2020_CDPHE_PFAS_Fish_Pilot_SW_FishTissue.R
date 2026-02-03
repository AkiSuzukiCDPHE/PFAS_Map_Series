# Corrective Action Site Screening
# GROUNDWATER AND LEACHATE

# 1: Importing the data ####

library(readxl)
library(dplyr)

# Importing Data
FCA_Pilot <- read_excel("01_Raw_Data/One_Time_Efforts/2020_CDPHE_PFAS_Fish_Pilot.xlsx")


# 2: Assign variable types ####

glimpse(FCA_Pilot)

# Define the groups of columns based on your data dictionary
char_cols <- c("Dataset",
               "Program",
               "Medium",
               "Site",
               "Link",
               "Species",
               "Notes",
               "Units")

numeric_cols <- c("Latitude", "Longitude")

date_cols <- c("Sample date") # Note the date format is MM/DD/YYYY

# Apply the transformations using mutate() and across()
FCA_Pilot1 <- FCA_Pilot %>%
  # 1. Convert Character Columns
  mutate(across(.cols = all_of(char_cols), .fns = as.character)) %>%
  
  # 2. Convert Numeric Columns
  mutate(across(.cols = all_of(numeric_cols), .fns = as.numeric)) %>%
  
  # 3. Convert Date Columns
  mutate(across(
    .cols = all_of(date_cols),
    # Specify the format: %m = Month, %d = Day, %Y = 4-digit Year
    .fns = ~ as.Date(., format = "%m/%d/%Y")
  ))


class(FCA_Pilot1$`Sample date`)


# 3: Reorder variables ####


# Create vector wth order of columns
desired_order <- c(
  "Dataset",
  "Program",
  "Medium",
  "Site",
  "Address",
  "Latitude",
  "Longitude",
  "Link",
  "Notes",
  "Species",
  "Sample date",
  "Number of samples",
  "Units",
  "Sum of PFOA and PFOS",
  "PFOA",
  "PFOS"
)


# Reorder variables using the select function
FCA_Pilot_FishTissue_2020 <- FCA_Pilot1 |> select(all_of(desired_order), everything())


# 4: Export ####

library("writexl")
write_xlsx(FCA_Pilot_FishTissue_2020,"03_Clean_Data/FishTissue/FCA_Pilot_FishTissue_2020.xlsx")



