# Department of Defense (DoD) On Base Site Investigations
# SOIL, SEDIMENT, GROUNDWATER AND SURFACE WATER
# DO NOT CONFUSE WITH PRIVATE WELL DOD DATA (off base sampling)

# 1: Importing the data ####

library(readxl)
library(dplyr)
options(scipen=999)

# Importing Data
DoD_Raw <- read_excel("X:\\Shared drives\\_CDPHE TEEO Data\\_Enviro\\PFAS\\PFAS Concentration Map May 2024 Update\\PFASMap 2024_RProject\\01_Raw_Data\\DOD On Base Sampling V.December2025.xlsx")


# 2: Cleaning the data ####


# Replace all spaces in names of variables with underscores
names(DoD_Raw) <- gsub("\\s", "_", names((DoD_Raw)))

# Get DF column names
colnames(DoD_Raw)

# Rename the dataset
DoD_Clean <- DoD_Raw


# 3: Rename and create new variables ####

# Rename variables to standardize across datasets, create variables, and remove extraneous variables
DoD_Clean1 <- DoD_Clean %>%
  rename(
    `Sample date` = Date_Max,
    Medium = Media,
    `Number of samples` = Number_Samples,
    Notes = Note,
    PFBS = PFBS_Max,
    PFOA = PFOA_Max,
    PFOS = PFOS_Max,
    PFHxS = Max_PFHxS,
    `HFPO-DA` = Max_GenX,
    PFDA = Max_PFDA,
    PFNA = Max_PFNA
  ) %>%
  mutate(
    Dataset = "Department of Defense (DoD) On Base Site Investigations",
    Program = "CDPHE's Hazardous Materials and Waste Management Division",
    Link = "https://ar.cce.af.mil/Search",
    `Sum of PFOA and PFOS` = PFOA + PFOS
  ) %>%
  select(-c(Extra_Notes, `Internal_Only-Date_Updated`, Date_Min, `Link_#1`, `PFOS+PFOA_Max`)) |> 
  mutate(Notes = paste0("PFAS results represent the maximum of multiple samples taken from the same site. ", Notes))




# 4: Reorder variables ####

# Create a character vector containing the EXACT column names in the desired order.
desired_order <- c(
  "Dataset",
  "Program",
  "Medium",
  "Site",
  "Latitude",
  "Longitude",
  "Link",
  "Notes",
  "Sample date",
  "Number of samples",
  "Units",
  "Sum of PFOA and PFOS",
  "PFOA",
  "PFOS",
  "PFHxS",
  "PFNA",
  "PFBS",
  "HFPO-DA"
)



# Reorder variables using the select function
DoD_Clean2 <- DoD_Clean1 |> select(all_of(desired_order), everything())








# 5: Assign variable types ####

glimpse(DoD_Clean2)

# Define the groups of columns based on your data dictionary
char_cols <- c("Dataset",
               "Program",
               "Medium",
               "Site",
               "Link",
               "Notes",
               "Units")

numeric_cols <- c("Latitude", "Longitude", "Number of samples")

date_cols <- c("Sample date") # Note the date format is MM/DD/YYYY

# Apply the transformations using mutate() and across()
DoD_Clean3 <- DoD_Clean2 %>%
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


# Ensure the sample date column is formatted as a date

DoD_Clean3 <- DoD_Clean3 %>%
  mutate(
    `Sample date` = as.Date(`Sample date`, format = "%m/%d/%Y")
  )

class(DoD_Clean2$`Sample date`)


# Convert all PFAS into numeric values and round to 1 decimal place

# Create vector of PFAS analytes

PFAS_Analytes <- c("Sum of PFOA and PFOS","PFOS","PFOA","PFBS","HFPO-DA","PFHxS","PFDA","PFNA")

# Reclassify all PFAS analytes as numeric variables and round to 1 decimal place
DoD_Clean4 <- DoD_Clean3 %>%
  mutate_at(vars(all_of(PFAS_Analytes)), ~ifelse(is.na(.), NA, round(as.numeric(.), 1)))

# Replacing blanks with "0" because blank representst ND
DoD_Clean5 <- DoD_Clean4 %>%
  mutate_at(vars(all_of(PFAS_Analytes)), ~ ifelse(is.na(.), 0, .))


# 6: Subset the data to different mediums ####

# Subset by media
DoD_Groundwater <- DoD_Clean5  %>% filter(Medium=="Groundwater")
DoD_Soil <- DoD_Clean5 %>% filter(Medium=="Soil")
DoD_Sediment <- DoD_Clean5  %>% filter(Medium=="Sediment")
DoD_SurfaceWater <- DoD_Clean5  %>% filter(Medium=="Surface water")

# Export the data frame as a cleaned and formatted dataset.

library("writexl")
write_xlsx(DoD_Groundwater,"03_Clean_Data/Groundwater/DoD_SiteInvestigations_Groundwater_2025.xlsx")

write_xlsx(DoD_Sediment,"03_Clean_Data/Sediment/DoD_SiteInvestigations_Sediment_2025.xlsx")

write_xlsx(DoD_Soil,"03_Clean_Data/Soil/DoD_SiteInvestigations_Soil_2025.xlsx")

write_xlsx(DoD_SurfaceWater,"03_Clean_Data/SurfaceWater/DoD_SiteInvestigations_SurfaceWater_2025.xlsx")


