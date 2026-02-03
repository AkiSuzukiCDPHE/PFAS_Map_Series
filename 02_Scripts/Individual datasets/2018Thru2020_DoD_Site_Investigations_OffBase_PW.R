# Department of Defense Site Investigations (off base private wells)
# Private wells


# 1: Importing the data ####

library(readxl)
library(dplyr)

# Importing data
OffBaseDoD_Data <- read_excel("X:/Shared drives/_CDPHE TEEO Data/_Enviro/PFAS/PFAS Concentration Map May 2024 Update/PFASMap 2024_RProject/01_Raw_Data/Private well data/OFF_Base_Sampling_True_Locations.xlsx")

# 2: Cleaning the data ####

# Replace all spaces in names of variables with underscores
names(OffBaseDoD_Data) <- gsub("\\s", "_", names((OffBaseDoD_Data)))


# Filtering to remove duplicates of the same sampling date and sample ID.
OffBaseDoD_Data <- OffBaseDoD_Data %>%
  group_by(Sample_ID, Date) %>%
  distinct() %>%
  ungroup()


class(OffBaseDoD_Data$Sum_PFOS_PFOA)


str(OffBaseDoD_Data)


# 3: Rename and create new variables ####

# Rename variables to standardize across datasets, create variables, and remove extraneous variables
OffBaseDoD_Data1 <- OffBaseDoD_Data %>%
  rename(
    Site = Name_OR_Site,
    `Sample date` = Date,
    `Sum of PFOA and PFOS` = Sum_PFOS_PFOA,
    `Sample ID` = Sample_ID
  ) %>%
  mutate(
    Link = "https://ar.afcec-cloud.af.mil/",
    Dataset = "Department of Defense Site Investigations (off base private wells)",
    Program = "CDPHE's Hazardous Materials and Waste Management Division",
    Medium = "Private well",
    `Number of samples` = 1,
    Notes = "Confidential private well samples"
  ) %>%
  select(-c(Data_Source, Type, Confidential))




# 4: Reorder variables ####

# Create a character vector containing the EXACT column names in the desired order.
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
  "Sample date",
  "Number of samples",
  "Units",
  "Sum of PFOA and PFOS"
)



# Reorder variables using the select function
OffBaseDoD_Data2 <- OffBaseDoD_Data1|> select(all_of(desired_order), everything())


# 5: Assign variable types ####


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
OffBaseDoD_Data3 <- OffBaseDoD_Data2 %>%
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

OffBaseDoD_Data3 <- OffBaseDoD_Data3 %>%
  mutate(
    `Sample date` = as.Date(`Sample date`, format = "%m/%d/%Y")
  )

class(OffBaseDoD_Data3$`Sample date`)


# Ensure that the sum pfoa and pfos variable is numeric and round to one place

# Make sure result is numeric before transforming
OffBaseDoD_Data3$`Sum of PFOA and PFOS` <- as.numeric(OffBaseDoD_Data3$`Sum of PFOA and PFOS`)


# Round results column to 1 decimal place
OffBaseDoD_Data3$`Sum of PFOA and PFOS`<- round(OffBaseDoD_Data3$`Sum of PFOA and PFOS`, digits = 1)


OffBaseDoD_Final <- OffBaseDoD_Data3


# Export the data frame as a cleaned and formatted dataset.
library("writexl")
write_xlsx(OffBaseDoD_Final,"03_Clean_Data/PrivateWell/DoD_OffBase_PrivateWells_2018Thru2020.xlsx")




