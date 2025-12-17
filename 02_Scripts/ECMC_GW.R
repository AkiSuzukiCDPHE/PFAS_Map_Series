# ECMC Analytical Samples
# Groundwater



#### Section 1 - Importing the data ####

# Importing data from an excel workbook
library(readxl)
library(dplyr)

# Get the original file path
getwd()

# Importing data
ECMC_Results <- read_excel("01_Raw_Data/Ongoing_Data_Collection/2025_ECMC_Groundwater_122511.xlsx")



#### Section 2 - Cleaning the data ####

# # Identifying the NA values for lat/longs
# NAs <- ECMC_Merged2 |> filter(is.na(Latitude83))
# # Idrntifying the unique facility ID's with missing lat longs - 286 found
# FacilityIDs <- as.data.frame(unique(NAs$FacilityID))
#

str((ECMC_Results))
# Filtering out various values

ECMC_Results1 <- ECMC_Results |> filter(
  ParamDescription %in% c(
    "Perfluoroundecanoic acid",
    "Perfluorotridecanoic acid",
    "Perfluorotetradecanoic acid",
    "Perfluoropentanoic acid",
    "Perfluoropentanesulfonic acid",
    "Perfluorooctanoic acid",
    "Perfluorooctanesulfonic acid",
    "Perfluorooctanesulfonamide",
    "Perfluorononanoic acid",
    "Perfluorononanesulfonic acid",
    "Perfluorohexanoic acid",
    "Perfluorohexanesulfonic acid",
    "Perfluoroheptanoic Acid",
    "Perfluoroheptane sulfonate",
    "Perfluorododecanoic acid",
    "Perfluorodecanoic acid",
    "Perfluorodecanesulfonic acid",
    "Perfluorobutanoic acid",
    "Perfluorobutanesulfonic acid"
  )
)


# Removes values where result is NA
ECMC_Results2 <- ECMC_Results1 |> filter(!is.na(ResultValue))


# Making sure there are not duplicates
ECMC_Results3 <- ECMC_Results2 %>%
  group_by(SampleID, ParamDescription, `Sample Date`) %>%
  distinct() %>%
  ungroup() # Ungroup if you don't need the grouping for subsequent operations


# Replacing non-detects with zero and rounding to 1 decimal place
ECMC_Results4 <- ECMC_Results3 |> mutate(ResultValue = (if_else(
  ResultValue == DetectionLimit, 0, ResultValue
))) |> mutate(ResultValue = round(as.numeric(ResultValue), 1))


# Adding a new column for PFAS analyte
# Adding a new column for PFAS analyte
ECMC_Results5 <- ECMC_Results4 |> 
  mutate(ParamAbbreviation = case_when(
    ParamDescription == "Perfluoroundecanoic acid" ~ "PFUDA",
    ParamDescription == "Perfluorotridecanoic acid" ~ "PFTrDA",
    ParamDescription == "Perfluorotetradecanoic acid" ~ "PFTDA",
    ParamDescription == "Perfluoropentanoic acid" ~ "PFPeA",
    ParamDescription == "Perfluoropentanesulfonic acid" ~ "PFPeS",
    ParamDescription == "Perfluorooctanoic acid" ~ "PFOA",
    ParamDescription == "Perfluorooctanesulfonic acid" ~ "PFOS",
    ParamDescription == "Perfluorooctanesulfonamide" ~ "FOSA",
    ParamDescription == "Perfluorononanoic acid" ~ "PFNA",
    ParamDescription == "Perfluorononanesulfonic acid" ~ "PFNS",
    ParamDescription == "Perfluorohexanoic acid" ~ "PFHxA",
    ParamDescription == "Perfluorohexanesulfonic acid" ~ "PFHxS",
    ParamDescription == "Perfluoroheptanoic Acid" ~ "PFHpA",
    ParamDescription == "Perfluoroheptane sulfonate" ~ "PFHpS",
    ParamDescription == "Perfluorododecanoic acid" ~ "PFDoA",
    ParamDescription == "Perfluorodecanoic acid" ~ "PFDA",
    ParamDescription == "Perfluorodecanesulfonic acid" ~ "PFDS",
    ParamDescription == "Perfluorobutanoic acid" ~ "PFBA",
    ParamDescription == "Perfluorobutanesulfonic acid" ~ "PFBS",
    # Add a final TRUE condition to handle any unmatched descriptions
    TRUE ~ ParamDescription 
  ))  |> select(-ParamDescription)



# Transpose the data from long to wide ####

library(tidyr)
library(dplyr)


# Transpose the aggregated data into wide format. 

# You should remove any column from id_cols that is specific to the result for a specific analyte
#(like detection limit, qualifier) and only keep the columns that define the unique sample.
# #check for val_result=NA
ECMC_Results_Wide <- ECMC_Results5 %>%
  pivot_wider(
    id_cols = c(
      "FacilityID",
      "Latitude83",
      "Longitude83",
      "Sample Date",
      "SampleID",
      "Matrix",
      "Sample Reason",
      "Units"
    ),
    names_from = ParamAbbreviation,
    values_from = ResultValue
  )



# 3: Rename and create new variables ####

# Rename variables to standardize across datasets, create variables, and remove extraneous variables
ECMC_Results_Wide1 <- ECMC_Results_Wide %>%
  rename(
    `Sample date` = `Sample Date`,
    Site=FacilityID,
    Latitude=Latitude83,
    Longitude=Longitude83,
    `Sample ID`=SampleID,
    Notes = `Sample Reason`
  ) %>%
  mutate(
    Dataset = "ECMC Analytical Samples",
    Program = "Colorado Energy & Carbon Management Commission",
    Link = "https://ecmc.colorado.gov/data-maps/downloadable-data-documents/water-well-download",
    `Sum of PFOA and PFOS` = PFOA + PFOS,
    Medium= "Groundwater",
    `Number of samples`= 1
    
  ) %>%
  select(-Matrix) 
 

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
  "PFBS"
)



# Reorder variables using the select function
ECMC_Results_Wide2 <- ECMC_Results_Wide1 |> select(all_of(desired_order), everything())






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
ECMC_Results_Wide3 <- ECMC_Results_Wide2 %>%
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

ECMC_Results_Wide3 <- ECMC_Results_Wide3 %>%
  mutate(
    `Sample date` = as.Date(`Sample date`, format = "%m/%d/%Y")
  )

class(ECMC_Results_Wide3$`Sample date`)



ECMC_Groundwater <- ECMC_Results_Wide3


# # # Exporting the data to look at it
library(writexl)
write_xlsx(ECMC_Groundwater, "03_Clean_Data/ECMC_Groundwater_2025.xlsx")
