# School of Mines - 2024 PFAS soil data from septic system and ski investigation project



# School of mines soil dataset from 2024 study


# 1: Importing the data ####

library(readxl)
library(dplyr)

getwd()


# Sub this file and file path out for new data each time
Mines_Soil_2024 <- read_excel("01_Raw_Data/One_Time_Efforts/2024_SchoolofMines_Soil.xlsx")
# col_types = cols(
#   `Sample Date` = col_date(format = "%m/%d/%Y")))


# 2: Misc. Cleaning ####


# Replace all spaces in names of variables with underscores
names(Mines_Soil_2024) <- gsub("\\s", "_", names((Mines_Soil_2024)))

glimpse(Mines_Soil_2024)


# Converting latitude and longitude to numeric variables
Mines_Soil_2024_1 <- Mines_Soil_2024 %>%
  mutate(across(Latitude:Longitude, .fns = as.numeric))


# Replace non-detects with zero and turn result into a numeric column
Mines_Soil_2024_2 <- Mines_Soil_2024_1 |> mutate(Result=if_else(grepl("<|ND", Result), "0.0", Result)) |>  mutate(Result=round(as.numeric(Result, digits=1)))

unique(Mines_Soil_2024_2$Result)

str(Mines_Soil_2024_2)

# 3: Rename and create new variables ####

# Rename variables to standardize across datasets, create variables, and remove extraneous variables
Mines_Soil_2024_3 <- Mines_Soil_2024_2 %>%
  rename(
    `Sample date` = Date_Collected,
    `Sample ID` = Sample_ID,
    Address = Sample_Location_Street_Address
  ) %>%
  mutate(
    Dataset = "2024 School of Mines - AFFF, ski wax, and septic system study",
    Link = "https://cdphe.colorado.gov/pfas-per-and-polyfluoroalkyl-substances/pfas-grant-program/pfas-grant-summaries",
    `Number of samples` = 1,
    Notes = Facility_Type,
    Program = "CDPHE's Water Quality Control Division (WQCD)",
    # coalesce takes the first value that isn't missing
    `Site ID` = Facility_Name,
    Site = `Site_ID___(optional)`,
    Units = `Units___(ng/L)`
  ) |>  select (
    c(
      Dataset,
      Program,
      Site,
      `Site ID`,
      Address,
      Latitude,
      Longitude,
      Link,
      `Sample date`,
      `Sample ID`,
      `Number of samples`,
      `Units`,
      Component,
      Result,
      Facility_Type,
      Source_Water_Type
    )
  ) |> 
  mutate(Units = "ug/kg")





# Create variable for medium and remove chlorinated PFAS
Mines_Soil_2024_4 <-  Mines_Soil_2024_3 |>  mutate(Medium = "Soil") |> filter(!grepl("Cl-", Component)) |> select(-c(Facility_Type, Source_Water_Type))


# 4: Transpose ####

# Filter data so there is one unique row for every combination
Mines_Soil_2024_5 <- Mines_Soil_2024_4 %>%
  distinct(Site,
           Medium,
           `Site ID`,
           `Sample date`,
           `Sample ID`,
           Latitude,
           Longitude,
           Component,
           .keep_all = TRUE)


str(Mines_Soil_2024_5)



# Transpose the data from long to wide format:
# Id_Cols = specify the column(s) that you want to keep as identifiers (i.e., columns that remain as is and do not get spread)
# names_from =  specifies the column from which the values will be spread to form new columns.
# Values_from =  specifies the columns whose values will be spread into new columns (names_from columns).
library(tidyr)
library(dplyr)


Mines_PFAS_Soil_Wide <- Mines_Soil_2024_5 %>%
  pivot_wider(
    id_cols = c(Dataset:Units, Medium),
    names_from = Component,
    values_from = Result
  )

PFAS <- as.data.frame(unique(Mines_Soil_2024_5$Component))

unique(Mines_PFAS_Soil_Wide$Medium)

# Create sum of pfoa and pfos variable
Mines_PFAS_Soil_Wide_1  <- Mines_PFAS_Soil_Wide |>  mutate (`Sum of PFOA and PFOS` = PFOA + PFOS) |> 
rename(
  PFDoDA = PFDoA,
  NEtFOSA = EtFOSA,
  NEtFOSAA = EtFOSAA,
  PFOSA = FOSA,
  NMeFOSA = MeFOSA,
  NMeFOSAA = MeFOSAA,
  PFUnA = PFUdA,
  PFDoDA = PFDoA
)


# 5: Reorder variables ####

# Create a character vector containing the EXACT column names in the desired order.
desired_order <- c(
  "Dataset",
  "Program",
  "Medium",
  "Site",
  "Site ID",
  "Address",
  "Latitude",
  "Longitude",
  "Link",
  "Sample date",
  "Sample ID",
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
Mines_PFAS_Soil_Wide_2 <- Mines_PFAS_Soil_Wide_1 |> select(all_of(desired_order), everything())




# 6: Assign variable types ####

glimpse(Mines_PFAS_Soil_Wide_2)

# Define the groups of columns based on your data dictionary
char_cols <- c("Dataset",
               "Program",
               "Medium",
               "Site",
               "Site ID",
               "Sample ID",
               "Address",
               "Link",
               "Units")

numeric_cols <- c("Latitude", "Longitude", "Number of samples")

date_cols <- c("Sample date") # Note the date format is MM/DD/YYYY

# Apply the transformations using mutate() and across()
Mines_PFAS_Soil_Wide_3 <- Mines_PFAS_Soil_Wide_2 %>%
  # 1. Convert Character Columns
  mutate(across(.cols = all_of(char_cols), .fns = as.character)) %>%
  
  # 2. Convert Numeric Columns
  mutate(across(.cols = all_of(numeric_cols), .fns = as.numeric)) %>%
  
  # 3. Convert Date Columns
  mutate(across(
    .cols = all_of(date_cols),
    # Specify the format: %m = Month, %d = Day, %Y = 4-digit Year
    .fns = ~ as.Date(., format = "%m/%d/%y")
  ))


# Ensure the sample date column is formatted as a date

Mines_PFAS_Soil_Wide_4 <- Mines_PFAS_Soil_Wide_3 %>%
  mutate(
    `Sample date` = as.Date(`Sample date`, format = "%m/%d/%Y")
  )

class(Mines_PFAS_Soil_Wide_4$`Sample date`)



# 9: Export ####

# Export the surface water data
library("writexl")
write_xlsx(Mines_PFAS_Soil_Wide_4,"03_Clean_Data/Soil/Mines_Soil_2024.xlsx")

