# WQCD PFAS Database


# 1: Importing the data ####

library(readxl)
library(dplyr)

getwd()


# Sub this file and file path out for new data each time
WQCD_PFAS_DB <- read_excel("01_Raw_Data/Ongoing_Data_Collection/2025_WQCD_Database_112525.xlsx")
                         # col_types = cols(
                         #   `Sample Date` = col_date(format = "%m/%d/%Y")))


# 2: Misc. Cleaning ####


# Replace all spaces in names of variables with underscores
names(WQCD_PFAS_DB) <- gsub("\\s", "_", names((WQCD_PFAS_DB)))

glimpse(WQCD_PFAS_DB)

# Filtering the DF for untreated water
WQCD_PFAS_DB_1 <- WQCD_PFAS_DB %>% filter (TreatmentStatus == "UNF")

# Converting latitude and longitude to numeric variables
WQCD_PFAS_DB_2 <- WQCD_PFAS_DB_1 %>%
  mutate(across(Latitude:Longitude, .fns = as.numeric))


# Replace non-detects with zero and turn result into a numeric column
WQCD_PFAS_DB_3 <- WQCD_PFAS_DB_2 |> mutate(Result=if_else(grepl("<|ND", Result), "0.0", Result)) |>  mutate(Result=round(as.numeric(Result, digits=1)))

unique(WQCD_PFAS_DB_3$Result)



# 3: Rename and create new variables ####

# Rename variables to standardize across datasets, create variables, and remove extraneous variables
WQCD_PFAS_DB_4 <- WQCD_PFAS_DB_3 %>%
  rename(
    `Sample date` = DateCollected,
    `Sample ID` = SampleID,
    Address = SampleLocationStreetAddress
  ) %>%
  mutate(
    Dataset = "PFAS Grant Program and Additional Projects (WQCD PFAS Database)",
    Link = "https://cdphe.colorado.gov/pfas-per-and-polyfluoroalkyl-substances/pfas-grant-program/pfas-grant-summaries",
    `Number of samples` = 1,
    Notes = FacilityType,
    Program = "CDPHE's Water Quality Control Division (WQCD)",
    # coalesce takes the first value that isn't missing
    `Site ID` = coalesce(PWSID, NPDESPermitID),
    Site = coalesce(PWSSystemName, FacilityName)
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
      Notes,
      `Sample date`,
      `Sample ID`,
      `Number of samples`,
      `Units`,
      Component,
      Result,
      FacilityType,
      SourceWaterType,
      FacilityName
    )
  )




# Create variable for medium
WQCD_PFAS_DB_5 <-  WQCD_PFAS_DB_4 |>  mutate(
  Medium = case_when(
    SourceWaterType %in% c("GW", "GU") & FacilityType %in% c("PWS", "OTH", "FS", "MW") ~ "Groundwater",
      FacilityType == "PRIV" ~ "Private well",
    SourceWaterType == "SW" & FacilityType %in% c("STR", "PWS", "LK") ~ "Surface water",
    FacilityType == "WW" ~ "Wastewater"
  )
) |>  select(-c(FacilityType,SourceWaterType))



# 4: Transpose ####

# Filter data so there is one unique row for every combination
WQCD_PFAS_DB_6 <- WQCD_PFAS_DB_5 %>%
  distinct(Site,
           Medium,
           `Site ID`,
           `Sample date`,
           `Sample ID`,
           Latitude,
           Longitude,
           Component,
           .keep_all = TRUE)


str(WQCD_PFAS_DB_5)



# Transpose the data from long to wide format:
# Id_Cols = specify the column(s) that you want to keep as identifiers (i.e., columns that remain as is and do not get spread)
# names_from =  specifies the column from which the values will be spread to form new columns.
# Values_from =  specifies the columns whose values will be spread into new columns (names_from columns).
library(tidyr)
library(dplyr)


WQCD_PFAS_Wide <- WQCD_PFAS_DB_6 %>%
  pivot_wider(
    id_cols = c(Dataset:Units, Medium),
    names_from = Component,
    values_from = Result
  )


unique(WQCD_PFAS_Wide$Medium)

# Create sum of pfoa and pfos variable
WQCD_PFAS_Wide_1  <- WQCD_PFAS_Wide |>  mutate (`Sum of PFOA and PFOS` = PFOA + PFOS) |>  rename(PFTrDA = PFTriA)


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
  "Notes",
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
WQCD_PFAS_Wide_2 <- WQCD_PFAS_Wide_1 |> select(all_of(desired_order), everything())




# 6: Assign variable types ####

glimpse(WQCD_PFAS_Wide_2)

# Define the groups of columns based on your data dictionary
char_cols <- c("Dataset",
               "Program",
               "Medium",
               "Site",
               "Site ID",
               "Sample ID",
               "Address",
               "Link",
               "Notes",
               "Units")

numeric_cols <- c("Latitude", "Longitude", "Number of samples")

date_cols <- c("Sample date") # Note the date format is MM/DD/YYYY

# Apply the transformations using mutate() and across()
WQCD_PFAS_Wide_3 <- WQCD_PFAS_Wide_2 %>%
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

WQCD_PFAS_Wide_4 <- WQCD_PFAS_Wide_3 %>%
  mutate(
    `Sample date` = as.Date(`Sample date`, format = "%m/%d/%Y")
  )

class(WQCD_PFAS_Wide_4$`Sample date`)


# 7: Jitter PWS locations ####

# Jitter any samples where Facility type = "PWS"

# set.seed(123): Ensures the randomness is reproducible.
# runif(nrow(JitterTest), -JitterAmount, JitterAmount): Generates random values uniformly distributed between -JitterAmount and JitterAmount.
# This approach directly adds the desired jitter to each latitude value within the specified range.


# Jitter the latitude where .007245 degrees latitude = .5 miles
set.seed(123) # for reproducibility
LatitudeJitterAmount <- 0.007245
WQCD_PFAS_DB_1_Jitter <- WQCD_PFAS_Wide_4  %>%
  mutate(
    Latitude = if_else(
      Notes == "PWS",
      Latitude + runif(n(), -LatitudeJitterAmount, LatitudeJitterAmount),
      Latitude
    )
  )



# Jitter the longitude where 0.00925 degrees longitude = .5 miles
set.seed(123) # for reproducibility
LongitudeJitterAmount <- 0.00925
WQCD_PFAS_DB_2_Jitter <- WQCD_PFAS_DB_1_Jitter %>%
  mutate(
    Longitude = if_else(
      Notes == "PWS",
      Longitude + runif(n(), -LongitudeJitterAmount, LongitudeJitterAmount),
      Longitude
    )
  )


# 8: Subset by medium ####

# Subsetting to a surface Water
WQCD_PFAS_DB_SW <- WQCD_PFAS_DB_2_Jitter %>%
  filter(Medium == "Surface water")


# Subsetting groundwater
# “GROUNDWATER” means any water under the surface of the ground that is not surface water or groundwater under the direct influence of surface water (GU).
WQCD_PFAS_DB_GW <- WQCD_PFAS_DB_2_Jitter %>%
  filter(Medium == "Groundwater")

# Subsetting private wells
WQCD_PFAS_DB_PrivateWell <- WQCD_PFAS_DB_2_Jitter %>%
  filter(Medium == "Private well")

# Subsetting wastewater
WQCD_PFAS_DB_Wastewater<- WQCD_PFAS_DB_2_Jitter %>%
  filter(Medium == "Wastewater")


# 9: Export ####

# Export the surface water data
library("writexl")
write_xlsx(WQCD_PFAS_DB_SW,"03_Clean_Data/SurfaceWater/WQCD_Database_SW_2025.xlsx")

# Export the groundwater water data
library("writexl")
write_xlsx(WQCD_PFAS_DB_GW,"03_Clean_Data/Groundwater/WQCD_Database_GW_2025.xlsx")

# Export the surface water data
library("writexl")
write_xlsx(WQCD_PFAS_DB_PrivateWell,"03_Clean_Data/PrivateWell/WQCD_Database_PW_2025.xlsx")

# Export the surface water data
library("writexl")
write_xlsx(WQCD_PFAS_DB_Wastewater,"03_Clean_Data/Wastewater/WQCD_Database_WW_2025.xlsx")







# ND replacements in case dataset is wide

# # Replacing "ND" with 0
# 
# PFAS_Analytes <- c(
#   "_11Cl_PF3OUdS",
#   "_3_3a_3_20_FTCA",
#   "_4_3a_2_20_FTS",
#   "_5_3_20_FTCA",
#   "_6_2FTS",
#   "_7_3a_3_20_FTCA",
#   "_8_3a_2_20_FTS",
#   "_9Cl_2d_PF3ONS",
#   "ADONA",
#   "FOSA",
#   "GenX",
#   "NEtFOSAA",
#   "NMeFOSA",
#   "NMeFOSAA",
#   "NEtFOSA",
#   "NEtFOSE",
#   "NFDHA",
#   "NMeFOSE",
#   "PFOA",
#   "PFOS",
#   "PFBA",
#   "PFBS",
#   "PFDA",
#   "PFNS",
#   "PFNA",
#   "PFHxA",
#   "PFHxS",
#   "PFDS",
#   "PFDoS",
#   "PFDoA",
#   "PFHpS",
#   "PFHpA",
#   "PFEESA",
#   "PFMBA",
#   "PFMPA",
#   "PFPeS",
#   "PFPeA",
#   "PFTeA",
#   "PFTriA",
#   "PFUnA"
# )
# 
# 
# WQCD_PFAS_DB_1 <- WQCD_PFAS_DB_1 %>%
#   mutate_at(vars(all_of(PFAS_Analytes)), ~ ifelse(.=="null" | . == "ND", 0, .))
# 
# # Transforming all PFAS analytes to be numeric variables
# WQCD_PFAS_DB_1 <- WQCD_PFAS_DB_1 %>%
#   mutate_at(vars(all_of(PFAS_Analytes)), ~ ifelse(. == "null", NA, .))
# 
# WQCD_PFAS_DB_1 <- WQCD_PFAS_DB_1 %>%
#   mutate_at(vars(all_of(PFAS_Analytes)), ~ as.numeric(., na.rm = TRUE))

