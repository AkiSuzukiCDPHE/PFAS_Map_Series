# The National Water Quality Portal
# SW, GW and fish Tissue



# Section 1 Importing the data ####

# Importing data from an excel workbook
library(readxl)
library(dplyr)
library(lubridate)
library(tidyverse)



# Get the original file path
getwd()


WQP_Wells <- read_excel("01_Raw_Data/Ongoing_Data_Collection/2025_WQP_20251212.xlsx",
                          col_types = c(rep("guess", 38), "text", rep("guess", 42)))

# # View all variables and types
str(WQP_Wells)


# # Turn activity end date into a date
# WQP_Wells$ActivityEndDate <- as.Date(WQP_Wells$ActivityEndDate, format = "%m-%d-%Y")

# Importing location data
WQP_LatLongs <- read_excel("01_Raw_Data/Ongoing_Data_Collection/WQP_LatLongs.xlsx")


# Section 2: Cleaning the data ####


# Remove extraneous variables from the lat longs dataset
WQP_LatLongs_1 <- WQP_LatLongs |> select(c(
  MonitoringLocationIdentifier,
  LatitudeMeasure,
  LongitudeMeasure
))


# Merge the Lat Longs from the Site dataset
WQP_Wells_Merged <- left_join(WQP_Wells, WQP_LatLongs_1, by = "MonitoringLocationIdentifier")


# Replace missing units with units from the qualifier unit variable
WQP_Wells_Merged <- WQP_Wells_Merged |> mutate(
  `ResultMeasure/MeasureUnitCode` =
    if_else(
      is.na(`ResultMeasure/MeasureUnitCode`),
      `DetectionQuantitationLimitMeasure/MeasureUnitCode`,
      `ResultMeasure/MeasureUnitCode`
    ))





# Remove extraneous variables for the wells dataset
WQP_Wells_Merged_1 <- WQP_Wells_Merged |> select(
  c(
    OrganizationFormalName,
    ActivityTypeCode,
    ActivityStartDate,
    ActivityMediaName,
    ActivityMediaSubdivisionName,
    ProjectName,
    MonitoringLocationName,
    MonitoringLocationIdentifier,
    LatitudeMeasure,
    LongitudeMeasure,
    ResultIdentifier,
    ResultDetectionConditionText,
    MethodSpeciationName,
    CharacteristicName,
    ResultSampleFractionText,
    ResultMeasureValue,
    ResultStatusIdentifier,
    ResultValueTypeName,
    ResultLaboratoryCommentText,
    DetectionQuantitationLimitTypeName,
    `DetectionQuantitationLimitMeasure/MeasureValue`,
    `DetectionQuantitationLimitMeasure/MeasureUnitCode`,
    `ResultMeasure/MeasureUnitCode`,
    StatisticalBaseCode
))




# Create a new column for PFAS_Abbrev
library(dplyr)
library(stringr)

WQP_Wells_Merged_1 <- WQP_Wells_Merged_1 |>
  mutate(
    # Create the new column 'PFAS_Abbrev' based on 'CharacteristicName'
    PFAS_Abbrev = case_when(
      # --- Tier 1: Common, fully-named compounds ---
      str_detect(CharacteristicName, "(^| )PFOA( |$| ion)") ~ "PFOA",
      str_detect(CharacteristicName, "N-Ethyl-N-(2-hydroxyethyl)perfluorooctanesulfonamide") ~ "N-EtFOSE",
      str_detect(CharacteristicName, "(^| )Perfluorooctanesulfon(ate|ic acid|amide)") ~ "PFOS",
      str_detect(CharacteristicName, "Perfluoro(hexanoic|hexanoate)") ~ "PFHxA",
      str_detect(CharacteristicName, "Perfluorohexanesulfon(ate|ic acid|amide)") ~ "PFHxS",
      str_detect(CharacteristicName, "Perfluorononano(ate|ic acid)") ~ "PFNA",
      str_detect(CharacteristicName, "Perfluorodecano(ate|ic acid|sulfonate)") ~ "PFDA",
      str_detect(CharacteristicName, "Perfluorobutanesulfon(ate|ic acid)") ~ "PFBS",
      str_detect(CharacteristicName, "Perfluorobutano(ate|ic acid)") ~ "PFBA",
      str_detect(CharacteristicName, "Perfluoroundecano(ate|ic acid)") ~ "PFUnDA",
      str_detect(CharacteristicName, "Perfluorododecano(ate|ic acid|sulfonate)") ~ "PFDoDA",
      str_detect(CharacteristicName, "Perfluorotridecano(ate|ic acid)") ~ "PFTrDA",
      str_detect(CharacteristicName, "Perfluorotetradecano(ate|ic acid)") ~ "PFTeDA",
      str_detect(CharacteristicName, "Perfluoroheptano(ate|ic acid)") ~ "PFHpA",
      str_detect(CharacteristicName, "Perfluoroheptanesulfon(ate|ic acid)") ~ "PFHpS",
      str_detect(CharacteristicName, "Perfluoropentano(ate|ic acid|sulfonate)") ~ "PFPeA",
      str_detect(CharacteristicName, "Perfluoropentanesulfon(ate|ic acid)") ~ "PFPeS",
      
      # --- Tier 2: Fluorotelomer compounds (FtS, FtCA) ---
      str_detect(CharacteristicName, "6:2 Fluorotelomer sulfonate") ~ "6:2 FTS",
      str_detect(CharacteristicName, "4:2 Fluorotelomer sulfonate") ~ "4:2 FTS",
      str_detect(CharacteristicName, "8:2 Fluorotelomer sulfonate") ~ "8:2 FTS",
      str_detect(CharacteristicName, "3:3 Fluorotelomer carboxylate") ~ "3:3 FtCA",
      
      # --- Tier 3: N-Substituted compounds (FOSA, FOSE) ---
      str_detect(CharacteristicName, "N-Methylperfluorooctanesulfonamidoacetate") ~ "NMeFOSAA",
      str_detect(CharacteristicName, "N-Ethylperfluorooctanesulfonamidoacetate") ~ "NEtFOSAA",
      str_detect(CharacteristicName, "N-methylperfluoro-1-octanesulfonamide") ~ "NMeFOSA",
      str_detect(CharacteristicName, "N-ethyl Perfluorooctane sulfonamide") ~ "NEtFOSA",
      str_detect(CharacteristicName, "2-(N-methylperfluoro-1-octanesulfonamido)-ethanol") ~ "NMeFOSE",
      str_detect(CharacteristicName, "N-Ethyl-N-(2-hydroxyethyl)perfluorooctanesulfonamide") ~ "NEtFOSE",
      str_detect(CharacteristicName, "Perfluorooctanesulfonamide") ~ "FOSA",
      
      # --- Tier 4: Other specific / Branded compounds ---
      str_detect(CharacteristicName, "Hexafluoropropylene oxide dimer acid") ~ "GenX",
      str_detect(CharacteristicName, "4,8-Dioxa-3H-perfluorononano(ate|ic acid)") ~ "ADONA",
      str_detect(CharacteristicName, "11-chloroeicosafluoro-3-oxaundecane-1-sulfonate") ~ "11Cl-PF3OUdS",
      str_detect(CharacteristicName, "9-Chlorohexadecafluoro-3-oxanonane-1-sulfonate") ~ "9Cl-PF3ONS",
      str_detect(CharacteristicName, "Perfluoro(2-propoxypropanoate)") ~ "PFPPrA",
      str_detect(CharacteristicName, "Sulfluramid") ~ "N-EtFOSA",
      
      # --- Default: If no match, keep the original name ---
      TRUE ~ CharacteristicName
    )
  )



# Filter out certain values
WQP_Wells_Merged_2 <- WQP_Wells_Merged_1 |>
  filter(
    !CharacteristicName %in% c("CFC-114", "Hexaflumuron", "(-)-delta9-THC-d3"),
    # Sample-Composite With Parents is for fish tissue samples
    ActivityTypeCode %in% c("Sample-Routine", "Sample-Composite With Parents"),
    !ResultSampleFractionText %in% c("Bed Sediment", "Non-filterable", "Settleable"),
    ResultValueTypeName == "Actual",
    !StatisticalBaseCode %in% c("Mean", "Counting Error"),
    !`ResultMeasure/MeasureUnitCode` %in% c(
      "uS/cm @25C",
      "NTU",
      "% saturatn",
      "tons/ac ft",
      "%",
      "tons/day",
      "pct modern",
      "cm3/g STP",
      "ratio",
      "NTRU",
      "cm3/g @STP",
      "None",
      "#/ml",
      "PCU",
      "T.U.",
      "g/mL @ 20C",
      "mm/Hg",
      "years BP",
      "FNU",
      "mg N/l",
      "ft3/s",
      "m3/sec",
      "ft",
      "code"
    ),
    !`DetectionQuantitationLimitMeasure/MeasureUnitCode` %in% c(
      "years BP",
      "tons/ac ft",
      "pct modern",
      "tons/day",
      "NTRU",
      "% saturatn",
      "cm3/g @STP",
      "uS/cm @25C",
      "T.U.",
      "None",
      "m3/sec",
      "ft3/s",
      "%"
    )
  )



# Section 3: Filtering out qualifiers ####

# Each time you will need to check which 
# lab comments and detection condition texts represent invalid results

# Evaluate the qualifiers in the data to determine which ones to keep and which are invalid
unique(WQP_Wells_Merged_2$ResultDetectionConditionText)
unique(WQP_Wells_Merged_2$ResultLaboratoryCommentText)

# Filter for result condition text equal to values that are not determined invalid
WQP_Wells_Merged_3 <- WQP_Wells_Merged_2 |> filter(
  ResultDetectionConditionText %in% c(
    "Not Detected", NA
  ) | is.na(ResultDetectionConditionText))



unique(WQP_Wells_Merged_2$ResultLaboratoryCommentText)


# Filter to remove samples with invalid lab comments. Remove any invalid, contaminated blanks or potentially biased.
WQP_Wells_Merged_4 <- WQP_Wells_Merged_3 |> filter(
  !ResultLaboratoryCommentText %in% c(
    "improper preservationsample was dilutedsee result laboratory comment",
    "analyte detected in laboratory blanksee result laboratory comment",
    "result may be affected by interferencesee result laboratory comment" ,
    "improper preservationsee result laboratory comment"
  )
)



# Section 4: Handling non-detects and formatting the results column ####


# For any result that includes a "<" make the result detection condition text "Not Detected"
WQP_Wells_Merged_5 <- WQP_Wells_Merged_4 |>  mutate(
  ResultDetectionConditionText = if_else(
    grepl("<", ResultMeasureValue),
    "Not Detected",
    ResultDetectionConditionText
  )
# Replace all non-detect results with 0
) |> 
  mutate(
    ResultMeasureValue = case_when(
      ResultLaboratoryCommentText == "below the detection level" |
        ResultDetectionConditionText == "Not Detected"
      ~ "0",
      TRUE ~ ResultMeasureValue))


unique(WQP_Wells_Merged_5$ResultMeasureValue)

# Create a new variable for medium
# Always check the MonitoringLocationName variable to verify samples with no value for activity media subdivision and which are labeled 
# "water" for activity media name are actually just surface water bodies.
WQP_Wells_Merged_5 <- WQP_Wells_Merged_5 |> mutate(
  Medium = case_when(
    ActivityMediaSubdivisionName == "Groundwater" ~ "Groundwater",
    ActivityMediaSubdivisionName == "Surface Water" ~ "Surface water",
    ActivityMediaName == "Tissue" ~ "Fish tissue",
    is.na(ActivityMediaSubdivisionName) & ActivityMediaName == "Water" ~ "Surface water"))



# Convert the Results column to numeric and round to one decimal place
WQP_Wells_Merged_6 <- WQP_Wells_Merged_5 |>  mutate(
  ResultMeasureValue =
    round(as.numeric(ResultMeasureValue), 1)) |> 
# Fix units 
  mutate(`ResultMeasure/MeasureUnitCode` = case_when(ActivityMediaName == "Water"~ "ng/L",
                                                     ActivityMediaName == "Tissue" ~ "ng/g"))


# Section 5: More misc cleaning ####


# Remove extraneous variables
WQP_Wells_Merged_7 <- WQP_Wells_Merged_6 |>
select(
  -c(CharacteristicName,
     ActivityTypeCode,
     ActivityMediaSubdivisionName,
     ActivityMediaName, 
     MethodSpeciationName,
     ResultSampleFractionText,
     ResultStatusIdentifier:`DetectionQuantitationLimitMeasure/MeasureUnitCode`,StatisticalBaseCode)
)



# Creating a sample ID
WQP_Wells_Merged_8 <- WQP_Wells_Merged_7 |>
  # 1. Group by the variables that define a single sampling event
  group_by(
    MonitoringLocationIdentifier, # Site ID
    ActivityStartDate,            # Sample Date
    LatitudeMeasure,              # Latitude
    LongitudeMeasure              # Longitude

  ) |>
  # 2. Assign a unique ID to each group
  mutate(`Sample ID` = cur_group_id()) |>
  # 3. Always ungroup after you are done!
  ungroup()




# Section 6: Rename and create new variables ####

# Rename variables to standardize across datasets, create variables, and remove extraneous variables
WQP_Wells_Merged_9 <- WQP_Wells_Merged_8 %>%
  rename(
    Site = MonitoringLocationIdentifier,
    `Site ID` = MonitoringLocationName,
    `Sample date` = ActivityStartDate,
    Result = ResultMeasureValue,
    Units = `ResultMeasure/MeasureUnitCode`,
    Longitude = LongitudeMeasure,
    Latitude = LatitudeMeasure,
    Notes = ProjectName,
    Program = OrganizationFormalName
  ) %>%
  mutate(
    Dataset = "The National Water Quality Portal",
    Link = "https://www.waterqualitydata.us/",
    `Number of samples` =1
  ) 





# Section 7: Transpose the dataset #####



str(WQP_Wells_Merged_9)

# Before transposing making sure there are not duplicates
WQP_Wells_Merged_10 <- WQP_Wells_Merged_9 %>%
  group_by(
    Program,
    Dataset,
    Notes,
    `Site ID`,
    Site,
    `Sample ID`, 
    Latitude, 
    Longitude, 
    `Sample date`, 
    Medium, 
    Units, 
    PFAS_Abbrev,
    `Number of samples` ,
    Link
    
  ) %>%
  
  # 2. Collapse the group, keeping the Maximum 'Result' value
  summarise(
    Result = max(Result, na.rm = TRUE),
    .groups = "drop" ) # Remove the grouping structure
  

# Check if there are still duplicates
duplicates <- WQP_Wells_Merged_10 |>
  summarise(n = n(), .by = c(`Sample ID`, PFAS_Abbrev)) |>
  filter(n > 1)

# Remove any column from id_cols that is specific to the result for a specific analyte
#(like detection limit, qualifier) and only keep the columns that define the unique sample.
WQP_Wide <- WQP_Wells_Merged_10 %>%
  pivot_wider(
    id_cols = c(
     Latitude,
     Longitude,
     Dataset,
     Program,
     `Site ID`,
     Notes,
     Site,
     `Sample date`,
     Link,
     `Number of samples`,
     Medium,
     Units,
     `Sample ID`
     
    ),
    names_from = PFAS_Abbrev,
    values_from = Result
  )



# Create sum of pfoa and pfos variable
WQP_Wide1  <- WQP_Wide |>  mutate (`Sum of PFOA and PFOS` = PFOA + PFOS)


# Section 8: Reorder variables ####

# Create a character vector containing the EXACT column names in the desired order.
desired_order <- c(
  "Dataset",
  "Program",
  "Medium",
  "Site",
  "Site ID",
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
WQP_Wide2 <- WQP_Wide1 |> select(all_of(desired_order), everything())



# Section 9: Assign variable types ####

glimpse(WQP_Wide2)

# Define the groups of columns based on your data dictionary
char_cols <- c("Dataset",
               "Program",
               "Medium",
               "Site",
               "Link",
               "Units")

numeric_cols <- c("Latitude", "Longitude", "Number of samples")

date_cols <- c("Sample date") # Note the date format is MM/DD/YYYY

# Apply the transformations using mutate() and across()
WQP_Wide3 <- WQP_Wide2 %>%
  # 1. Convert Character Columns
  mutate(across(.cols = all_of(char_cols), .fns = as.character)) %>%
  
  # 2. Convert Numeric Columns
  mutate(across(.cols = all_of(numeric_cols), .fns = as.numeric)) %>%
  
  # 3. Convert Date Columns
  mutate(across(
    .cols = all_of(date_cols),
    # Specify the format: %m = Month, %d = Day, %Y = 4-digit Year
    .fns = ~ as.Date(., format = "%Y-%m-%d")
  ))


# Ensure the sample date column is formatted as a date

WQP_Wide3 <- WQP_Wide3 %>%
  mutate(
    `Sample date` = as.Date(`Sample date`, format = "%m-%d-%Y")
  )

class(WQP_Wide3$`Sample date`)


WQP_GW_SW_Fish_2025 <- WQP_Wide3

# Section 10: Export ####

# Subset by medium
WQP_GW_2025 <- WQP_GW_SW_Fish_2025 |> filter(Medium == "Groundwater")
WQP_SW_2025 <- WQP_GW_SW_Fish_2025 |> filter(Medium == "Surface water")
WQP_Fish_2025 <- WQP_GW_SW_Fish_2025 |> filter(Medium == "Fish tissue")


# Exporting the data
library(writexl)
write_xlsx(WQP_GW_2025, "03_CleanData/WQP_GW_2025.xlsx")
write_xlsx(WQP_SW_2025, "03_CleanData/WQP_SW_2025.xlsx")
write_xlsx(WQP_Fish_2025, "03_CleanData/WQP_Fish_2025.xlsx")
