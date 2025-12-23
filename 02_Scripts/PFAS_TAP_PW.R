# PFAS Testing and Assistance Program
# Private well data

# 1: Importing the pilot data ####

library(readxl)
library(dplyr)

# : Importing the pilot effort data
# Lat longs added
PFAS_TAP_Pilot <- read_excel("01_Raw_Data/One_Time_Efforts/2024_2025_PFAS_TAP_Pilot.xlsx")


# 2: Cleaning Pilot effort data ####

# checking for duplicates of the same types of PFAS within sample Ids and creating a data frame from them called "duplicates"
duplicates <- PFAS_TAP_Pilot %>%
  group_by(SAMPLE_ID, ANALYTE_NAME) %>%
  filter(row_number() > 1)

print(duplicates)

# Remove duplicates of the same types of PFAS within sample IDs.
PFAS_TAP_Pilot_NoDup <- PFAS_TAP_Pilot %>%
  group_by(SAMPLE_ID, ANALYTE_NAME) %>%
  distinct() %>%
  ungroup()

table(PFAS_TAP_Pilot_NoDup$ANALYTE_NAME)

# Create an indicator for whether a result is non-detect
PFAS_TAP_Pilot_ND <- PFAS_TAP_Pilot_NoDup  %>%
  mutate(TAP_ND = ifelse(grepl("<", RESULT_AMT), "yes", "no"))

# Remove the "<" symbol and format as numeric.
PFAS_TAP_Pilot_ND <- PFAS_TAP_Pilot_ND %>%
  mutate(RESULT_AMT = sub("<", "", RESULT_AMT)) |>
  mutate(RESULT_AMT = as.numeric(RESULT_AMT))

# # Divide non-detects by 2 - not necessary since we are substituing all ND as 0
# PFAS_TAP_Pilot_ND_1 <- PFAS_TAP_Pilot_ND %>%
#   mutate(RESULT_AMT = if_else(TAP_ND == "yes", RESULT_AMT / 2, RESULT_AMT)) |>
#   mutate(RESULT_AMT = round(RESULT_AMT, 1)) |>  select(-TAP_ND)

# Sub non-detects with 0
PFAS_TAP_Pilot_ND_1 <- PFAS_TAP_Pilot_ND %>%
  mutate(RESULT_AMT = if_else(TAP_ND == "yes", 0.0, RESULT_AMT)) |>
  mutate(RESULT_AMT = round(RESULT_AMT, 1)) |>  select(-c(TAP_ND, -SAMPLE_ADDRESS))

unique(PFAS_TAP_Pilot_ND_1$ANALYTE_NAME)

# Transposing the pilot data from long to wide

PFAS_TAP_Pilot_ND_2 <- PFAS_TAP_Pilot_ND_1 |>
  mutate(
    ANALYTE_NAME = case_when(
      # --- 1. Formatting and Special Character Differences ---
      ANALYTE_NAME == "10:2_FTS" ~ "10:2FTS",
      ANALYTE_NAME == "3:3_FTCA" ~ "3:3FTCA",
      ANALYTE_NAME == "3:3 FTCA" ~ "3:3FTCA",
      ANALYTE_NAME == "5:3_FTCA" ~ "5:3FTCA",
      ANALYTE_NAME == "5:3 FTCA" ~ "5:3FTCA",
      ANALYTE_NAME == "6:2_FTS" ~ "6:2FTS",
      ANALYTE_NAME == "6:2 FTS" ~ "6:2FTS",
      ANALYTE_NAME == "7:3_FTCA" ~ "7:3FTCA",
      ANALYTE_NAME == "8:2_FTS" ~ "8:2FTS",
      
      # --- 2. Prefix and Synonym Differences ---
      ANALYTE_NAME == "EtFOSA" ~ "N-EtFOSA",
      ANALYTE_NAME == "EtFOSAA" ~ "N-EtFOSAA",
      ANALYTE_NAME == "MeFOSA" ~ "N-MeFOSA",
      ANALYTE_NAME == "MeFOSAA" ~ "N-MeFOSAA",
      ANALYTE_NAME == "PFUdA" ~ "PFUnA",
      TRUE ~ ANALYTE_NAME
    )
  ) # Standardizing PFUnA to match List 1


# 3: Transposing the pilot effort data ####


library(tidyr)
PFAS_TAP_Pilot_Wide <- PFAS_TAP_Pilot_ND_2 |> pivot_wider(names_from = ANALYTE_NAME, values_from = RESULT_AMT)


# If a value is NA then it should be 0 because it means it was not detected
# Create a vector for all the columns

unique(PFAS_TAP_Pilot_ND_2$ANALYTE_NAME)

All_Analytes <- c(
  "FBSA",
  "4:2 FTS",
  "10:2_FTCA",
  "3:3FTCA",
  "5:3FTCA",
  "N-EtFOSAA",
  "NMeFOSE",
  "NEtFOSE",
  "GenX",
  "NFDHA",
  "7:3FTCA",
  "PFBA_MRM",
  "PFPeA",
  "PFUnA",
  "PFTrDA",
  "PFNS",
  "Cl-O-PFUdS",
  "PFMPA",
  "PFMBA",
  "PFEESA",
  "PFODA",
  "PFTeDA",
  "10:2FTS",
  "10:2_UFTCA",
  "6:2_FTCA",
  "6:2FTS",
  "6:2_UFTCA",
  "8:2_FTCA",
  "8:2FTS",
  "8:2_UFTCA",
  "PFHxA",
  "PFOA",
  "PFNA",
  "PFDoA",
  "PFHxDA",
  "FOSAA",
  "N-MeFOSAA",
  "PFDoS",
  "Cl-PFOS",
  "FHpSA",
  "PFDA",
  "FHxSA",
  "N-MeFOSA",
  "N-EtFOSA",
  "PFHpS",
  "PFOS",
  "PFDS",
  "ADONA",
  "Cl-O-PFNS",
  "FPeSA",
  "PFPrS",
  "PFPeS",
  "PFEtCHxS",
  "TOTAL_PFAS",
  "PFHxS",
  "PFHpA",
  "PFBS",
  "PFBA",
  "Total PFAS (EPA PFAS6)",
  "PFOSA",
  "FOUEA",
  "9Cl-PF3ONS"
)


PFAS_TAP_Pilot_Wide_1 <- PFAS_TAP_Pilot_Wide |>
  mutate(across(All_Analytes, ~ dplyr::if_else(is.na(.), 0.0, .)))


# 4: Importing the expansion data ####


library(readxl)
library(dplyr)

# : Importing the data

# When you perform the final update, vlookup the lat and longs form the PFAS tap append here datasheet using the tap ID
PFAS_TAP_Expansion <- read_excel("01_Raw_Data/Ongoing_Data_Collection/2025_PFAS_TAP_Expansion_12152015.xlsx")

glimpse(PFAS_TAP_Expansion)

# 5: Cleaning the expansion data ####

# select the columns to keep
PFAS_TAP_Expansion_1 <- PFAS_TAP_Expansion |>  select(SAMPLE_ID,
                                                      SAMPLE_DATE,
                                                      PFAS_TAP_ID,
                                                      PFOA_RESULT_AMT:PFUnA_RESULT_AMT)


# Removing "RESULT_AMT" in the column names
PFAS_TAP_Expansion_2 <- PFAS_TAP_Expansion_1 |>
  rename_with(
    .fn = ~ gsub("_RESULT_AMT", "", ., ignore.case = TRUE),
    .cols = contains("RESULT")
  )


# Replace "<" and "ppt" in the PFAS analyte columns
PFAS_Columns <- c(
  "PFOA",
  "PFOS",
  "PFNA",
  "PFHxS",
  "PFBS",
  "GenX",
  "PFBA",
  "PFPeA",
  "PFHxA",
  "PFHpA",
  "PFDA",
  "10:2FTS",
  "11CL-PF3OUdS",
  "3:3FTCA",
  "4:2FTS",
  "5:3FTCA",
  "6:2FTS",
  "6:6PFPi",
  "6:8PFPi",
  "7:3FTCA",
  "8:2FTS",
  "8:8PFPi",
  "8Cl-PFOS",
  "9Cl-PF3ONS",
  "ADONA",
  "FBSA",
  "FDSA-I",
  "FDUEA",
  "FHxSA",
  "FOSAA",
  "FOUEA",
  "MeFBSA",
  "N-AP-FHxSA",
  "N-EtFOSA",
  "N-EtFOSAA",
  "NEtFOSE",
  "NFDHA",
  "N-MeFOSA",
  "N-MeFOSAA",
  "NMeFOSE",
  "PFDoA",
  "PFDoS",
  "PFDS",
  "PFECHS",
  "PFEESA",
  "PFHpS",
  "PFMOBA",
  "PFMOPrA",
  "PFNS",
  "PFOSA",
  "PFPeS",
  "PFPrS",
  "PFTeA",
  "PFTrDA",
  "PFUnA"
)



# Replace values with "<" with "0"
PFAS_TAP_Expansion_3 <- PFAS_TAP_Expansion_2 |>
  mutate(across(
    .cols = PFAS_Columns,
    # Use a lambda function to explicitly define the steps:
    .fns = ~ {
      # 1. Use base R's ifelse for conditional replacement:
      #    CONDITION: grepl("<", x=.) returns TRUE/FALSE (logical vector)
      #    TRUE: Replace with the string "0"
      #    FALSE: Keep the original value (.)
      cleaned_value <- ifelse(grepl(pattern = "<", x = .), "0", .)
      
      # 2. Convert the clean string (which is now either "0" or a number string) to numeric
      as.numeric(cleaned_value)
    }
  ))

# round the columns to one decimal
PFAS_TAP_Expansion_4 <- PFAS_TAP_Expansion_3 |>
  mutate(across(PFAS_Columns, ~ round(., 1))) |>  rename(`4:2 FTS`= `4:2FTS`)


# 6: Merging the expansion data and pilot data ####
PFAS_TAP_All <- bind_rows(PFAS_TAP_Expansion_4, PFAS_TAP_Pilot_Wide_1)


# Removing unecessary variables
PFAS_TAP_All_1 <- PFAS_TAP_All |> select(-c(`Total PFAS (EPA PFAS6)`, TOTAL_PFAS))


# 7: Rename and create new variables ####

PFAS_TAP_All_2 <- PFAS_TAP_All_1 %>%
  rename(`Site ID` = PFAS_TAP_ID,
         `Sample date` = SAMPLE_DATE,
         `Sample ID` = SAMPLE_ID) |>
  # Create new variables
  mutate(
    Dataset = "PFAS Testing and Assistance Program Data",
    Medium = "Private well",
    `Sum of PFOA and PFOS` = PFOA + PFOS,
    Link = "https://coepht.colorado.gov/fish-consumption",
    Program = "CDPHE's Toxicology and Environmental Epidemiology Office (TEEO)",
    `Number of samples` = 1,
    Units = "ng/L"
  )



# 8: Reorder variables ####

# Create a character vector containing the EXACT column names in the desired order.
desired_order <- c(
  "Dataset",
  "Program",
  "Medium",
  "Site ID",
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
  "PFBS",
  "GenX"
)

# Reorder variables using the select function
PFAS_TAP_All_3 <- PFAS_TAP_All_2  |> select(all_of(desired_order), everything())


# 9: Assign variable types ####

# Define the groups of columns based on your data dictionary
char_cols <- c("Dataset", "Program", "Medium", "Link", "Units")

numeric_cols <- c("Latitude", "Longitude", "Number of samples")

date_cols <- c("Sample date") # Note the date format is MM/DD/YYYY

# Apply the transformations using mutate() and across()
PFAS_TAP_All_4 <- PFAS_TAP_All_3 %>%
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


library(dplyr)

PFAS_TAP_All_5 <- PFAS_TAP_All_4 %>% rename(
  # Standardizing duplicates and fixing syntax
  `3:3 FTCA`      = `3:3FTCA`,
  `6:8 PFPi`      = `6:8PFPi`,
  `NEtFOSA`      = `N-EtFOSA`,   
  `NMeFOSAA`     = `N-MeFOSAA`,
  `10:2 UFTCA`    = `10:2_UFTCA`,
  `7:3 FTCA`      = `7:3FTCA`,
  `NEtFOSAA`     = `N-EtFOSAA`,
  `6:2 FTCA`      = `6:2_FTCA`,
  `HFPO-DA`          = `GenX`,      
  `5:3 FTCA`      = `5:3FTCA`,
  `8:2 FTS`       = `8:2FTS`,
  `6:2 UFTCA`     = `6:2_UFTCA`,
  `10:2 FTS`      = `10:2FTS`,
  `6:2 FTS`       = `6:2FTS`,
  `8:8 PFPi`      = `8:8PFPi`,
  `10:2 FTCA`     = `10:2_FTCA`,
  `8:2 FTCA`      = `8:2_FTCA`,  
  `11Cl-PF3OUdS`  = `11CL-PF3OUdS`,
  `6:6 PFPi`      = `6:6PFPi`,
  `8Cl-PFOS`      = `8Cl-PFOS`,
  `NMeFOSA`      = `N-MeFOSA`,
  `8:2 UFTCA`     = `8:2_UFTCA`
)



# Ensure the sample date column is formatted as a date

PFAS_TAP_PW_121525 <- PFAS_TAP_All_5 %>%
  mutate(`Sample date` = as.Date(`Sample date`, format = "%m/%d/%Y"))

class(PFAS_TAP_All_5$`Sample date`)


# 10: Export ####

# Export Datasets
library("writexl")


getwd()

write_xlsx(PFAS_TAP_PW_121525,
           "03_Clean_Data/PrivateWell/PFAS_TAP_PW_121525.xlsx")

