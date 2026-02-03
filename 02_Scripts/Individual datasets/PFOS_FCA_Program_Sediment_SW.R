# CDPHE PFOS FCA PROGRAM SEDIMENT AND SURFACE WATER


# 1: Importing the data ####

library(readxl)
library(dplyr)


getwd()
# Importing data
PFOS_FCA_Sed_SW_2023 <- read_excel(
  "01_Raw_Data/Ongoing_Data_Collection/2023_PFOS_FCA_Sediment_SW.xlsx")



# 2: Misc. Cleaning ####


# Check to see columns in DF
colnames(PFOS_FCA_Sed_SW_2023)


# Filter out values that are not blank for spiked
PFOS_FCA_Sed_SW_2023_1 <- PFOS_FCA_Sed_SW_2023 %>% filter(Spiked == "" |
                                                              is.na(Spiked) &
                                                              (Units != "%REC") & Aliquot == "SA" &  Validation_Level != "NV")


# Filter out duplicate
PFOS_FCA_Sed_SW_2023_2 <- PFOS_FCA_Sed_SW_2023_1 %>% filter(!(Client_ID %in% c("P2023001_W_DUP", "P2023001_S_DUP")))

# View the number of samples per client ID 
table(PFOS_FCA_Sed_SW_2023_2$Client_ID)


# Filtering to remove duplicates of the same waterbody, collection date, PFAS analyte, and result
PFOS_FCA_Sed_SW_2023_3 <- PFOS_FCA_Sed_SW_2023_2 %>%
  group_by(Waterbody, Matrix, Component, Client_ID) %>%
  distinct() %>%
  ungroup()



# Replace Non-detects with 0
PFOS_FCA_Sed_SW_2023_4 <- PFOS_FCA_Sed_SW_2023_3 %>%
  mutate(Result = case_when(Result %in% c("ND","0") ~ "0.0", TRUE ~ Result))

# Turn the result column into a numeric variable
PFOS_FCA_Sed_SW_2023_4$Result <- as.numeric(PFOS_FCA_Sed_SW_2023_4$Result)

# Round results column to 1 decimal place
PFOS_FCA_Sed_SW_2023_4$Result <- round(PFOS_FCA_Sed_SW_2023_4$Result, digits = 1)


class(PFOS_FCA_Sed_SW_2023_4$Result)




# 3: Transposing the data ####


# Transpose the data from long to wide format:
# Id_Cols = specify the column(s) that you want to keep as identifiers (i.e., columns that remain as is and do not get spread)
# names_from =  specifies the column from which the values will be spread to form new columns.
# Values_from =  specifies the columns whose values will be spread into new columns (names_from columns).
library(tidyr)
library(dplyr)


# Transpose the aggregated data into wide format. Included all variables except SeasonID because it messed up the transposing for some reason.

# Try using all variables as id_cols
#check for val_result=NA
PFOS_FCA_Sed_SW_2023_Wide <- PFOS_FCA_Sed_SW_2023_4 %>%
  pivot_wider(
    id_cols = c(
      "Waterbody",
      "Client_ID",
      "Collected",
      "Matrix",
      "SampleNumber",
      "Units",
      "Latitude",
      "Longitude"
    ),
    names_from = Component,
    values_from = Result
  )


# Get DF column names
colnames(PFOS_FCA_Sed_SW_2023_Wide)


# 4: Rename and create new variables ####


PFOS_FCA_Sed_SW_2023_Wide1 <- PFOS_FCA_Sed_SW_2023_Wide %>%
  rename(
    Site = Waterbody,
    `Sample date` = Collected,
    `Sample ID` = Client_ID,
    `HFPO-DA` = "HFPO-DA (GenX)",
    ADONA = "4,8-Dioxa-3H-perfluorononanoic acid (ADONA)",
    NEtFOSAA = "N-ethylperfluorooctanesulfonamidoacetic acid (NEtFOSAA)",
    NFDHA = "NFDHA",
    NMeFOSE = "NMeFOSE",
    NMeFOSA = "N-methylperfluorooctane sulfonamide (NMeFOSA)",
    NMeFOSAA = "N-methylperfluorooctanesulfonamidoacetic acid (NMeFOSAA)",
    PFBS = "Perfluorobutanesulfonic acid (PFBS)",
    PFBA = "Perfluorobutanoic acid (PFBA)",
    PFDS = "Perfluorodecanesulfonic acid (PFDS)",
    PFDA = "Perfluorodecanoic acid (PFDA)",
    PFDoS = "Perfluorododecanesulfonic acid (PFDoS)",
    PFDoA = "Perfluorododecanoic acid (PFDoA)",
    PFHpS = "Perfluoroheptanesulfonic acid (PFHpS)",
    PFHpA = "Perfluoroheptanoic acid (PFHpA)",
    PFHxS = "Perfluorohexanesulfonic acid (PFHxS)",
    PFHxA = "Perfluorohexanoic acid (PFHxA)",
    PFNS = "Perfluorononanesulfonic acid (PFNS)",
    PFNA = "Perfluorononanoic acid (PFNA)",
    PFOSA = "Perfluorooctanesulfonamide (FOSA)",
    PFOS = "Perfluorooctanesulfonic acid (PFOS)",
    PFOA = "Perfluorooctanoic acid (PFOA)",
    PFPeS = "Perfluoropentanesulfonic acid (PFPeS)",
    PFPeA = "Perfluoropentanoic acid (PFPeA)",
    PFTeA = "Perfluorotetradecanoic acid (PFTeA)",
    PFTrDA = "Perfluorotridecanoic acid (PFTriA)",
    PFUnA = "Perfluoroundecanoic acid (PFUnA)",
    Notes = Matrix
  ) 


# Create new variables

PFOS_FCA_Sed_SW_2023_Wide2 <- PFOS_FCA_Sed_SW_2023_Wide1 %>%
  mutate(
    Dataset = "PFOS FCA Program",
    `Sum of PFOA and PFOS` = PFOA + PFOS,
    Link = "https://coepht.colorado.gov/fish-consumption",
    Program = "CDPHE's Water Quality Control Division - Fish Consumption Advisory Program",
    `Number of samples` = 1
  ) %>%
  mutate(Medium = case_when(Notes == "SOLID" ~ "Sediment", TRUE ~ "Surface water"))


# 5: Delete columns ####

PFOS_FCA_Sed_SW_2023_Wide2 <- PFOS_FCA_Sed_SW_2023_Wide2 |>  select (-SampleNumber)


# 6: Reorder variables ####

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
  "Sample ID",
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
PFOS_FCA_Sed_SW_2023_Wide3 <- PFOS_FCA_Sed_SW_2023_Wide2  |> select(all_of(desired_order), everything())


# 7: Assign variable types ####

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
PFOS_FCA_Sed_SW_2023_Wide4 <- PFOS_FCA_Sed_SW_2023_Wide3 %>%
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

PFOS_FCA_Sed_SW_2023_Wide4 <- PFOS_FCA_Sed_SW_2023_Wide3 %>%
  mutate(
    `Sample date` = as.Date(`Sample date`, format = "%m/%d/%Y")
  )

class(PFOS_FCA_Sed_SW_2023_Wide4$`Sample date`)




# 7: Subset into different media ####


# Subset into different Media
PFOS_FCA_Sediment_2023 <- PFOS_FCA_Sed_SW_2023_Wide4 %>% filter (Medium == "Sediment")
PFOS_FCA_SurfaceWater_2023 <- PFOS_FCA_Sed_SW_2023_Wide4 %>% filter (Medium ==
                                                                     "Surface water")
# 8: Export ####

# Export Datasets
library("writexl")


getwd()

write_xlsx(
  PFOS_FCA_Sediment_2023,
  "03_Clean_Data/Sediment/PFOS_FCA_Sediment_2023.xlsx"
)
write_xlsx(
  PFOS_FCA_SurfaceWater_2023,
  "03_Clean_Data/SurfaceWater/PFOS_FCA_SurfaceWater_2023.xlsx"
)
