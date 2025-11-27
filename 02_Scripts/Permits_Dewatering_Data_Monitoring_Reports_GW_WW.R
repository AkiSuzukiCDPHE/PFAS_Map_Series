# PERMITS - DATA MONITORING REPORTS.


# 1: Importing the data ####

library(readxl)
library(dplyr)


# Importing data
DMR_RawData <- read_excel(
  "X:\\Shared drives\\_CDPHE TEEO Data\\_Enviro\\PFAS\\PFAS Concentration Map May 2024 Update\\PFASMap 2024_RProject\\01_Raw_Data\\DMR Data Report PFAS 20240701.xlsx"
)



# 2: Misc Data Cleaning ####


# Replace all spaces in names of variables with underscores
names(DMR_RawData) <- gsub("\\s", "_", names((DMR_RawData)))

# Change ug/L units to ng/L because the ug/L entries were a typo
DMR_RawData <- DMR_RawData %>% mutate(
  Concentration_Units = case_when(
    Concentration_Units == "ug/L" ~ "ng/L",
    TRUE ~ Concentration_Units
  )
)

# Create a table to see the types of PFAS analytes included in the DF
PFAS_Types <- data.frame(table(DMR_RawData$Parameter))


# Check for duplicates of the same types of PFAS within NPDES ID for the same sampling date and same PFAS analyte. This creates a dataframe from them called "duplicates"
DMR_RawData_1 <- DMR_RawData %>%
  group_by(NPDES_ID, DMR_Received_Date, Parameter) %>%
  distinct() %>%
  ungroup()


# Delete any observations with NODI codes other than NODI B
Codes_Delete <- c(
  "NODI C",
  "NODI **E**",
  "NODI 7",
  "NODI 9" ,
  "NODI **P**",
  "NODI **8**",
  "NODI N",
  "NODI 2",
  "NODI 3",
  "NODI F"
)

# remove values of concentration 2 that are = "NODI C", "NODI **E**", "NODI 7", "NODI 9" ,"NODI **P**", or "NODI **8**"
DMR_RawData_2  <- DMR_RawData_1 %>%
  filter(!Concentration_2 %in% Codes_Delete)

DMR_RawData_3  <- DMR_RawData_2 %>%
  filter(!Concentration_3 %in% Codes_Delete)


# Create a variable with PFAS abbreviations called PFAS_Analyte
# Add PFOA

DMR_RawData_3 <- DMR_RawData_3 %>%
  mutate(
    PFAS_Analyte = case_when(
      Parameter %in% c("Perfluorooctanesulfonamide") ~ "PFOSA",
      Parameter %in% c("Perfluoroheptanoic acid") ~ "PFHpA",
      Parameter %in% c("Perfluorobutanesulfonic acid") ~ "PFBS",
      Parameter %in% c("Perfluorododecanoic acid") ~ "PFDoA",
      Parameter %in% c("N-Methylperfluorooctanesulfonamide") ~ "NMeFOSA",
      Parameter %in% c("Perfluorononanoic acid") ~ "PFNA",
      Parameter %in% c("Perfluoroundecanoic acid") ~ "PFUnA",
      Parameter %in% c("N-Ethylperfluorooctanesulfonamide") ~ "NEtFOSA",
      Parameter %in% c("Perfluorohexanoic acid") ~ "PFHxA",
      Parameter %in% c("Perfluorotridecanoic acid") ~ "PFTrDA",
      Parameter %in% c("Perfluorononane sulfonic acid") ~ "PFNS",
      Parameter %in% c("Perfluorotetradecanoic acid") ~ "PFTeDA",
      Parameter %in% c("Perfluoropentane sulfonic acid") ~ "PFPeS",
      Parameter %in% c("Perfluoroheptanesulfonic acid") ~ "PFHpS",
      Parameter %in% c("4,8-Dioxa-3H-perfluorononanoic acid") ~ "ADONA",
      Parameter %in% c("Hexafluoropropylene oxide dimer acid") ~ "GenX",
      Parameter %in% c("Perfluorodecanoic acid") ~ "PFDA",
      Parameter %in% c("Perfluoropentanoic acid") ~ "PFPeA",
      Parameter %in% c("Perfluorooctanoic Acid") ~ "PFOA",
      Parameter %in% c("Perfluorohexanesulfonic acid") ~ "PFHxS",
      Parameter %in% c("2-[N-ethylperfluorooctanesulfonamido] acetic acid") ~ "NEtFOSAA",
      Parameter %in% c("Perfluorodecanesulfonic acid") ~ "PFDS",
      Parameter %in% c("Perfluorododecanesulfonic acid") ~ "PFDoS",
      Parameter %in% c("Perfluorooctanesulfonic acid") ~ "PFOS",
      Parameter %in% c("N-methyl perfluorooctanesulfonamidoacetic acid") ~ "NMeFOSAA",
      Parameter %in% c("Perfluoro-3,6-dioxaheptanoic acid") ~ "NFDHA",
      Parameter %in% c("Perfluorobutanoic Acid") ~ "PFBA",
      Parameter %in% c("Perfluoroethoxyethanesulfonic acid") ~ "PFEESA",
      Parameter %in% c("Potassium 9-chlorohexadecafluoro-3-oxanonane-1-sulfonate") ~ "9Cl-PF3ONS",
      Parameter %in% c("Perfluoro-4-methoxybutanoic acid") ~ "PFMBA",
      Parameter %in% c("Perfluoro-2-methoxypropanoic acid") ~ "PFMPA",
      Parameter %in% c("11-Chloroeicosafluoro-3-oxaundecane-1-sulfonic Acid") ~ "11Cl-PF3OUdS",
      Parameter %in% c("Potassium 11-chloroeicosafluoro-3-oxaundecane-1-sulfonate") ~ "11Cl-PF3OUdS",
      Parameter %in% c("4:2 Fluorotelomer sulfonic acid") ~ "4:2FTS",
      Parameter %in% c("6:2 Fluorotelomer sulfonic acid") ~ "6:2FTS",
      Parameter %in% c("3-Perfluoropropyl propanoic acid") ~ "3:3FTCA",
      Parameter %in% c("8:2 Fluorotelomer sulfonic acid") ~ "8:2FTS",
      Parameter %in% c("N-methylperfluorooctane Sulfonamidoethanol") ~ "NMeFOSE",
      Parameter %in% c("N-ethylperfluorooctane Sulfonamidoethanol") ~ "NEtFOSE",
      Parameter %in% c("3-Perfluoroheptyl propanoic acid") ~ "7:3FTCA",
      Parameter %in% c("2H,2H,3H,3H-Perfluorooctanoic acid") ~ "5:3FTCA",
      TRUE ~ Parameter
    )
  )


#Create a variable that has the MDL values for each type of PFAS analyte
DMR_RawData_4 <- DMR_RawData_3 %>%
  mutate(
    MDL = case_when(
      PFAS_Analyte == "PFOSA" ~ 0.32,
      PFAS_Analyte == "PFHpA" ~ 0.37,
      PFAS_Analyte == "PFBS" ~ 0.37,
      PFAS_Analyte == "PFDoA" ~ 0.40,
      PFAS_Analyte == "NMeFOSA" ~ 0.43,
      PFAS_Analyte == "PFNA" ~ 0.45,
      PFAS_Analyte == "PFUnA" ~ 0.45,
      PFAS_Analyte == "NEtFOSA" ~ 0.45,
      PFAS_Analyte == "PFHxA" ~ 0.46,
      PFAS_Analyte == "PFTrDA" ~ 0.46,
      PFAS_Analyte == "PFNS" ~ 0.47,
      PFAS_Analyte == "PFTeDA" ~ 0.49,
      PFAS_Analyte == "PFPeS" ~ 0.50,
      PFAS_Analyte == "PFHpS" ~ 0.50,
      PFAS_Analyte == "ADONA" ~ 0.50,
      PFAS_Analyte == "GenX" ~ 0.51,
      PFAS_Analyte == "PFDA" ~ 0.52,
      PFAS_Analyte == "PFPeA" ~ 0.54,
      PFAS_Analyte == "PFOA" ~ 0.54,
      PFAS_Analyte == "PFHxS" ~ 0.54,
      PFAS_Analyte == "NEtFOSAA" ~ 0.59,
      PFAS_Analyte == "PFDS" ~ 0.60,
      PFAS_Analyte == "PFDoS" ~ 0.60,
      PFAS_Analyte == "PFOS" ~ 0.63,
      PFAS_Analyte == "NMeFOSAA" ~ 0.68,
      PFAS_Analyte == "NFDHA" ~ 0.75,
      PFAS_Analyte == "PFBA" ~ 0.79,
      PFAS_Analyte == "PFEESA" ~ 1.17,
      PFAS_Analyte == "9Cl-PF3ONS" ~ 1.38,
      PFAS_Analyte == "PFMBA" ~ 1.41,
      PFAS_Analyte == "PFMPA" ~ 1.46,
      PFAS_Analyte == "11Cl-PF3OUdS" ~ 1.67,
      PFAS_Analyte == "4:2FTS" ~ 1.69,
      PFAS_Analyte == "6:2FTS" ~ 2.45,
      PFAS_Analyte == "3:3FTCA" ~ 2.47,
      PFAS_Analyte == "8:2FTS" ~ 2.50,
      PFAS_Analyte == "NMeFOSE" ~ 3.81,
      PFAS_Analyte == "NEtFOSE" ~ 4.84,
      PFAS_Analyte == "7:3FTCA" ~ 8.71,
      PFAS_Analyte == "5:3FTCA" ~ 9.59
    )
  )


# 3: Substituting non-detects for zero ####

# Replace ND values with 0 and NA values with 0

# Concentration_3
DMR_RawData_5 <- DMR_RawData_4 %>%
  mutate(Concentration_3 = ifelse(
    is.na(Concentration_3),
    0.0,
    ifelse(grepl("<|NODI B", Concentration_3), 0.0, Concentration_3)
  ))

Zeroes <- DMR_RawData_5 %>% filter(DMR_RawData_5$Concentration_3 == "0")

# Concentration_2
DMR_RawData_6 <- DMR_RawData_5 %>%
  mutate(Concentration_2 = ifelse(
    is.na(Concentration_2),
    "0.0",
    ifelse(grepl("<|NODI B", Concentration_2), "0.0", Concentration_2)
  ))

# Check the replacement worked
Zeroes_2 <- DMR_RawData_6 %>% filter(DMR_RawData_6$Concentration_2 == "0" |
                                       DMR_RawData_6$Concentration_3 == "0")

# Standarize the format of zeroes
DMR_RawData_6  <- DMR_RawData_6 %>%
  mutate(Concentration_2 = case_when(
    Concentration_2 %in% c("0", "0.00", "0.000", "0.0000", "0.0000000", "0.0000000") ~ "0.0",
    TRUE ~ Concentration_2
  ))

# Standarize the format of zeroes
DMR_RawData_7  <- DMR_RawData_6 %>%
  mutate(Concentration_3 = case_when(
    Concentration_3 %in% c("0", "0.00", "0.000", "0.0000", "0.0000000", "0.0000000") ~ "0.0",
    TRUE ~ Concentration_3
  ))

# Check there arent any NA values
NAValues <- DMR_RawData_7 %>% filter(is.na(Concentration_2) |
                                       is.na(Concentration_3))

# Convert Concentration_2 and # Concentration_3 to numeric
DMR_RawData_7$Concentration_2 <- as.numeric(DMR_RawData_7$Concentration_2)
DMR_RawData_7$Concentration_3 <- as.numeric(DMR_RawData_7$Concentration_3)


# 4: Creating a results variable ####

# Create Result Variable that is the average of Concentration_2 and Concentration_3 because these are duplicate samples taken on the same day.

# Create a new variable for the average - when one of the concentrations is missing result should equal the value of the
# concentration that isn't missing.
DMR_RawData_7 <- DMR_RawData_7 %>%
  mutate(Result = ifelse(
    is.na(Concentration_2),
    Concentration_3,
    ifelse(
      is.na(Concentration_3),
      Concentration_2,
      (Concentration_2 + Concentration_3) / 2
    )
  ))




# 5: Subset the data to reduce # of columns ####


DMR_Columns <- c(
  "Facility_Name",
  "NPDES_ID",
  "Permit_Type",
  "Permittee_Address",
  "Latitude_in_Decimal_Degrees",
  "Longitude_in_Decimal_Degrees",
  "Mon._Period_End_Date",
  "Concentration_Units",
  "PFAS_Analyte",
  "Result"
)

# Filter for the columns we need to retain
DMR_RawData_8 <- DMR_RawData_7 %>%
  select(all_of(DMR_Columns))


# 6: Transpose from long to wide ####

# Transpose the dataset long to wide

# Filter data so there is one unique row for every combination of NPDES_ID, received date and parameter.
DMR_Unique <- DMR_RawData_8 %>%
  distinct(NPDES_ID,
           Facility_Name,
           Mon._Period_End_Date,
           PFAS_Analyte,
           .keep_all = TRUE)





# Make sure result is numeric before transforming
DMR_Unique$Result <- as.numeric(DMR_Unique$Result)



# Round results column to 1 decimal place
DMR_Unique$Result <- round(DMR_Unique$Result, digits = 1)


# Transpose the data from long to wide format:
# Id_Cols = specify the column(s) that you want to keep as identifiers (i.e., columns that remain as is and do not get spread)
# names_from =  specifies the column from which the values will be spread to form new columns.
# Values_from =  specifies the columns whose values will be spread into new columns (names_from columns).
library(tidyr)
library(dplyr)


# First aggregate to ensure there is one unique row for every combination of NPDES_ID, Mon._Period_End_Date, and PFAS_Analyte.
Aggregated_DMR <- DMR_Unique %>%
  group_by(NPDES_ID, Mon._Period_End_Date, PFAS_Analyte) %>%
  summarise(
    across(
      .cols = Result,
      .fns = function(x)
        ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
    ),
    across(.cols = everything(), .fns = ~ first(.))
  ) %>%
  ungroup()

glimpse(Aggregated_DMR)




# Transpose the aggregated data into wide format.

library(tidyr)

wide_data_DMR <- Aggregated_DMR %>%
  pivot_wider(
    id_cols = c(
      "NPDES_ID",
      "Permittee_Address",
      "Facility_Name",
      "Permit_Type",
      "Latitude_in_Decimal_Degrees",
      "Longitude_in_Decimal_Degrees",
      "Mon._Period_End_Date",
      "Concentration_Units"
    ),
    names_from = PFAS_Analyte,
    values_from = Result,
    values_fill = list(Result = 0)
  )


# 7: Rename and create new variables ####


# Rename variables to standardize across datasets
wide_data_DMR1 <- wide_data_DMR %>%
  rename(
    Site = Facility_Name,
    `Site ID` = NPDES_ID,
    Address = Permittee_Address,
    `Sample date` = Mon._Period_End_Date,
    Notes = Permit_Type,
    Units = Concentration_Units,
    Latitude = Latitude_in_Decimal_Degrees,
    Longitude = Longitude_in_Decimal_Degrees
  )


# Create new variables

wide_data_DMR2 <- wide_data_DMR1 %>% mutate(
  Dataset = "Permits - Data Monitoring Reports",
  Program = "CDPHE's Water Quality Control Division (WQCD) - Clean Water Program",
  Link = "https://docs.google.com/document/d/1KyRl6b-t1o73jK7mlZ8mhHn8ubbBbgZL-2hk6A1PyaA/edit",
  `Number of samples` = 2
) |> mutate(Medium = ifelse(
  Notes == "General Permit Covered Facility",
  "Groundwater",
  "Wastewater"
)) |>
  mutate(`Sum of PFOA and PFOS` = PFOA + PFOS) |>
  mutate(`Sample ID` = paste0("DMR_", 1:nrow(wide_data_DMR2)))


# 8: Assign variable types ####

# Define the groups of columns based on your data dictionary
char_cols <- c("Dataset",
               "Program",
               "Medium",
               "Site",
               "Site ID",
               "Address",
               "Link",
               "Notes",
               "Units")

numeric_cols <- c("Latitude", "Longitude", "Number of samples")

date_cols <- c("Sample date") # Note the date format is MM/DD/YYYY

# Apply the transformations using mutate() and across()
wide_data_DMR3 <- wide_data_DMR2 %>%
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


class(wide_data_DMR3$`Sample date`)


# 9: Reorder variables ####

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
  "PFBS",
  "GenX"
)


# Reorder variables using the select function
wide_data_DMR4 <- wide_data_DMR3 |> select(all_of(desired_order), everything())

# 10: Final misc. cleaning ####

# Remove Total PFAS values from
wide_data_DMR5  <- wide_data_DMR4  %>% select(-`Total PFOA and PFOS`)


# Subset wastewater and groundwater
UpdateDate_Permits_DMR_Groundwater <- wide_data_DMR5 %>% filter(Notes == "General Permit Covered Facility")
UpdateDate_Permits_DMR_Wastewater <- wide_data_DMR5 %>% filter(Notes == "NPDES Individual Permit")

# 11: Exporting ####

# Export the data frame as a cleaned and formatted dataset.

library("writexl")

write_xlsx(
  Update_DatePermits_DMR_Groundwater,
  "X:\\Shared drives\\_CDPHE TEEO Data\\_Enviro\\PFAS\\PFAS Concentration Map May 2024 Update\\PFASMap 2024_RProject\\03_Clean_Data\\Groundwater\\DMR_GW_test_new.xlsx"
)

write_xlsx(
  UpdateDate_Permits_DMR_Wastewater,
  "X:\\Shared drives\\_CDPHE TEEO Data\\_Enviro\\PFAS\\PFAS Concentration Map May 2024 Update\\PFASMap 2024_RProject\\03_Clean_Data\\Wastewater\\DMR_Wastewater.xlsx"
)
