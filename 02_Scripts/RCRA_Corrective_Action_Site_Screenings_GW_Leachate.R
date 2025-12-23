# Corrective Action Site Screening
# GROUNDWATER AND LEACHATE

# 1: Importing the data ####

library(readxl)
library(dplyr)

# Importing Data
RCRA_Raw <- read_excel("X:\\Shared drives\\_CDPHE TEEO Data\\_Enviro\\PFAS\\PFAS Concentration Map May 2024 Update\\PFASMap 2024_RProject\\01_Raw_Data\\RCRA PFAS Results V.April2024.xlsx")


# 2: Misc Data Cleaning ####

# Replace all spaces in names of variables with underscores
names(RCRA_Raw) <- gsub("\\s", "_", names((RCRA_Raw)))


# Format Date Min and Date Max to be a date variable
RCRA_Raw$Date_Min <- as.Date(RCRA_Raw$Date_Min, format = "%m/%d/%Y")
RCRA_Raw$Date_Max <- as.Date(RCRA_Raw$Date_Max, format = "%m/%d/%Y")

# Create vector of PFAS analytes
PFAS_Analytes <- c("Max_PFOS_and_PFOA","Max_PFOS","Max_PFOA","Max_PFBS","Max_GenX","Max_PFHxS","Max_PFDA","Max_PFNA")

# Reclassify all PFAS analytes as numeric variables
# Reclassify all PFAS analytes as numeric variables and round to 1 decimal place
RCRA_Clean <- RCRA_Raw %>%
  mutate_at(vars(all_of(PFAS_Analytes)), ~ifelse(is.na(.), NA, round(as.numeric(.), 1)))

# Replacing blanks with "0" because blank representst ND
RCRA_Clean <- RCRA_Clean %>%
  mutate_at(vars(all_of(PFAS_Analytes)), ~ ifelse(is.na(.), 0, .))


# 3: Rename and create new variables ####

# Rename variables to standardize across datasets, create missing variables, and remove extraneous variables
RCRA_Clean1 <- RCRA_Clean %>%
  rename(Medium= Sample_Type,
         Site = Site_Name,
         `Number of samples` = Numbr_of_Samples,
         `Sample date`= Date_Max,
         `Site ID` = NAICS_Code,
         Confidential_Location= `Confidential_Location?`,
         PFOS=Max_PFOS,
         PFOA=Max_PFOA,
         PFBS=Max_PFBS,
         `HFPO-DA`=Max_GenX,
         PFHxS=Max_PFHxS,
         PFDA=Max_PFDA,
         PFNA=Max_PFNA) %>%
  mutate(Dataset = "Corrective Action Site Screening - RCRA",
         Program = "CDPHE's Hazardous Materials and Waste Management Division",
         Notes = "PFAS results represent the maximum of multiple samples taken from the same site.",
         Link = "https://oitco.hylandcloud.com/CDPHERMPublicAccess/index.html",
         `Sum of PFOA and PFOS`= PFOS + PFOA) |> 
  select(-c(Notes_on_Data_Source_1, Notes_on_Data_Source_2, Notes_on_Data_Source_3, 
            `Internal_Only-Date_Updated`, Street_Address, Confidential_Location, Link_to_Pub_Records_Search, State, City, County, Zip, Max_PFOS_and_PFOA, EPA_Number, Date_Min))



# 4: Assign variable types ####

glimpse(RCRA_Clean1)

# Define the groups of columns based on your data dictionary
char_cols <- c("Dataset",
               "Program",
               "Medium",
               "Site",
               "Site ID",
               "Link",
               "Notes",
               "Units")

numeric_cols <- c("Latitude", "Longitude", "Number of samples")

date_cols <- c("Sample date") # Note the date format is MM/DD/YYYY

# Apply the transformations using mutate() and across()
RCRA_Clean2<- RCRA_Clean1%>%
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


class(RCRA_Clean2$`Sample date`)


# Format  all PFAS Columns as numeric and round to 1 decimal place

PFAS_Analytes <- c("PFOS", "PFOA", "PFBS", "HFPO-DA", "PFHxS", "PFDA", "PFNA", "Sum of PFOA and PFOS")

# Round to one decimal place
RCRA_Clean3 <- RCRA_Clean2 %>%
  mutate(across(all_of(PFAS_Analytes), ~ round(as.numeric(.), 1)))



# 5: Reorder variables ####


# Create vector wth order of columns
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
RCRA_Clean4 <- RCRA_Clean3 |> select(all_of(desired_order), everything())


# 6: Subset by media
RCRA_Groundwater_2024 <- RCRA_Clean4 %>% filter(Medium=="Groundwater")
RCRA_Leachate_2024 <- RCRA_Clean4 %>% filter(Medium=="Leachate")

# Exporting ####

# Export the data frame as a cleaned and formatted dataset.

library("writexl")
write_xlsx(RCRA_Groundwater_2024,"03_Clean_Data/Groundwater/RCRA_Groundwater_2025.xlsx")

write_xlsx(RCRA_Leachate_2024, "03_Clean_Data/Leachate/RCRA_Leachate_2025.xlsx")

