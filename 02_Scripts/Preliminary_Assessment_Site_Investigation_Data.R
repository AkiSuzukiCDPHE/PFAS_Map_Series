# PRELIMINARY ASSESSMENT AND SITE INVESTIGATION.


library(readxl)
library(dplyr)

# Importing Data for Norge ####

NorgePFASdata <- read_excel("X:/Shared drives/_CDPHE TEEO Data/_Enviro/PFAS/PFAS Concentration Map May 2024 Update/PFASMap 2024_RProject/01_Raw_Data/PASI Data/NorgePFASdata.xlsx")


# 2: Misc cleaning for Norge ####

# Replace all spaces in names of variables with underscores
names(NorgePFASdata) <- gsub("\\s", "_", names((NorgePFASdata)))


# Filter for unique only
NorgePFASdata1 <- NorgePFASdata %>%
  group_by(Sample_ID, PFAS_Analyte) %>%
  distinct() %>%
  ungroup()


# Replace non-detects in the dataset with 0
NorgePFASdata1 <-NorgePFASdata %>%
  mutate(Result = ifelse(is.na(Result), NA,
                         ifelse(grepl("<", Result), 0, Result)))


# Converting results to numeric
NorgePFASdata1$Result <- as.numeric(NorgePFASdata1$Result)


# Converting the MDL to numeric
NorgePFASdata1$MDL <- as.numeric(NorgePFASdata1$MDL)


table(NorgePFASdata1$PFAS_Analyte)



# 3: Importing Data for TGap #### 


TGapPFASdata <- read_excel("X:/Shared drives/_CDPHE TEEO Data/_Enviro/PFAS/PFAS Concentration Map May 2024 Update/PFASMap 2024_RProject/01_Raw_Data/PASI Data/TGapPFASdata.xlsx")


# 4: Misc Cleaning for TGap #### 


# Replace all spaces in names of variables with underscores
names(TGapPFASdata) <- gsub("\\s", "_", names((TGapPFASdata)))


# Filter for unique only
TGapPFASdata <- TGapPFASdata %>%
  group_by(Sample_ID, PFAS_Analyte) %>%
  distinct() %>%
  ungroup()

# Replace non-detects in the dataset with 0
TGapPFASdata1 <-TGapPFASdata %>%
  mutate(Result = ifelse(is.na(Result), NA,
                         ifelse(grepl("10", Result), 0, Result)))

# Converting results to numeric
TGapPFASdata1$Result <- as.numeric(TGapPFASdata1$Result)


# 5: Binding data from multiple sites#### 


# Binding the two dry cleaning datasets together
Dry_Cleaning_PASI <- bind_rows(TGapPFASdata1, NorgePFASdata1)

# Rename pfas analytes

Dry_Cleaning_PASI <- Dry_Cleaning_PASI |> mutate(PFAS_Analyte=case_when(PFAS_Analyte == "PFUdA" ~ "PFUnA",
                                                                        PFAS_Analyte == "PFDoA" ~ "PFDoDA",
                                                                        PFAS_Analyte == "FOSA" ~ "PFOSA",
                                                                        TRUE~PFAS_Analyte))

# 6: Transform wide #### 


# Make sure result is numeric before transforming
Dry_Cleaning_PASI$Result <- as.numeric(Dry_Cleaning_PASI$Result)


# Round results column to 1 decimal place
Dry_Cleaning_PASI$Result <- round(Dry_Cleaning_PASI$Result, digits = 1)


# Remove extraneous variables. This is necessary before pivotting wide because some of these variables have
# unique values within 1 Sample_ID that result in multiple columns per Sample_ID. 
Dry_Cleaning_PASI1 <- Dry_Cleaning_PASI %>% select(-Analyte, -MDL, -MRL, -CAS_No, -Val_Qual)


library(tidyr)

# Tranforming from long to wide format.
Dry_Cleaning_PASIWide <- Dry_Cleaning_PASI1 %>%
  pivot_wider(
    id_cols = c("Sample_ID", "Units",
                "Data_Source", "Latitude", "Longitude", "Sample_Date", 
                "Remarks_Notes"),
    names_from = PFAS_Analyte,
    values_from = Result,
  )



# 7: Rename and create new variables #### 


# Rename variables
Dry_Cleaning_PASIWide1 <- Dry_Cleaning_PASIWide %>%
  rename(Notes = Remarks_Notes,
         `Site ID` = Data_Source,
         `Sample date` = Sample_Date,
         `Sample ID`= Sample_ID,
         PFDS=`L-PFDS`,
         PFOS=`L-PFOS`,
         PFDoS=`L-PFDoS`,
         PFHpS=`L-PFHpS`,
         PFPeS=`L-PFPeS`,
         PFHxS=`L-PFHxS`,
         PFNS=`L-PFNS`,
         PFBS=`L-PFBS`,
  ) %>%
  mutate(Dataset = "Preliminary Assessment and Site Investigation Data",
         Program = "CDPHE's Hazardous Materials and Waste Management Division",
         Medium = "Groundwater",
         `Number of samples` = 1) |> 
  mutate(Site = case_when(
    `Site ID` == "Tgap" ~ "Templeton GAP Landfill Site",
    `Site ID` == "Norge" ~ "Norge Laundry and Cleaning Village Site")) |> 
  mutate(Link = case_when(
    `Site ID` == "Tgap" ~ "For more information contact jennifer.charles@state.co.us",
    `Site ID` == "Norge" ~ "For more information contact alex.hedgepath@state.co.us")) |> 
  mutate(`Sum of PFOA and PFOS` = PFOA + PFOS) 



# 8: Assign variable types ####

glimpse(Dry_Cleaning_PASIWide1)

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
Dry_Cleaning_PASIWide2<- Dry_Cleaning_PASIWide1%>%
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


class(Dry_Cleaning_PASIWide2$`Sample date`)



# 9: Reorder variables ####

# Create vector wth order of columns
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
  "PFBS",
  "HFPO-DA"
)



# Reorder variables using the select function
Dry_Cleaning_PASIWide3 <- Dry_Cleaning_PASIWide2 |> select(all_of(desired_order), everything())


# Rename the dataset

PASI_Groundwater_2024 <- Dry_Cleaning_PASIWide3

# 10: Export ####

# Export the data frame as a cleaned and formatted dataset.

library("writexl")
write_xlsx(PASI_Groundwater_2024, "03_Clean_Data/Groundwater/PASI_Groundwater_2024.xlsx")


