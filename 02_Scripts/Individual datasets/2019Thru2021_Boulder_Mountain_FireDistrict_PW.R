library(readxl)
library(dplyr)
library(tidyr)

# SECTION 1: Importing and reviewing the data ####

getwd()


# Importing data
BoulderMountain_Original <- read_excel("01_Raw_Data/One_Time_Efforts/2019Thru2021_Boulder_Mountain_FireDistrict_PW.xlsx")


# SECTION 2:Cleaning the data ####


# Remove extraneous columns
BoulderMountain_1 <- BoulderMountain_Original |> select(-c(`Number of Samples`, Type, Units,`Max PFOS and PFOA`, `Data Source`, Confidential))

# Creating new columns based on the portal template
new_cols <- c("CDPHE_SAMPLING_NUMBER", "SITE_ID", "PWSID", "PWS_SYSTEM_NAME", "FACILITY_NAME",
              "SAMPLE_LOCATION_DESCRIPTION", "FACILITY_TYPE", "SOURCE_WATER_TYPE", "PWS_SAMPLE_LOCATION_TYPE", 
              "TREATMENT_STATUS", "NOTES_PWS_TREATMENT", "POST_POU_TREATMENT", "SAMPLE_ID", "DATE_COLLECTED", "Lab_LotID", "Lab_Methods", "CAS_NUMBER",
              "FINAL_QUALIFIER", "UNITS", "RL", "MDL", "SAMPLE_TYPE", "Sampled_By", "DWR_PERMIT_ID", "NPDES_PERMIT_ID",
              "SAMPLE_LOCATION_ADDRESS","LabName", "COMMENTS", "TEMPFIELD")

# Creating a new column assigned to "new_cols" using the [] function and assigning NA values.
# new_cols contains a character vector
BoulderMountain_1[new_cols] <- NA

# Assign values to variables
BoulderMountain_2 <- BoulderMountain_1  %>%
  mutate(
    SOURCE_WATER_TYPE = "GW",
    FACILITY_TYPE = "PRIV",
    TREATMENT_STATUS = "UNF",
    UNITS = "ng/L",
    SAMPLE_TYPE = "G",
    FACILITY_NAME = Name_OR_Site,
    DATE_COLLECTED = Date,
    SAMPLE_ID =  `Sample ID`,
    SAMPLE_LOCATION_ADDRESS = Address,
  ) |> select(-c(Date, Address, Name_OR_Site, `Sample ID`)) |>  rename (PFTeA= PFTA)


# Reorder variables to match the portal template
BoulderMountain_3  <- BoulderMountain_2 |>  select(
  c(
    "CDPHE_SAMPLING_NUMBER",
    "SITE_ID",
    "PWSID",
    "PWS_SYSTEM_NAME",
    "FACILITY_NAME",
    "SAMPLE_LOCATION_DESCRIPTION",
    "FACILITY_TYPE",
    "SOURCE_WATER_TYPE",
    "PWS_SAMPLE_LOCATION_TYPE",
    "TREATMENT_STATUS",
    "NOTES_PWS_TREATMENT",
    "POST_POU_TREATMENT",
    "SAMPLE_ID",
    "DATE_COLLECTED",
    "Lab_LotID",
    "Lab_Methods",
    "CAS_NUMBER",
    "FINAL_QUALIFIER",
    "UNITS",
    "RL",
    "MDL",
    "SAMPLE_TYPE",
    "Sampled_By",
    "DWR_PERMIT_ID",
    "NPDES_PERMIT_ID",
    "SAMPLE_LOCATION_ADDRESS",
    "Latitude",
    "Longitude",
    "LabName",
    "COMMENTS",
    "TEMPFIELD",
    "PFOA",
    "PFOS",
    "PFHpA",
    "PFHxS",
    "PFNA" ,
    "PFBS",
    "PFHxA",
    "PFTeA",
    "PFDA",
    "PFUnA",
    "NMeFOSAA",
    "NEtFOSAA",
    "PFDoA",
    "PFTrDA",
    "PFNA"
  ))






# Section 3: Transposing to long ####

BoulderMountain_4 <- BoulderMountain_3 %>% 
  # Pivot the data to a longer format
  pivot_longer(
    cols = c("PFOA", "PFOS", "PFHpA", "PFHxS", "PFNA" , "PFBS",
             "PFHxA", "PFTeA", "PFDA", "PFUnA", "NMeFOSAA", "NEtFOSAA", "PFDoA", "PFTrDA", "PFNA"),
    # Overwrite the names of the two new columns
    names_to = "Component", 
    values_to = "Result"
  )


# Reorder variables to match the portal template
BoulderMountain_5  <- BoulderMountain_4  |>  select(c("CDPHE_SAMPLING_NUMBER", "SITE_ID", "PWSID", "PWS_SYSTEM_NAME", "FACILITY_NAME",
                                    "SAMPLE_LOCATION_DESCRIPTION", "FACILITY_TYPE", "SOURCE_WATER_TYPE", "PWS_SAMPLE_LOCATION_TYPE", 
                                    "TREATMENT_STATUS", "NOTES_PWS_TREATMENT", "POST_POU_TREATMENT", "SAMPLE_ID", "DATE_COLLECTED", "Lab_LotID", "Lab_Methods", "Component", "CAS_NUMBER", "Result",
                                    "FINAL_QUALIFIER", "UNITS", "RL", "MDL", "SAMPLE_TYPE", "Sampled_By", "DWR_PERMIT_ID", "NPDES_PERMIT_ID",
                                    "SAMPLE_LOCATION_ADDRESS", "Latitude", "Longitude", "LabName", "COMMENTS", "TEMPFIELD")) |> mutate(Result=as.character(Result))


# Replacing non-detects with "ND"
BoulderMountain_6 <- BoulderMountain_5 |> 
  mutate(Result = if_else(grepl("<", Result), "ND", as.character(Result)))
                                                   
                                                   


table(BoulderMountain_6$SAMPLE_LOCATION_DESCRIPTION)

# Section 4: Export ####
library("writexl")

#Example of exporting a final dataframe
write_xlsx(BoulderMountain_6 ,"03_Clean_Data/PFAS_Database/2008Thru2017_BoulderMountain_PW.xlsx")



