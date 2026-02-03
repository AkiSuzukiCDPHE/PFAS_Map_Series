library(readxl)
library(dplyr)
library(tidyr)

# SECTION 1: Importing and reviewing the data ####

getwd()


# Importing data
Sugarloaf_Original <- read_excel("01_Raw_Data/One_Time_Efforts/2018_Sugarloaf_FireDistrict_PW.xlsx")


# SECTION 2:Cleaning the data ####


# Creating new columns based on the portal template
new_cols <- c("CDPHE_SAMPLING_NUMBER", "SITE_ID", "PWSID", "PWS_SYSTEM_NAME", "FACILITY_NAME",
              "SAMPLE_LOCATION_DESCRIPTION", "FACILITY_TYPE", "SOURCE_WATER_TYPE", "PWS_SAMPLE_LOCATION_TYPE", 
              "TREATMENT_STATUS", "NOTES_PWS_TREATMENT", "POST_POU_TREATMENT", "SAMPLE_ID", "DATE_COLLECTED", "Lab_LotID", "Lab_Methods", "CAS_NUMBER",
              "FINAL_QUALIFIER", "UNITS", "RL", "MDL", "SAMPLE_TYPE", "Sampled_By", "DWR_PERMIT_ID", "NPDES_PERMIT_ID",
              "SAMPLE_LOCATION_ADDRESS","LabName", "COMMENTS", "TEMPFIELD")

# Creating a new column assigned to "new_cols" using the [] function and assigning NA values.
# new_cols contains a character vector
Sugarloaf_Original[new_cols] <- NA

colnames(Sugarloaf_Original)

# Assign values to variables
Sugarloaf_2 <- Sugarloaf_Original  %>%
  mutate(
    SOURCE_WATER_TYPE = "GW",
    FACILITY_TYPE = "PRIV",
    TREATMENT_STATUS = "UNF",
    UNITS = "ng/L",
    SAMPLE_TYPE = "G",
    FACILITY_NAME = Name_OR_Site,
    DATE_COLLECTED = Date,
    SAMPLE_ID =  `Sample ID`,
    PFOS =`Max of PFOS`, 
    PFOA = `Max of PFOA`,
    PFHxS = `Max of PFHxS`,
    PFHxA =`Max of PFHxA`,
    PFBS =`Max of PFBS`,
    COMMENTS = "PFAS results represent the maximum of multiple samples taken from the same site."
  ) |> select(-c(Date:Address,`Data Source`:`Number of Samples`, `Sum PFOS PFOA`, `Sum PFAS`, Confidential))


# Reorder variables to match the portal template
Sugarloaf_3  <- Sugarloaf_2 |>  select(c("CDPHE_SAMPLING_NUMBER", "SITE_ID", "PWSID", "PWS_SYSTEM_NAME", "FACILITY_NAME",
                                   "SAMPLE_LOCATION_DESCRIPTION", "FACILITY_TYPE", "SOURCE_WATER_TYPE", "PWS_SAMPLE_LOCATION_TYPE", 
                                   "TREATMENT_STATUS", "NOTES_PWS_TREATMENT", "POST_POU_TREATMENT", "SAMPLE_ID", "DATE_COLLECTED", "Lab_LotID", "Lab_Methods","CAS_NUMBER", 
                                   "FINAL_QUALIFIER", "UNITS", "RL", "MDL", "SAMPLE_TYPE", "Sampled_By", "DWR_PERMIT_ID", "NPDES_PERMIT_ID",
                                   "SAMPLE_LOCATION_ADDRESS", "Latitude", "Longitude", "LabName", "COMMENTS", "TEMPFIELD", "PFOA", "PFOS", "PFHxS", "PFHxA" , "PFBS"))






# Section 3: Transposing to long ####

Sugarloaf_4 <- Sugarloaf_3 %>% 
  # Pivot the data to a longer format
  pivot_longer(
    cols = c("PFOA", "PFOS", "PFHxS", "PFHxA" , "PFBS"),
    # Overwrite the names of the two new columns
    names_to = "Component", 
    values_to = "Result"
  )


# Reorder variables to match the portal template
Sugarloaf_5  <- Sugarloaf_4  |>  select(c("CDPHE_SAMPLING_NUMBER", "SITE_ID", "PWSID", "PWS_SYSTEM_NAME", "FACILITY_NAME",
                                    "SAMPLE_LOCATION_DESCRIPTION", "FACILITY_TYPE", "SOURCE_WATER_TYPE", "PWS_SAMPLE_LOCATION_TYPE", 
                                    "TREATMENT_STATUS", "NOTES_PWS_TREATMENT", "POST_POU_TREATMENT", "SAMPLE_ID", "DATE_COLLECTED", "Lab_LotID", "Lab_Methods", "Component", "CAS_NUMBER", "Result",
                                    "FINAL_QUALIFIER", "UNITS", "RL", "MDL", "SAMPLE_TYPE", "Sampled_By", "DWR_PERMIT_ID", "NPDES_PERMIT_ID",
                                    "SAMPLE_LOCATION_ADDRESS", "Latitude", "Longitude", "LabName", "COMMENTS", "TEMPFIELD")) |> mutate(Result=as.character(Result))


# Turn ND into "ND"
Sugarloaf_6  <- Sugarloaf_5  |> mutate(Result= case_when(Result == "0"~ "ND",
                                                   TRUE ~ Result))


table(Sugarloaf_6$SAMPLE_LOCATION_DESCRIPTION)

# Section 4: Export ####
library("writexl")

#Example of exporting a final dataframe
write_xlsx(Sugarloaf_6 ,"03_Clean_Data/PFAS_Database/2018_Sugarloaf_Fire_District_PW.xlsx")



