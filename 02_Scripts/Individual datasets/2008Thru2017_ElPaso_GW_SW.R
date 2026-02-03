library(readxl)
library(dplyr)
library(tidyr)

# SECTION 1: Importing and reviewing the data ####

getwd()


# Importing data
ElPaso_Original <- read_excel("01_Raw_Data/One_Time_Efforts/2008Thru2017_ElPaso_GW_PW.xlsx")


# SECTION 2:Cleaning the data ####


# Remove extraneous columns
ElPaso_1 <- ElPaso_Original |> select(-c(ID, Map_Key))

# Creating new columns based on the portal template
new_cols <- c("CDPHE_SAMPLING_NUMBER", "SITE_ID", "PWSID", "PWS_SYSTEM_NAME", "FACILITY_NAME",
              "SAMPLE_LOCATION_DESCRIPTION", "FACILITY_TYPE", "SOURCE_WATER_TYPE", "PWS_SAMPLE_LOCATION_TYPE", 
              "TREATMENT_STATUS", "NOTES_PWS_TREATMENT", "POST_POU_TREATMENT", "SAMPLE_ID", "DATE_COLLECTED", "Lab_LotID", "Lab_Methods", "CAS_NUMBER",
              "FINAL_QUALIFIER", "UNITS", "RL", "MDL", "SAMPLE_TYPE", "Sampled_By", "DWR_PERMIT_ID", "NPDES_PERMIT_ID",
              "SAMPLE_LOCATION_ADDRESS","LabName", "COMMENTS", "TEMPFIELD")

# Creating a new column assigned to "new_cols" using the [] function and assigning NA values.
# new_cols contains a character vector
ElPaso_1[new_cols] <- NA

# Assign values to variables
ElPaso_2 <- ElPaso_1  %>%
  mutate(
    SOURCE_WATER_TYPE = "GW",
    FACILITY_TYPE = if_else(Type == "Private Well", "PRIV", "PWS"),
    TREATMENT_STATUS = "UNF",
    UNITS = "ng/L",
    SAMPLE_TYPE = "G",
    COMMENTS = "El Paso County Sampling-Fountain Valley Communities",
    DATE_COLLECTED = Sample_Date,
    SAMPLE_ID =  `ID no PII`,
    PWSID = PWS_ID,
    SAMPLE_LOCATION_DESCRIPTION = GIS_name
  ) |> select(-c(GIS_name, Sample_Date, Type, `ID no PII`, PWS_ID))


# Reorder variables to match the portal template
ElPaso_3  <- ElPaso_2 |>  select(c("CDPHE_SAMPLING_NUMBER", "SITE_ID", "PWSID", "PWS_SYSTEM_NAME", "FACILITY_NAME",
                                   "SAMPLE_LOCATION_DESCRIPTION", "FACILITY_TYPE", "SOURCE_WATER_TYPE", "PWS_SAMPLE_LOCATION_TYPE", 
                                   "TREATMENT_STATUS", "NOTES_PWS_TREATMENT", "POST_POU_TREATMENT", "SAMPLE_ID", "DATE_COLLECTED", "Lab_LotID", "Lab_Methods","CAS_NUMBER", 
                                   "FINAL_QUALIFIER", "UNITS", "RL", "MDL", "SAMPLE_TYPE", "Sampled_By", "DWR_PERMIT_ID", "NPDES_PERMIT_ID",
                                   "SAMPLE_LOCATION_ADDRESS", "Latitude", "Longitude", "LabName", "COMMENTS", "TEMPFIELD", "PFOA", "PFOS", "PFHpA", "PFHxS", "PFNA" , "PFBS"))






# Section 3: Transposing to long ####

ElPaso_4 <- ElPaso_3 %>% 
  # Pivot the data to a longer format
  pivot_longer(
    cols = c(PFOA, PFOS, PFHpA, PFHxS, PFNA ,PFBS),
    # Overwrite the names of the two new columns
    names_to = "Component", 
    values_to = "Result"
  )


# Reorder variables to match the portal template
ElPaso_5  <- ElPaso_4  |>  select(c("CDPHE_SAMPLING_NUMBER", "SITE_ID", "PWSID", "PWS_SYSTEM_NAME", "FACILITY_NAME",
                                    "SAMPLE_LOCATION_DESCRIPTION", "FACILITY_TYPE", "SOURCE_WATER_TYPE", "PWS_SAMPLE_LOCATION_TYPE", 
                                    "TREATMENT_STATUS", "NOTES_PWS_TREATMENT", "POST_POU_TREATMENT", "SAMPLE_ID", "DATE_COLLECTED", "Lab_LotID", "Lab_Methods", "Component", "CAS_NUMBER", "Result",
                                    "FINAL_QUALIFIER", "UNITS", "RL", "MDL", "SAMPLE_TYPE", "Sampled_By", "DWR_PERMIT_ID", "NPDES_PERMIT_ID",
                                    "SAMPLE_LOCATION_ADDRESS", "Latitude", "Longitude", "LabName", "COMMENTS", "TEMPFIELD")) |> mutate(Result=as.character(Result))


# Turn ND into "ND"
ElPaso_6  <- ElPaso_5  |> mutate(Result= case_when(Result == "0"~ "ND",
                                                   TRUE ~ Result))


table(ElPaso_6$SAMPLE_LOCATION_DESCRIPTION)

# Section 4: Export ####
library("writexl")

#Example of exporting a final dataframe
write_xlsx(ElPaso_6 ,"03_Clean_Data/PFAS_Database/2008Thru2017_ElPaso_GW.xlsx")



