library(readxl)
library(dplyr)
library(tidyr)

# SECTION 1: Importing and reviewing the data ####

getwd()


# Importing data
Adams_Original <- read_excel("01_Raw_Data/One_Time_Efforts/2017Thru2019_AdamsCounty_GW_SW_PW.xlsx")


# SECTION 2:Cleaning the data ####


# Creating new columns based on the portal template
new_cols <- c("CDPHE_SAMPLING_NUMBER", "SITE_ID", "PWSID", "PWS_SYSTEM_NAME", "FACILITY_NAME",
              "SAMPLE_LOCATION_DESCRIPTION", "FACILITY_TYPE", "SOURCE_WATER_TYPE", "PWS_SAMPLE_LOCATION_TYPE", 
              "TREATMENT_STATUS", "NOTES_PWS_TREATMENT", "POST_POU_TREATMENT", "SAMPLE_ID", "DATE_COLLECTED", "Lab_LotID", "Lab_Methods","CAS_NUMBER", 
              "FINAL_QUALIFIER", "UNITS", "RL", "MDL", "SAMPLE_TYPE", "Sampled_By", "DWR_PERMIT_ID", "NPDES_PERMIT_ID",
              "SAMPLE_LOCATION_ADDRESS", "Latitude", "Longitude", "LabName", "COMMENTS", "TEMPFIELD")

# Creating a new column assigned to "new_cols" using the [] function and assigning NA values.
# new_cols contains a character vector
Adams_Original[new_cols] <- NA

# Assign values to variables
Adams_2 <- Adams_Original  %>%
  mutate(
    SOURCE_WATER_TYPE = Source_Water_Type,
    FACILITY_TYPE = Facility_Type,
    TREATMENT_STATUS = Treatment_Status,
    UNITS = Units,
    SAMPLE_TYPE = Sample_Type,
    COMMENTS = Name_OR_Site,
    DATE_COLLECTED = Sample_Date,
    SAMPLE_ID =  Sample_ID,
    PWS_SYSTEM_NAME = PWS_System_Name,
    SAMPLE_LOCATION_DESCRIPTION = SampleLocID,
    Latitude = Latitude_Real,
    Longitude = Longitude_Real
  ) |> select(
    -c(
      Latitude_Real,
      Longitude_Real,
      Source_Water_Type,
      Facility_Type,
      Treatment_Status,
      Units,
      Name_OR_Site,
      Sample_Type,
      Sample_Date,
      Sample_ID,
      PWS_System_Name,
      SampleLocID
    )
  )


# Reorder variables to match the portal template
Adams_3  <- Adams_2 |>  select(c("CDPHE_SAMPLING_NUMBER", "SITE_ID", "PWSID", "PWS_SYSTEM_NAME", "FACILITY_NAME",
                                   "SAMPLE_LOCATION_DESCRIPTION", "FACILITY_TYPE", "SOURCE_WATER_TYPE", "PWS_SAMPLE_LOCATION_TYPE", 
                                   "TREATMENT_STATUS", "NOTES_PWS_TREATMENT", "POST_POU_TREATMENT", "SAMPLE_ID", "DATE_COLLECTED", "Lab_LotID", "Lab_Methods","CAS_NUMBER", 
                                   "FINAL_QUALIFIER", "UNITS", "RL", "MDL", "SAMPLE_TYPE", "Sampled_By", "DWR_PERMIT_ID", "NPDES_PERMIT_ID",
                                   "SAMPLE_LOCATION_ADDRESS", "Latitude", "Longitude", "LabName", "COMMENTS", "TEMPFIELD", "PFOA", "PFOS","PFBS"))






# Section 3: Transposing to long ####

Adams_4 <- Adams_3 %>% 
  # Pivot the data to a longer format
  pivot_longer(
    cols = c(PFOA, PFOS, PFBS),
    # Overwrite the names of the two new columns
    names_to = "Component", 
    values_to = "Result"
  )


# Reorder variables to match the portal template
Adams_5  <- Adams_4  |>  select(c("CDPHE_SAMPLING_NUMBER", "SITE_ID", "PWSID", "PWS_SYSTEM_NAME", "FACILITY_NAME",
                                    "SAMPLE_LOCATION_DESCRIPTION", "FACILITY_TYPE", "SOURCE_WATER_TYPE", "PWS_SAMPLE_LOCATION_TYPE", 
                                    "TREATMENT_STATUS", "NOTES_PWS_TREATMENT", "POST_POU_TREATMENT", "SAMPLE_ID", "DATE_COLLECTED", "Lab_LotID", "Lab_Methods", "Component", "CAS_NUMBER", "Result",
                                    "FINAL_QUALIFIER", "UNITS", "RL", "MDL", "SAMPLE_TYPE", "Sampled_By", "DWR_PERMIT_ID", "NPDES_PERMIT_ID",
                                    "SAMPLE_LOCATION_ADDRESS", "Latitude", "Longitude", "LabName", "COMMENTS", "TEMPFIELD")) |> mutate(Result=as.character(Result))


# Turn ND into "ND"
Adams_6  <- Adams_5  |> mutate(Result= case_when(Result == "0"~ "ND",
                                                   TRUE ~ Result))


table(Adams_6$SAMPLE_LOCATION_DESCRIPTION)

# Section 4: Export ####
library("writexl")

#Example of exporting a final dataframe
write_xlsx(Adams_6 ,"03_Clean_Data/PFAS_Database/20017Thru2019_Adams_GW_SW_PW.xlsx")



