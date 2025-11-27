# WQCD PFAS Database


# 1: Importing the data ####

library(readxl)
library(dplyr)

getwd()

# Sub this file and file path out for new data each time
WQCD_PFAS_DB <- read_excel("01_Raw_Data/Test_Datasets/WQCD_PFAS_Databases.xlsx")


# 2: Misc. Cleaning ####


# Replace all spaces in names of variables with underscores
names(WQCD_PFAS_DB) <- gsub("\\s", "_", names((WQCD_PFAS_DB)))

glimpse(WQCD_PFAS_DB)

colnames(WQCD_PFAS_DB_1)

# Filtering the DF for untreated water
WQCD_PFAS_DB_1 <- WQCD_PFAS_DB %>% filter (TreatmentStatus == "UNF")


# Replacing "ND" with 0

PFAS_Analytes <- c(
  "_11Cl_PF3OUdS",
  "_3_3a_3_20_FTCA",
  "_4_3a_2_20_FTS",
  "_5_3_20_FTCA",
  "_6_2FTS",
  "_7_3a_3_20_FTCA",
  "_8_3a_2_20_FTS",
  "_9Cl_2d_PF3ONS",
  "ADONA",
  "FOSA",
  "GenX",
  "NEtFOSAA",
  "NMeFOSA",
  "NMeFOSAA",
  "NEtFOSA",
  "NEtFOSE",
  "NFDHA",
  "NMeFOSE",
  "PFOA",
  "PFOS",
  "PFBA",
  "PFBS",
  "PFDA",
  "PFNS",
  "PFNA",
  "PFHxA",
  "PFHxS",
  "PFDS",
  "PFDoS",
  "PFDoA",
  "PFHpS",
  "PFHpA",
  "PFEESA",
  "PFMBA",
  "PFMPA",
  "PFPeS",
  "PFPeA",
  "PFTeA",
  "PFTriA",
  "PFUnA"
)


WQCD_PFAS_DB_1 <- WQCD_PFAS_DB_1 %>%
  mutate_at(vars(all_of(PFAS_Analytes)), ~ ifelse(.=="null" | . == "ND", 0, .))

# Transforming all PFAS analytes to be numeric variables
WQCD_PFAS_DB_1 <- WQCD_PFAS_DB_1 %>%
  mutate_at(vars(all_of(PFAS_Analytes)), ~ ifelse(. == "null", NA, .))

WQCD_PFAS_DB_1 <- WQCD_PFAS_DB_1 %>%
  mutate_at(vars(all_of(PFAS_Analytes)), ~ as.numeric(., na.rm = TRUE))


# Jitter any samples where Facility type = "PWS"

# set.seed(123): Ensures the randomness is reproducible.
# runif(nrow(JitterTest), -JitterAmount, JitterAmount): Generates random values uniformly distributed between -JitterAmount and JitterAmount.
# This approach directly adds the desired jitter to each latitude value within the specified range.


# Jitter the latitude where .007245 degrees latitude = .5 miles
set.seed(123) # for reproducibility
LatitudeJitterAmount <- 0.007245
WQCD_PFAS_DB_1_Jitter <- WQCD_PFAS_DB_1 %>%
  mutate(
    Latitude= if_else(
      Facility_Type == "PWS",
      Latitude + runif(n(), -LatitudeJitterAmount, LatitudeJitterAmount),
      Latitude
    )
  )



# Jitter the longitude where 0.00925 degrees longitude = .5 miles
set.seed(123) # for reproducibility
LongitudeJitterAmount <- 0.00925
WQCD_PFAS_DB_1_Jitter <- WQCD_PFAS_DB_1_Jitter %>%
  mutate(
    Longitude = if_else(
      Facility_Type == "PWS",
      Longitude + runif(n(), -LongitudeJitterAmount, LongitudeJitterAmount),
      Longitude
    )
  )




# Subsetting to a surface Water dataset - Source Water Type = "SW"
WQCD_PFAS_DB_SW <- WQCD_PFAS_DB_1_Jitter %>%
  filter(Source_Water_Type == "SW")


# Creating a variable for identifying whether a sample is from a public water system
WQCD_PFAS_DB_SW <- WQCD_PFAS_DB_SW %>%
  mutate(Public_Water_System = case_when(
    Facility_Type == "PWS" ~ "Yes",
    TRUE ~ "No"
  ))


# Rename variables to standardize across data sets, create missing variables, and remove extraneous variables
WQCD_PFAS_DB_SW1 <- WQCD_PFAS_DB_SW %>%
  rename(
    Sample_Date = Date_Collected,
    Name_OR_Site = PWS_System_Name,
    Sample_Location = Facility_Name
  ) %>%
  mutate(
    Data_Source = "PFAS Portal",
    Media = "Surface water",
    Permittee = NA,
    NAICS_Code = NA,
    Confidential_Location = NA,
    Address = NA,
    Link_More_Info = "https://cdphe.colorado.gov/pfas-water",
    Notes = Sample_Location_Description,
    Number_Samples = 1,
    Units = "ng/L"
  ) %>%
  select(-c(CDPHE_Sample_Number, PWS_Sample_Location_Type, Source_Water_Type, Treatment_Status, Sample_Location_Description))

# Define the function to reorder columns
reorder_columns <- function(df, desired_order) {
  # Use select with all_of to specify desired columns and everything to include the rest
  df <- df %>%
    select(all_of(desired_order), everything())
  
  return(df)
}

# Create vector with order of columns
desired_order <- c("Data_Source", "Media", "Name_OR_Site", "Permittee", "NAICS_Code", 
                   "Address", "Latitude", "Longitude", "Confidential_Location", 
                   "Link_More_Info", "Notes", "Sample_Date", "Number_Samples", "Units", "Public_Water_System")


# reorder columns
Portal_05.08_SW <- reorder_columns(WQCD_PFAS_DB_SW1, desired_order)



# Exporting the surface water dataset
library("writexl")
write_xlsx(Portal_05.08_SW,"X:\\Shared drives\\_CDPHE TEEO Data\\_Enviro\\PFAS\\PFAS Concentration Map May 2024 Update\\PFASMap 2024_RProject\\03_Clean_Data\\Surface water\\Portal_05.08_SW.xlsx")



# Subsetting all groundwater data including groundwater from private wells
# “GROUNDWATER” means any water under the surface of the ground that is not surface water or groundwater under the direct influence of surface water (GU).
WQCD_PFAS_DB_GW <- WQCD_PFAS_DB_1_Jitter %>%
  filter(Source_Water_Type %in% c("GW", "GU"))

# Subsetting again for all groundwater that are NOT private wells
WQCD_PFAS_DB_OtherGW <- WQCD_PFAS_DB_GW %>%
  filter(!Facility_Type == "PRIV")

# Creating a variable for identifying whether a sample is from a public water system
WQCD_PFAS_DB_OtherGW  <- WQCD_PFAS_DB_OtherGW  %>%
  mutate(Public_Water_System = case_when(
    Facility_Type == "PWS" ~ "Yes",
    TRUE ~ "No"
  ))

# Other groundwater samples that are not private wells

# Rename variables to standardize across data sets, create missing variables, and remove extraneous variables
WQCD_PFAS_DB_OtherGW1 <- WQCD_PFAS_DB_OtherGW %>%
  rename(
    Sample_Date = Date_Collected,
    Name_OR_Site = PWS_System_Name,
    Sample_Location = Facility_Name
  ) %>%
  mutate(
    Data_Source = "PFAS Portal",
    Media = "Groundwater",
    Permittee = NA,
    NAICS_Code = NA,
    Confidential_Location = NA,
    Address = NA,
    Link_More_Info = "https://cdphe.colorado.gov/pfas-water",
    Note = Sample_Location_Description,
    Number_Samples = 1,
    Units = "ng/L"
  ) %>%
  select(-c(CDPHE_Sample_Number, PWS_Sample_Location_Type, Source_Water_Type, Treatment_Status, Sample_Location_Description))


# Define the function to reorder columns
reorder_columns <- function(df, desired_order) {
  # Use select with all_of to specify desired columns and everything to include the rest
  df <- df %>%
    select(all_of(desired_order), everything())
  
  return(df)
}

# Create vector with order of columns
desired_order <- c("Data_Source", "Media", "Name_OR_Site", "Permittee", "NAICS_Code", 
                   "Address", "Latitude", "Longitude", "Confidential_Location", 
                   "Link_More_Info", "Note", "Sample_Date", "Number_Samples", "Units", "Public_Water_System")


# reorder columns
Portal_05.08_GW <- reorder_columns(WQCD_PFAS_DB_OtherGW1, desired_order)





# Export the groundwater data from the PFAS portal
library("writexl")
write_xlsx(Portal_05.08_GW,"X:\\Shared drives\\_CDPHE TEEO Data\\_Enviro\\PFAS\\PFAS Concentration Map May 2024 Update\\PFASMap 2024_RProject\\03_Clean_Data\\Groundwater\\Portal_05.08_GW.xlsx")





# Private well data only

# Subsetting private well data
WQCD_PFAS_DB_PrivateWells <- WQCD_PFAS_DB_GW %>%
  filter(Facility_Type == "PRIV")


# Rename variables to standardize across data sets, create missing variables, and remove extraneous variables
WQCD_PFAS_DB_PrivateWells1 <- WQCD_PFAS_DB_PrivateWells %>%
  rename(
    Sample_Date = Date_Collected,
    Address = Facility_Name,
    Sample_Location = PWS_System_Name
  ) %>%
  mutate(
    Data_Source = "PFAS Portal",
    Media = "Private Well",
    Name_OR_Site=NA,
    Confidential_Location = "Yes",
    Link_More_Info = "https://cdphe.colorado.gov/pfas-projects",
    Notes = Sample_Location_Description,
    Number_Samples = 1,
    Units = "ng/L",
    Analysis_Method = "EPA Method"
  ) %>%
  select(-c(CDPHE_Sample_Number, PWS_Sample_Location_Type, Facility_Type, PWSID, Source_Water_Type, Treatment_Status, Sample_Location_Description))


# Create a new variable Hazard Index using the HI formula.
WQCD_PFAS_DB_PrivateWells1$Hazard_Index <- (WQCD_PFAS_DB_PrivateWells1$PFNA/10)+(WQCD_PFAS_DB_PrivateWells1$PFHxS/9) +(WQCD_PFAS_DB_PrivateWells1$PFBS/2000) + (WQCD_PFAS_DB_PrivateWells1$GenX/10)

WQCD_PFAS_DB_PrivateWells_Final <- WQCD_PFAS_DB_PrivateWells1

# Exporting the private well data
library("writexl")
write_xlsx(WQCD_PFAS_DB_PrivateWells_Final,"X:\\Shared drives\\_CDPHE TEEO Data\\_Enviro\\PFAS\\PFAS Concentration Map May 2024 Update\\PFASMap 2024_RProject\\03_Clean_Data\\Private Well Data\\Portal_PW_05.08.xlsx")


