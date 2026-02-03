# Groundwater data

library(readxl)
library(dplyr)


# 1: Importing data ####

DoD_SiteInvestigations_GW <- read_excel("03_Clean_Data/Groundwater/DoD_SiteInvestigations_Groundwater_2025.xlsx")
ECMC_GW <- read_excel("03_Clean_Data/Groundwater/ECMC_Groundwater_2025.xlsx")
PASI_GW <- read_excel("03_Clean_Data/Groundwater/PASI_Groundwater_2024.xlsx")
Permits_Sourcewater_GW <- read_excel("03_Clean_Data/Groundwater/Permits_Sourcewater_2025.xlsx")
RCRA_GW <- read_excel("03_Clean_Data/Groundwater/RCRA_Groundwater_2025.xlsx")
WQCD_GW <- read_excel("03_Clean_Data/Groundwater/WQCD_Database_GW_2025.xlsx")
WQP_GW <- read_excel("03_Clean_Data/Groundwater/WQP_GW_2025.xlsx")

str(DoD_SiteInvestigations_GW)

# 2: Merge data frames ####


# Combine them using bind_rows()
Groundwater <- bind_rows(
  DoD_SiteInvestigations_GW,
  ECMC_GW,
  PASI_GW,
  Permits_Sourcewater_GW,
  RCRA_GW,
  WQCD_GW,
  WQP_GW
)


column_names <- as.data.frame(colnames(Groundwater))

unique(Groundwater_Final$Units)

# Reorder
Groundwater_Final <- Groundwater |>  select (
  Dataset,
  Program,
  Medium,
  Site,
  `Site ID`,
  Address,
  Latitude,
  Longitude,
  Link,
  Notes,
  `Sample date`,
  `Sample ID`,
  `Number of samples`,
  `Units`,
  `Sum of PFOA and PFOS`,
  PFOA,
  PFOS,
  PFHxS,
  PFNA,
  PFBS,
  `HFPO-DA`,
  everything()
)

str(Groundwater_Final)


# 3: Export ####

library("writexl")
write_xlsx(Groundwater_Final,
           "03_Clean_Data/Groundwater/Groundwater_2025.xlsx")
