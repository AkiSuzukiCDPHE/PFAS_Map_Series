# SurfaceWater data

library(readxl)
library(dplyr)


# 1: Importing data ####

DoD_SiteInvestigations_SW <- read_excel("03_Clean_Data/SurfaceWater/DoD_SiteInvestigations_SurfaceWater_2025.xlsx")
FCA_SW <- read_excel("03_Clean_Data/SurfaceWater/PFOS_FCA_SurfaceWater_2023.xlsx")
WQCD_SW <- read_excel("03_Clean_Data/SurfaceWater/WQCD_Database_SW_2025.xlsx")
WQP_SW <- read_excel("03_Clean_Data/SurfaceWater/WQP_SW_2025.xlsx")


# 2: Merge data frames ####


# Combine them using bind_rows()
SurfaceWater <- bind_rows(DoD_SiteInvestigations_SW, FCA_SW, WQCD_SW, WQP_SW)


column_names <- as.data.frame(colnames(SurfaceWater))
colnames(SurfaceWater1)



# Reorder
SurfaceWater_Final <- SurfaceWater |>  select (
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


# 3: Export ####

library("writexl")
write_xlsx(SurfaceWater_Final,
           "03_Clean_Data/SurfaceWater/SurfaceWater_2025.xlsx")
