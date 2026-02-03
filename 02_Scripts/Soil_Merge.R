# Soil data

library(readxl)
library(dplyr)


# 1: Importing data ####

DoD_SiteInvestigations_Soil <- read_excel("03_Clean_Data/Soil/DoD_SiteInvestigations_Soil_2025.xlsx")
FCA_Soil <- read_excel("03_Clean_Data/Soil/Mines_Soil_2024.xlsx")


# 2: Merge data frames ####


# Combine them using bind_rows()
Soil <- bind_rows(DoD_SiteInvestigations_Soil, FCA_Soil)


column_names <- as.data.frame(colnames(Soil))
colnames(Soil)



# Reorder
Soil_Final <- Soil |>  select (
  Dataset,
  Program,
  Medium,
  Site,
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
write_xlsx(Soil_Final, "03_Clean_Data/Soil/Soil_2025.xlsx")
