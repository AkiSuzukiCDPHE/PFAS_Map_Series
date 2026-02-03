# Sediment data

library(readxl)
library(dplyr)


# 1: Importing data ####

DoD_SiteInvestigations_Sediment <- read_excel("03_Clean_Data/Sediment/DoD_SiteInvestigations_Sediment_2025.xlsx")
FCA_Sediment <- read_excel("03_Clean_Data/Sediment/PFOS_FCA_Sediment_2023.xlsx")


# 2: Merge data frames ####


# Combine them using bind_rows()
Sediment <- bind_rows(DoD_SiteInvestigations_Sediment, FCA_Sediment)


column_names <- as.data.frame(colnames(Sediment))
colnames(Sediment)



# Reorder
Sediment_Final <- Sediment |>  select (
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
write_xlsx(Sediment_Final, "03_Clean_Data/Sediment/Sediment_2025.xlsx")
