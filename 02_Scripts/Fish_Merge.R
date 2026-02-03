# Fish data

library(readxl)
library(dplyr)


# 1: Importing data ####

FCA_Program <- read_excel("03_Clean_Data/FishTissue/CDPHE_PFOS_FCA_FishTissue_Ongoing.xlsx")
FCA_Pilot <- read_excel("03_Clean_Data/FishTissue/FCA_Pilot_FishTissue_2020.xlsx")
WQP_Fish <- read_excel("03_Clean_Data/FishTissue/WQP_Fish_2025.xlsx")





# 2: Merge data frames ####


# Combine them using bind_rows()
Fish <- bind_rows(FCA_Program, FCA_Pilot, WQP_Fish)


column_names <- as.data.frame(colnames(Fish))
colnames(Fish)



# Reorder
Fish_Final <- Fish |>  select (
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
  Species,
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
write_xlsx(Fish_Final, "03_Clean_Data/FishTissue/Fish_2025.xlsx")
