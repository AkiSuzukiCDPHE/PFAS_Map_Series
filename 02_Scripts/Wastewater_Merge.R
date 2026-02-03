# Wastewater data

library(readxl)
library(dplyr)


# 1: Importing data ####

WQCD_WW <- read_excel("03_Clean_Data/Wastewater/WQCD_Database_WW_2025.xlsx")
Permits_DMR_WW <- read_excel("03_Clean_Data/Wastewater/Permits_DMR_Wastewater_Date.xlsx")


# 2: Merge data frames ####


# Combine them using bind_rows()
Wastewater <- bind_rows(WQCD_WW, Permits_DMR_WW)


column_names <- as.data.frame(colnames(Wastewater))
colnames(Wastewater1)



# Reorder
Wastewater_Final <- Wastewater |>  select (
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
write_xlsx(Wastewater_Final,
           "03_Clean_Data/Wastewater/Wastewater_2025.xlsx")
