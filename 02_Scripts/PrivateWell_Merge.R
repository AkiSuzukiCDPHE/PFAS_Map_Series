# Private Well data

library(readxl)
library(dplyr)


# 1: Importing data ####

DoD_OffBase_PW <- read_excel("03_Clean_Data/PrivateWell/DoD_OffBase_PrivateWells_2018Thru2020.xlsx")
TAP_PW <- read_excel("03_Clean_Data/PrivateWell/PFAS_TAP_PW_121525.xlsx")
WQCD_PW <- read_excel("03_Clean_Data/PrivateWell/WQCD_Database_PW_2025.xlsx")

str(WQCD_PW)

# 2: Merge data frames ####


# Combine them using bind_rows()
PrivateWell_0 <- bind_rows(DoD_OffBase_PW, TAP_PW, WQCD_PW)


column_names <- as.data.frame(colnames(PrivateWells))
colnames(PrivateWell_0)



# Reorder
PrivateWell_1 <- PrivateWell_0 |>  select (
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
  everything()
)




# Ensure all datasets have lat longs before running this

PrivateWells_NA <- PrivateWell_1 |>  filter(is.na(Latitude))

# Remove duplicates where there are multiple samples taken from the same locations. This is necessary for the sampling grid
PrivateWell_Final <- PrivateWell_1 |>
  group_by (Dataset, Program, Latitude, Longitude) |>  slice_max(order_by = `Sum of PFOA and PFOS`,
                                                                 n = 1,
                                                                 with_ties = FALSE) |>  ungroup()


# 3: Export private wells ####

library("writexl")
write_xlsx(PrivateWell_Final,
           "03_Clean_Data/PrivateWell/PrivateWell_2025.xlsx")



# 4: Merge with groundwater for a combined groundwater and private well dataset


# 4: Merge data private well and groundwater data frames ####


# Combine them using bind_rows()
PrivateWells_Groundwater <- bind_rows(PrivateWell_Final, Groundwater_Final)


# View columns
column_names <- as.data.frame(colnames(PrivateWells_Groundwater))
Columns <- as.data.frame(colnames(PrivateWells_Groundwater))


# Remove duplicates where there are multiple samples taken from the same locations. This is necessary for the sampling grid
PrivateWells_Groundwater_Final <- PrivateWells_Groundwater |>
  group_by (Dataset, Latitude, Longitude) |>  slice_max(order_by = `Sum of PFOA and PFOS`,
                                                        n = 1,
                                                        with_ties = FALSE) |>  ungroup()


# 5: Export groundwater and private well data frames####

library("writexl")
write_xlsx(
  PrivateWells_Groundwater_Final,
  "03_Clean_Data/PrivateWell/PrivateWell_Groundwater_2025.xlsx"
)
