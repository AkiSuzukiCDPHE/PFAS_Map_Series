library(readxl)
library(dplyr)

# # Importing Data for just private well grid
PW_Grid <- read_excel("X:/Shared drives/_CDPHE TEEO Data/_Enviro/PFAS/PFAS Concentration Map May 2024 Update/PFASMap 2024_RProject/01_Raw_Data/PrivateWell_FishnetID.xlsx")
colnames(PW_Grid)

# 
# # Importing Data for private well and groundwater grid
# PW_Grid <- read_excel("X:\\Shared drives\\_CDPHE TEEO Data\\_Enviro\\PFAS\\PFAS Concentration Map May 2024 Update\\PFASMap 2024_RProject\\03_Clean_Data\\Grid Summary Stats\\PWGW_FIshnet_ID_Aug2.xlsx")
# colnames(PW_Grid)


# Rename sum pfoa and pfos temporarily
PW_Grid <- PW_Grid |> rename (Sum_PFOS_PFOA =`Sum of PFOA and PFOS` )

class(PW_Grid$Sum_PFOS_PFOA)
class(PW_Grid$PFOA)
class(PW_Grid$PFOS)

# Change PFOA, PFOS and Sum
PW_Grid$PFOA <-as.numeric(PW_Grid$PFOA)
PW_Grid$PFOS <-as.numeric(PW_Grid$PFOS)
PW_Grid$Sum_PFOS_PFOA <-as.numeric(PW_Grid$Sum_PFOS_PFOA)

PW_Grid1 <- PW_Grid %>%
  group_by(Fishnet_ID) %>%
  mutate(
    min_PFOS = if_else(all(is.na(PFOS)), NA_real_, min(PFOS, na.rm = TRUE)),
    min_PFOA = if_else(all(is.na(PFOA)), NA_real_, min(PFOA, na.rm = TRUE)),
    min_SUM_PFOA_PFOS = if_else(all(is.na(Sum_PFOS_PFOA)), NA_real_, min(Sum_PFOS_PFOA, na.rm = TRUE))
  ) %>%
  ungroup()



# Create a variable that assigns each sample row the max PFOS concentrations for samples in its corresponding fishnet grid number
PW_Grid2 <- PW_Grid1 %>%
  group_by(Fishnet_ID) %>%
  mutate(max_PFOS = max(PFOS, na.rm = TRUE),
         max_PFOS = replace(max_PFOS, is.infinite(max_PFOS), NA),
         max_PFOA = max(PFOA, na.rm = TRUE),
         max_PFOA = replace(max_PFOA, is.infinite(max_PFOA), NA),
         max_SUM_PFOA_PFOS = max(Sum_PFOS_PFOA, na.rm = TRUE),
         max_SUM_PFOA_PFOS = replace(max_SUM_PFOA_PFOS, is.infinite(max_SUM_PFOA_PFOS), NA)) %>%
  ungroup()

colnames(PW_Grid1)


# Create a variable that assigns each sample row the average PFOS concentrations for samples in its corresponding fishnet grid number
PW_Grid3 <- PW_Grid2 %>%
  group_by(Fishnet_ID) %>%
  mutate(avg_PFOS = mean(PFOS, na.rm = TRUE),
         avg_PFOS = replace(avg_PFOS, is.infinite(avg_PFOS), NA),
         avg_PFOA = mean(PFOA, na.rm = TRUE),
         avg_PFOA = replace(avg_PFOA, is.infinite(avg_PFOA), NA),
         avg_Sum_PFOS_PFOA = mean(Sum_PFOS_PFOA, na.rm = TRUE),
         avg_Sum_PFOS_PFOA = replace(avg_Sum_PFOS_PFOA, is.infinite(avg_Sum_PFOS_PFOA), NA)) %>%
  ungroup()


# Create a variable that assigns each sample row the median PFOS concentrations for samples in its corresponding fishnet grid number
PW_Grid4 <- PW_Grid3 %>%
  group_by(Fishnet_ID) %>%
  mutate(
    median_PFOS = if_else(all(is.na(PFOS)), NA_real_, median(PFOS, na.rm = TRUE)),
    median_PFOA = if_else(all(is.na(PFOA)), NA_real_, median(PFOA, na.rm = TRUE)),
    median_SUM_PFOA_PFOS = if_else(all(is.na(Sum_PFOS_PFOA)), NA_real_, median(Sum_PFOS_PFOA, na.rm = TRUE))
  ) %>%
  ungroup()


# Delete individual pfas analytes and other unecessary variables
# Only need Number of samples, units and the summary stats
PW_Grid5 <- PW_Grid4 %>% select(-c(Dataset: `Sample ID`, Sum_PFOS_PFOA: PFPROPrA))

# remove duplicate rows (only need one row with summary stats per fishnet ID)
PW_Grid6 <- PW_Grid5 %>%
  distinct(Fishnet_ID, .keep_all = TRUE)

# Rename variables

PW_Grid7 <- PW_Grid6 %>%
   rename("Min PFOS" = "min_PFOS",
          "Max PFOS" = "max_PFOS",
          "Average PFOS" = "avg_PFOS",
          "Median PFOS" = "median_PFOS",
          "Min PFOA" = "min_PFOA",
          "Max PFOA" = "max_PFOA",
          "Average PFOA" = "avg_PFOA",
          "Median PFOA" = "median_PFOA",
          "Min sum of PFOA and PFOS" = "min_SUM_PFOA_PFOS",
          "Max sum of PFOA and PFOS" = "max_SUM_PFOA_PFOS",
          "Average sum of PFOA and PFOS" = "avg_Sum_PFOS_PFOA",
          "Median sum of PFOA and PFOS" = "median_SUM_PFOA_PFOS",)

# Export the data frame as a cleaned and formatted dataset.

library("writexl")
write_xlsx(PW_Grid4,"X:/Shared drives/_CDPHE TEEO Data/_Enviro/PFAS/PFAS Concentration Map May 2024 Update/PFASMap 2024_RProject/03_Clean_Data/Grid Summary Stats/PW_Grid_SumStats_PWGW_Aug2.xlsx")



