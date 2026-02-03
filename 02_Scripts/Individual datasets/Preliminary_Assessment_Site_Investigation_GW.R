# PRELIMINARY ASSESSMENT AND SITE INVESTIGATION.

# Each year save the individual datasets to the "one time efforts" folder and clean separately then merge. 
# Merge should include all data from previous years as well as new data. Another option is to import previous
# year's clean dataset and merge old dataset with new sites.


library(readxl)
library(dplyr)

# 2023 Norge ####

Norge_PASI <- read_excel("01_Raw_Data/One_Time_Efforts/2023_Norge_PASI.xlsx")



# Replace all spaces in names of variables with underscores
names(Norge_PASI) <- gsub("\\s", "_", names((Norge_PASI)))


# Filter for unique only
Norge_PASI1 <- Norge_PASI %>%
  group_by(Sample_ID, Analyte) %>%
  distinct() %>%
  ungroup()


# Replace non-detects in the dataset with 0
Norge_PASI1 <-Norge_PASI %>%
  mutate(Result = ifelse(is.na(Result), NA,
                         ifelse(grepl("<", Result), 0, Result)))


# Converting results to numeric
Norge_PASI1$Result <- as.numeric(Norge_PASI1$Result)


# Converting the MDL to numeric
Norge_PASI1$MDL <- as.numeric(Norge_PASI1$MDL)

# Converting MRL to numeric
Norge_PASI1$MRL <- as.numeric(Norge_PASI1$MRL)



# 2023 TGap ####


TGap_PASI <- read_excel("01_Raw_Data/One_Time_Efforts/2023_TGap_PASI.xlsx")


# Replace all spaces in names of variables with underscores
names(TGap_PASI) <- gsub("\\s", "_", names((TGap_PASI)))


# Filter for unique only
TGap_PASI <- TGap_PASI %>%
  group_by(Sample_ID, Analyte) %>%
  distinct() %>%
  ungroup()

# Replace non-detects in the dataset with 0
TGap_PASI1 <-TGap_PASI %>%
  mutate(Result = ifelse(is.na(Result), NA,
                         ifelse(grepl("10", Result), 0, Result)))

# Converting results to numeric
TGap_PASI1$Result <- as.numeric(TGap_PASI1$Result)

# Adding short PFAS abbreviations to the dataset
TGap_PASI2 <- TGap_PASI1 |>  
  mutate(PFAS_Analyte = case_when(
    CAS_No %in% c("2058-94-8", "2058948") ~ "PFUnA",
    CAS_No %in% c("72629-94-8", "72629948") ~ "PFTrDA",
    CAS_No %in% c("376-06-7", "376067") ~ "PFTeA",
    CAS_No %in% c("423-41-6", "423416") ~ "PFPrS",
    CAS_No %in% c("2706-91-4", "2706914") ~ "PFPeS",
    CAS_No %in% c("2706-90-3", "2706903") ~ "PFPeA",
    CAS_No %in% c("754-91-6", "754916") ~ "PFOSA",
    CAS_No %in% c("1763-23-1", "1763231") ~ "PFOS",
    CAS_No %in% c("335-67-1", "335671") ~ "PFOA",
    CAS_No %in% c("68259-12-1", "68259121") ~ "PFNS",
    CAS_No %in% c("375-95-1", "375951") ~ "PFNA",
    CAS_No %in% c("377-73-1", "377731") ~ "PFMPA",
    CAS_No %in% c("863090-89-5", "863090895") ~ "PFMBA",
    CAS_No %in% c("355-46-4", "355464") ~ "PFHxS",
    CAS_No %in% c("307-24-4", "307244") ~ "PFHxA",
    CAS_No %in% c("375-92-8", "375928") ~ "PFHpS",
    CAS_No %in% c("375-85-9", "375859") ~ "PFHpA",
    CAS_No %in% c("646-83-3", "646833") ~ "PFEtCHxS",
    CAS_No %in% c("113507-82-7", "113507827") ~ "PFEESA",
    CAS_No %in% c("335-77-3", "335773") ~ "PFDS",
    CAS_No %in% c("79780-39-5", "79780395") ~ "PFDoS",
    CAS_No %in% c("307-55-1", "307551") ~ "PFDoA",
    CAS_No %in% c("335-76-2", "335762") ~ "PFDA",
    CAS_No %in% c("375-73-5", "375735") ~ "PFBS",
    CAS_No %in% c("375-22-4", "375224") ~ "PFBA",
    CAS_No %in% c("24448-09-7", "24448097") ~ "NMeFOSE",
    CAS_No %in% c("2355-31-9", "2355319") ~ "NMeFOSAA",
    CAS_No %in% c("31506-32-8", "31506328") ~ "NMeFOSA",
    CAS_No %in% c("151772-58-6", "151772586") ~ "NFDHA",
    CAS_No %in% c("1691-99-2", "1691992") ~ "NEtFOSE",
    CAS_No %in% c("2991-50-6", "2991506") ~ "NEtFOSAA",
    CAS_No %in% c("4151-50-2", "4151502") ~ "NEtFOSA",
    CAS_No %in% c("13252-13-6", "13252136") ~ "HFPO-DA",
    CAS_No %in% c("919005-14-4", "919005144") ~ "ADONA",
    CAS_No %in% c("756426-58-1", "756426581") ~ "9Cl-PF3ONS",
    CAS_No %in% c("70887-84-2", "70887842") ~ "8:2 FTUCA",
    CAS_No %in% c("39108-34-4", "39108344") ~ "8:2 FTS",
    CAS_No %in% c("27854-31-5", "27854315") ~ "8:2 FTCA",
    CAS_No %in% c("812-70-4", "812704") ~ "7:3 FTCA",
    CAS_No %in% c("70887-88-6", "70887886") ~ "6:2 FTUCA",
    CAS_No %in% c("27619-97-2", "27619972") ~ "6:2 FTS",
    CAS_No %in% c("53826-12-3", "53826123") ~ "6:2 FTCA",
    CAS_No %in% c("914637-49-3", "914637493") ~ "5:3 FTCA",
    CAS_No %in% c("757124-72-4", "757124724") ~ "4:2 FTS",
    CAS_No %in% c("0356-02-05", "3560205") ~ "3:3 FTCA",
    CAS_No %in% c("763051-92-9", "763051929") ~ "11Cl-PF3OUdS",
    CAS_No %in% c("70887-94-4", "70887944") ~ "10:2 FTUCA",
    CAS_No %in% c("120226-60-0", "120226600") ~ "10:2 FTS",
    CAS_No %in% c("53826-13-4", "53826134") ~ "10:2 FTCA",
    CAS_No %in% c("67905-19-5", "67905195") ~ "PFHxDA",
    CAS_No %in% c("2806-24-8", "2806248") ~ "FOSAA",
    Analyte == "Perfluoro-1-butanesulfonate" ~"PFBS",
    Analyte == "Perfluoro-1-dodecanesulfonate" ~ "PFDoS",
    Analyte == "Perfluoro-1-dodecanesulfonate" ~ "PFDoS",
    Analyte == "Perfluoro-1-heptanesulfonate" ~ "PFHpS",
    Analyte == "Perfluoro-1-hexanesulfonate" ~ "PFHxS",
    Analyte == "Perfluoro-1-nonanesulfonate" ~ "PFNS",
    Analyte == "Perfluoro-1-octanesulfonamidoacetic acid" ~ "FOSAA",
    Analyte == "Perfluoro-1-octanesulfonate" ~ "PFOS",
    Analyte == "Perfluoro-1-pentanesulfonate" ~ "PFPeS",
    Analyte == "Perfluoro-n-octadecanoic acid" ~ "PFODA",
    Analyte == "Perfluoro-n-tetradecanoic acid" ~ "PFTeA",
    Analyte == "Perfluoro-1-decanesulfonate" ~ "PFDS",
TRUE ~ NA_character_ # Catch-all for anything not in the list
  ))


# 2022 Swedes Custom Chrome ####

Swedes_PASI <- read_excel("01_Raw_Data/One_Time_Efforts/2022_SwedesCustomChrome_PASI.xlsx")


# Replace all spaces in names of variables with underscores
names(Swedes_PASI) <- gsub("\\s", "_", names((Swedes_PASI)))


# Filter for unique only
Swedes_PASI_1 <- Swedes_PASI %>%
  group_by(Sample_ID, Analyte) %>%
  distinct() %>%
  ungroup()

# Replace non-detects in the dataset with 0
Swedes_PASI_2 <-Swedes_PASI_1 %>%
  mutate(Result = ifelse(is.na(Result), NA,
                         ifelse(grepl("<", Result), 0, Result)))

# Converting results to numeric
Swedes_PASI_2$Result <- as.numeric(Swedes_PASI_2$Result)


# Adding short PFAS abbreviations to the dataset
Swedes_PASI_3 <- Swedes_PASI_2 |>  
  mutate(PFAS_Analyte = case_when(
    CAS_Number %in% c("2058-94-8", "2058948") ~ "PFUnA",
    CAS_Number %in% c("72629-94-8", "72629948") ~ "PFTrDA",
    CAS_Number %in% c("376-06-7", "376067") ~ "PFTeA",
    CAS_Number %in% c("423-41-6", "423416") ~ "PFPrS",
    CAS_Number %in% c("2706-91-4", "2706914") ~ "PFPeS",
    CAS_Number %in% c("2706-90-3", "2706903") ~ "PFPeA",
    CAS_Number %in% c("754-91-6", "754916") ~ "PFOSA",
    CAS_Number %in% c("1763-23-1", "1763231") ~ "PFOS",
    CAS_Number %in% c("335-67-1", "335671") ~ "PFOA",
    CAS_Number %in% c("68259-12-1", "68259121") ~ "PFNS",
    CAS_Number %in% c("375-95-1", "375951") ~ "PFNA",
    CAS_Number %in% c("377-73-1", "377731") ~ "PFMPA",
    CAS_Number %in% c("863090-89-5", "863090895") ~ "PFMBA",
    CAS_Number %in% c("355-46-4", "355464") ~ "PFHxS",
    CAS_Number %in% c("307-24-4", "307244") ~ "PFHxA",
    CAS_Number %in% c("375-92-8", "375928") ~ "PFHpS",
    CAS_Number %in% c("375-85-9", "375859") ~ "PFHpA",
    CAS_Number %in% c("646-83-3", "646833") ~ "PFEtCHxS",
    CAS_Number %in% c("113507-82-7", "113507827") ~ "PFEESA",
    CAS_Number %in% c("335-77-3", "335773") ~ "PFDS",
    CAS_Number %in% c("79780-39-5", "79780395") ~ "PFDoS",
    CAS_Number %in% c("307-55-1", "307551") ~ "PFDoA",
    CAS_Number %in% c("335-76-2", "335762") ~ "PFDA",
    CAS_Number %in% c("375-73-5", "375735") ~ "PFBS",
    CAS_Number %in% c("375-22-4", "375224") ~ "PFBA",
    CAS_Number %in% c("24448-09-7", "24448097") ~ "NMeFOSE",
    CAS_Number %in% c("2355-31-9", "2355319") ~ "NMeFOSAA",
    CAS_Number %in% c("31506-32-8", "31506328") ~ "NMeFOSA",
    CAS_Number %in% c("151772-58-6", "151772586") ~ "NFDHA",
    CAS_Number %in% c("1691-99-2", "1691992") ~ "NEtFOSE",
    CAS_Number %in% c("2991-50-6", "2991506") ~ "NEtFOSAA",
    CAS_Number %in% c("4151-50-2", "4151502") ~ "NEtFOSA",
    CAS_Number %in% c("13252-13-6", "13252136") ~ "HFPO-DA",
    CAS_Number %in% c("919005-14-4", "919005144") ~ "ADONA",
    CAS_Number %in% c("756426-58-1", "756426581") ~ "9Cl-PF3ONS",
    CAS_Number %in% c("70887-84-2", "70887842") ~ "8:2 FTUCA",
    CAS_Number %in% c("39108-34-4", "39108344") ~ "8:2 FTS",
    CAS_Number %in% c("27854-31-5", "27854315") ~ "8:2 FTCA",
    CAS_Number %in% c("812-70-4", "812704") ~ "7:3 FTCA",
    CAS_Number %in% c("70887-88-6", "70887886") ~ "6:2 FTUCA",
    CAS_Number %in% c("27619-97-2", "27619972") ~ "6:2 FTS",
    CAS_Number %in% c("53826-12-3", "53826123") ~ "6:2 FTCA",
    CAS_Number %in% c("914637-49-3", "914637493") ~ "5:3 FTCA",
    CAS_Number %in% c("757124-72-4", "757124724") ~ "4:2 FTS",
    CAS_Number %in% c("0356-02-05", "3560205") ~ "3:3 FTCA",
    CAS_Number %in% c("763051-92-9", "763051929") ~ "11Cl-PF3OUdS",
    CAS_Number %in% c("70887-94-4", "70887944") ~ "10:2 FTUCA",
    CAS_Number %in% c("120226-60-0", "120226600") ~ "10:2 FTS",
    CAS_Number %in% c("53826-13-4", "53826134") ~ "10:2 FTCA",
    CAS_Number %in% c("67905-19-5", "67905195") ~ "PFHxDA",
    CAS_Number %in% c("2806-24-8", "2806248") ~ "FOSAA",
    Analyte == "Perfluoro-1-butanesulfonate" ~"PFBS",
    Analyte == "Perfluoro-1-dodecanesulfonate" ~ "PFDoS",
    Analyte == "Perfluoro-1-dodecanesulfonate" ~ "PFDoS",
    Analyte == "Perfluoro-1-heptanesulfonate" ~ "PFHpS",
    Analyte == "Perfluoro-1-hexanesulfonate" ~ "PFHxS",
    Analyte == "Perfluoro-1-nonanesulfonate" ~ "PFNS",
    Analyte == "Perfluoro-1-octanesulfonamidoacetic acid" ~ "FOSAA",
    Analyte == "Perfluoro-1-octanesulfonate" ~ "PFOS",
    Analyte == "Perfluoro-1-pentanesulfonate" ~ "PFPeS",
    Analyte == "Perfluoro-n-octadecanoic acid" ~ "PFODA",
    Analyte == "Perfluoro-n-tetradecanoic acid" ~ "PFTeA",
    Analyte == "Perfluoro-1-decanesulfonate" ~ "PFDS",
    Analyte == "Perfluoro-1-butanesulfonate (L-PFBS)" ~ "PFBS",
    Analyte == "Perfluoro-1-decanesulfonate (L-PFDS)" ~ "PFDS",
    Analyte == "Perfluoro-1-dodecanesulfonate (L-PFDoS)" ~ "PFDoS",
    Analyte == "Perfluoro-1-heptanesulfonate (L-PFHpS)" ~ "PFHpS",
    Analyte == "Perfluoro-1-nonanesulfonate (L-PFNS)" ~ "PFNS",
    Analyte == "Perfluoro-1-octanesulfonate (L-PFOS)" ~ "PFOS",
    Analyte == "Perfluoro-1-pentanesulfonate (L-PFPeS)" ~ "PFPeS",
    Analyte == "Perfluoro-1-pentanesulfonate (PFPeS)" ~ "PFPeS",
    Analyte == "Perfluoro-n-octadecanoic acid (PFODA)" ~ "PFODA",
    Analyte == "Perfluoro-n-tetradecanoic acid (PFTeDA)" ~ "PFTeDA",
    Analyte == "Perfluoro-n-tetradecanoic acid (PFTeDA)" ~ "PFTeDA",
    Analyte == "Perfluoro-1-hexanesulfonate (L-PFHxS)" ~ "PFHxS",
    TRUE ~ NA_character_ # Catch-all for anything not in the list
  ))




# 2025 Intermountain Tanning ####
InterMtn_PASI <- read_excel("01_Raw_Data/One_Time_Efforts/2025_IntermountainTanning_PASI.xlsx")


# Replace all spaces in names of variables with underscores
names(InterMtn_PASI) <- gsub("\\s", "_", names((InterMtn_PASI)))


# Filter for unique only
InterMtn_PASI_1 <- InterMtn_PASI %>%
  group_by(Sample_ID, Analyte) %>%
  distinct() %>%
  ungroup()

# Replace non-detects in the dataset with 0
InterMtn_PASI_2 <-InterMtn_PASI_1 %>%
  mutate(Result = ifelse(is.na(Result), NA,
                         ifelse(grepl(10, Result), 0, Result)))

# Converting results to numeric
InterMtn_PASI_2$Result <- as.numeric(InterMtn_PASI_2$Result)



# Adding short PFAS abbreviations to the dataset
InterMtn_PASI_3 <- InterMtn_PASI_2 |>  
  mutate(PFAS_Analyte = case_when(
    CAS_No %in% c("2058-94-8", "2058948") ~ "PFUnA",
    CAS_No %in% c("72629-94-8", "72629948") ~ "PFTrDA",
    CAS_No %in% c("376-06-7", "376067") ~ "PFTeA",
    CAS_No %in% c("423-41-6", "423416") ~ "PFPrS",
    CAS_No %in% c("2706-91-4", "2706914") ~ "PFPeS",
    CAS_No %in% c("2706-90-3", "2706903") ~ "PFPeA",
    CAS_No %in% c("754-91-6", "754916") ~ "PFOSA",
    CAS_No %in% c("1763-23-1", "1763231") ~ "PFOS",
    CAS_No %in% c("335-67-1", "335671") ~ "PFOA",
    CAS_No %in% c("68259-12-1", "68259121") ~ "PFNS",
    CAS_No %in% c("375-95-1", "375951") ~ "PFNA",
    CAS_No %in% c("377-73-1", "377731") ~ "PFMPA",
    CAS_No %in% c("863090-89-5", "863090895") ~ "PFMBA",
    CAS_No %in% c("355-46-4", "355464") ~ "PFHxS",
    CAS_No %in% c("307-24-4", "307244") ~ "PFHxA",
    CAS_No %in% c("375-92-8", "375928") ~ "PFHpS",
    CAS_No %in% c("375-85-9", "375859") ~ "PFHpA",
    CAS_No %in% c("646-83-3", "646833") ~ "PFEtCHxS",
    CAS_No %in% c("113507-82-7", "113507827") ~ "PFEESA",
    CAS_No %in% c("335-77-3", "335773") ~ "PFDS",
    CAS_No %in% c("79780-39-5", "79780395") ~ "PFDoS",
    CAS_No %in% c("307-55-1", "307551") ~ "PFDoA",
    CAS_No %in% c("335-76-2", "335762") ~ "PFDA",
    CAS_No %in% c("375-73-5", "375735") ~ "PFBS",
    CAS_No %in% c("375-22-4", "375224") ~ "PFBA",
    CAS_No %in% c("24448-09-7", "24448097") ~ "NMeFOSE",
    CAS_No %in% c("2355-31-9", "2355319") ~ "NMeFOSAA",
    CAS_No %in% c("31506-32-8", "31506328") ~ "NMeFOSA",
    CAS_No %in% c("151772-58-6", "151772586") ~ "NFDHA",
    CAS_No %in% c("1691-99-2", "1691992") ~ "NEtFOSE",
    CAS_No %in% c("2991-50-6", "2991506") ~ "NEtFOSAA",
    CAS_No %in% c("4151-50-2", "4151502") ~ "NEtFOSA",
    CAS_No %in% c("13252-13-6", "13252136") ~ "HFPO-DA",
    CAS_No %in% c("919005-14-4", "919005144") ~ "ADONA",
    CAS_No %in% c("756426-58-1", "756426581") ~ "9Cl-PF3ONS",
    CAS_No %in% c("70887-84-2", "70887842") ~ "8:2 FTUCA",
    CAS_No %in% c("39108-34-4", "39108344") ~ "8:2 FTS",
    CAS_No %in% c("27854-31-5", "27854315") ~ "8:2 FTCA",
    CAS_No %in% c("812-70-4", "812704") ~ "7:3 FTCA",
    CAS_No %in% c("70887-88-6", "70887886") ~ "6:2 FTUCA",
    CAS_No %in% c("27619-97-2", "27619972") ~ "6:2 FTS",
    CAS_No %in% c("53826-12-3", "53826123") ~ "6:2 FTCA",
    CAS_No %in% c("914637-49-3", "914637493") ~ "5:3 FTCA",
    CAS_No %in% c("757124-72-4", "757124724") ~ "4:2 FTS",
    CAS_No %in% c("0356-02-05", "3560205") ~ "3:3 FTCA",
    CAS_No %in% c("763051-92-9", "763051929") ~ "11Cl-PF3OUdS",
    CAS_No %in% c("70887-94-4", "70887944") ~ "10:2 FTUCA",
    CAS_No %in% c("120226-60-0", "120226600") ~ "10:2 FTS",
    CAS_No %in% c("53826-13-4", "53826134") ~ "10:2 FTCA",
    CAS_No %in% c("67905-19-5", "67905195") ~ "PFHxDA",
    CAS_No %in% c("2806-24-8", "2806248") ~ "FOSAA",
    Analyte == "Perfluoro-1-butanesulfonate" ~"PFBS",
    Analyte == "Perfluoro-1-dodecanesulfonate" ~ "PFDoS",
    Analyte == "Perfluoro-1-dodecanesulfonate" ~ "PFDoS",
    Analyte == "Perfluoro-1-heptanesulfonate" ~ "PFHpS",
    Analyte == "Perfluoro-1-hexanesulfonate" ~ "PFHxS",
    Analyte == "Perfluoro-1-nonanesulfonate" ~ "PFNS",
    Analyte == "Perfluoro-1-octanesulfonamidoacetic acid" ~ "FOSAA",
    Analyte == "Perfluoro-1-octanesulfonate" ~ "PFOS",
    Analyte == "Perfluoro-1-pentanesulfonate" ~ "PFPeS",
    Analyte == "Perfluoro-n-octadecanoic acid" ~ "PFODA",
    Analyte == "Perfluoro-n-tetradecanoic acid" ~ "PFTeA",
    Analyte == "Perfluoro-1-decanesulfonate" ~ "PFDS",
    Analyte == "Perfluoro-1-butanesulfonate (L-PFBS)" ~ "PFBS",
    Analyte == "Perfluoro-1-decanesulfonate (L-PFDS)" ~ "PFDS",
    Analyte == "Perfluoro-1-dodecanesulfonate (L-PFDoS)" ~ "PFDoS",
    Analyte == "Perfluoro-1-heptanesulfonate (L-PFHpS)" ~ "PFHpS",
    Analyte == "Perfluoro-1-nonanesulfonate (L-PFNS)" ~ "PFNS",
    Analyte == "Perfluoro-1-octanesulfonate (L-PFOS)" ~ "PFOS",
    Analyte == "Perfluoro-1-pentanesulfonate (L-PFPeS)" ~ "PFPeS",
    Analyte == "Perfluoro-1-pentanesulfonate (PFPeS)" ~ "PFPeS",
    Analyte == "Perfluoro-n-octadecanoic acid (PFODA)" ~ "PFODA",
    Analyte == "Perfluoro-n-tetradecanoic acid (PFTeDA)" ~ "PFTeDA",
    Analyte == "Perfluoro-n-tetradecanoic acid (PFTeDA)" ~ "PFTeDA",
    Analyte == "Perfluoro-1-hexanesulfonate (L-PFHxS)" ~ "PFHxS",
    TRUE ~ NA_character_ # Catch-all for anything not in the list
  ))


# 5: Binding data from multiple sites#### 


colnames(InterMtn_PASI_3)
colnames(Swedes_PASI_3)
colnames(TGap_PASI2)
colnames(Norge_PASI1)

# Renaming variables for the merge

InterMtn_PASI_4 <- InterMtn_PASI_3 |>  rename(Qual = Val_Qual) |>  select(- c(CAS_No, MDL,Analyte, Qual))
TGap_PASI3 <- TGap_PASI2 |>  rename(Qual = Val_Qual) |>  select(-c(CAS_No, Qual, MDL,Analyte))
Swedes_PASI_4 <- Swedes_PASI_3 |> rename(CAS_No= CAS_Number) |>  select(-c(CAS_No, Analyte, Qual, MRL))
Norge_PASI2 <- Norge_PASI1 |>  select(-c(CAS_No, MDL, MRL,Analyte))

# Binding the two dry cleaning datasets together
PASI_Merge <- bind_rows(InterMtn_PASI_4, Swedes_PASI_4, Norge_PASI2, TGap_PASI3)

colnames(PASI_Merge)

# 6: Transform wide #### 


# Make sure result is numeric before transforming
PASI_Merge$Result <- as.numeric(PASI_Merge$Result)


# Round results column to 1 decimal place
PASI_Merge$Result <- round(PASI_Merge$Result, digits = 1)



library(tidyr)

# Tranforming from long to wide format.
PASI_Wide <- PASI_Merge%>%
  pivot_wider(
    id_cols = c("Sample_ID", "Units",
                "Data_Source", "Latitude", "Longitude", "Sample_Date"),
    names_from = PFAS_Analyte,
    values_from = Result,
  )



# 7: Rename and create new variables #### 


# Rename variables
PASI_Wide_1 <- PASI_Wide %>%
  rename(Site = Data_Source,
         `Sample date` = Sample_Date,
         `Sample ID`= Sample_ID
  ) %>%
  mutate(Dataset = "Preliminary Assessment and Site Investigation Data",
         Program = "CDPHE's Hazardous Materials and Waste Management Division",
         Medium = "Groundwater",
         `Number of samples` = 1) |> 
  mutate(Link = "https://cdphe.colorado.gov/hm/superfund-sites-contacts") |> 
  mutate(`Sum of PFOA and PFOS` = PFOA + PFOS) 



# 8: Assign variable types ####

glimpse(PASI_Wide_1)

# Define the groups of columns based on your data dictionary
char_cols <- c("Dataset",
               "Program",
               "Medium",
               "Site",
               "Link",
               "Units")

numeric_cols <- c("Latitude", "Longitude", "Number of samples")

date_cols <- c("Sample date") # Note the date format is MM/DD/YYYY

# Apply the transformations using mutate() and across()
PASI_Wide_2<- PASI_Wide_1%>%
  # 1. Convert Character Columns
  mutate(across(.cols = all_of(char_cols), .fns = as.character)) %>%
  
  # 2. Convert Numeric Columns
  mutate(across(.cols = all_of(numeric_cols), .fns = as.numeric)) %>%
  
  # 3. Convert Date Columns
  mutate(across(
    .cols = all_of(date_cols),
    # Specify the format: %m = Month, %d = Day, %Y = 4-digit Year
    .fns = ~ as.Date(., format = "%m/%d/%Y")
  ))


class(PASI_Wide_2$`Sample date`)



# 9: Reorder variables ####

# Create vector wth order of columns
desired_order <- c(
  "Dataset",
  "Program",
  "Medium",
  "Site",
  "Latitude",
  "Longitude",
  "Link",
  "Sample date",
  "Sample ID",
  "Number of samples",
  "Units",
  "Sum of PFOA and PFOS",
  "PFOA",
  "PFOS",
  "PFHxS",
  "PFNA",
  "PFBS",
  "HFPO-DA"
)



# Reorder variables using the select function
PASI_Wide_3 <- PASI_Wide_2 |> select(all_of(desired_order), everything())


# Rename the dataset

PASI_Groundwater_2024 <- PASI_Wide_3

# 10: Export ####

# Export the data frame as a cleaned and formatted dataset.

library("writexl")
write_xlsx(PASI_Groundwater_2024, "03_Clean_Data/Groundwater/PASI_Groundwater_2024.xlsx")


