# PERMITS - DEWATERING SOURCE WATER 


# 1: Importing the data ####

library(readxl)
library(dplyr)
library(tidyr)


# Importing Data
SourceWater_Raw <- read_excel("01_Raw_Data/Ongoing_Data_Collection/2024_Permits_SourceWater_May2024.xlsx")

colnames(SourceWater_Raw)



# 2: Misc Cleaning ####


# List of columns to pivot
columns_to_pivot <- c("COG318003", "COG318004", "COG318005", "COG318006", "COG318007", "COG318008", "COG318009", "COG318010", 
                      "COG318011", "COG318012", "COG318013", "COG318014", "COG318015", "COG318016", "COG318017", "COG318018", 
                      "COG318019", "COG318020", "COG318021", "COG318022", "COG318023", "COG318024", "COG318025", "COG318026", 
                      "COG318027", "COG318029", "COG318032", "COG318036", "COG318037", "COG318060", "COG318094", "COG318095", 
                      "COG318104", "COG318111", "COG318110", "COG318108", "COG318116", "COG318120", "COG318122", "COG318123", 
                      "COG318124", "COG318119", "COG318125", "COG318112", "COG318126", "COG318127", "COG318128", "COG603005", 
                      "COG603006", "COG603008", "COG603011", "COG603015", "COG603020", "COG603021", "COG603023", "COG603024", 
                      "COG603032", "COG603033", "COG603036", "COG603065", "COG603078", "COG603081", "COG603087", "COG603090", 
                      "COG603101", "COG603103", "COG603107", "COG603108", "COG603109", "COG603115", "COG603116", "COG603118", 
                      "COG603123", "COG603125", "COG603127", "COG603145", "COG603148", "COG603149", "COG603150", "COG603151", 
                      "COG603155", "COG603156", "COG603157", "COG603158", "COG603159", "COG603160", "COG603165", "COG603168", 
                      "COG603174", "COG603178", "COG603179", "COG603183", "COG603190", "COG603199", "COG603205", "COG603218", 
                      "COG603219", "COG603220", "COG603222", "COG603223", "COG603233", "COG603260", "COG603263", "COG603285", 
                      "COG603293", "COG603294", "COG603295", "COG603331", "COG603333", "COG603352", "COG603356", "COG603359", 
                      "COG603367", "COG603374", "COG603375", "COG603376", "COG603382", "COG603389", "COG603363", "COG603298", 
                      "COG603189", "COG603128", "COG317058", "COG317061", "COG317062", "COG317072", "COG317075", 
                      "COG317079", "COG317091", "COG317092", "COG317098", "COG317099", "COG317101", "COG080621", "COG317114...139", 
                      "COG317114...140", "COG080689", "COG080690", "COG317109...143", "COG317109...144", "COG317118", "COG317120", 
                      "COG317121", "COG080710", "COG317126...149","COG317126...150", "COG080830...151", "COG080830...152", "COG080830...153", 
                      "COG080830...154", "COG080830...155", "COG080830...156", "COG080830...157", "COG317134", "COG080854", "COG317143", 
                      "COG317144", "COG317149", "COG080968")

# Ensure all specified columns are character so that we can pivot both the character and numeric data
SourceWater_Raw[columns_to_pivot] <- lapply(SourceWater_Raw[columns_to_pivot], as.character)

# Perform the pivot to make the different permit numbers all under one column "Permit Number"
SourceLong <- pivot_longer(
  SourceWater_Raw,
  cols = all_of(columns_to_pivot),  # Using all_of to ensure the column names are correctly interpreted
  names_to = "Permit_Number",
  values_to = "Results"
)

# Filter for sample date and change sample_date variable to date format
sample_dates <- SourceLong %>%
  filter(Acronym == "Sample Date") %>%
  select(Permit_Number, Sample_Date = Results)

# Convert Excel date format to date format in R
sample_dates$Sample_Date <- as.Date(as.numeric(sample_dates$Sample_Date), origin = "1899-12-30")

# Filter for facility names
Facility_Name <- SourceLong %>%
  filter(Acronym == "Facility Name") %>%
  select(Permit_Number, Facility_Name = Results) 

# Filter for notes
Notes <- SourceLong %>%
  filter(Acronym == "Notes") %>%
  select(Permit_Number, Notes = Results) 


# Step 2: Remove rows where Acronym is "Sample Date", "Facility Name", or "Notes"
SourceFiltered <- SourceLong %>%
  filter(!(Acronym %in% c("Sample Date", "Facility Name", "Notes")))

#  Join the fields you removed above
SourceFinal <- SourceFiltered %>%
  left_join(sample_dates, by = "Permit_Number") %>%
  left_join(Facility_Name, by = "Permit_Number") %>%
  left_join(Notes, by = "Permit_Number")


SourceFinal <- SourceFinal %>% select(-Chemical_Name)


# Replace NDs with 0
SourceFinal <- SourceFinal %>%  
  mutate(Results= ifelse(is.na(Results), "0.0",
                         ifelse(grepl("<", Results), "0.0", Results)))

# Transform the results column to numeric
SourceFinal$Results <- as.numeric(SourceFinal$Results)
class(SourceFinal$Results)

# Round results column to 2 decimal place
# Round the 'Value' column to 1 decimal place
SourceFinal$Results  <- round(SourceFinal$Results , digits = 1)


# Fix permit Numbers with...
SourceLong1 <- SourceFinal %>%
  mutate(Permit_Number = case_when(
    Permit_Number %in% c("COG080830...151", "COG080830...152", "COG080830...153", "COG080830...154",
                         "COG080830...155", "COG080830...156", "COG080830...157") ~ "COG080830",
    Permit_Number %in% c("COG317109...143", "COG317109...144") ~ "COG317109",
    Permit_Number %in% c("COG317114...139", "COG317114...140") ~ "COG317114",
    Permit_Number  %in% c("COG317126...149", "COG317126...150") ~ "COG317126",
    Permit_Number  %in% c("COG603023...124", "COG603023...58") ~ "COG603023",
    TRUE ~ Permit_Number
  ))


# Aggregate the data before transforming it
aggregated_data <- SourceLong1 %>%
  group_by(Permit_Number, Sample_Date, Acronym, Quantification_Limit, Facility_Name, Notes) %>%
  summarise(Sum_Results = sum(Results, na.rm = TRUE), .groups = "drop")




Sourcewide1 <- aggregated_data %>%
  pivot_wider(
    id_cols = c("Permit_Number", "Sample_Date", "Facility_Name", "Notes"),
    names_from = Acronym,
    values_from = Sum_Results
  ) %>%
  rename_with(~ gsub("^Sum_Results_", "", .), starts_with("Sum_Results_"))



# Update permit numbers using the notes column

Sourcewide1 <- Sourcewide1 %>%
  mutate(Permit_Number = case_when(
    Permit_Number == "COG603128" ~ "COG318117",
    Permit_Number == "COG603189" ~ "COG318115",
    Permit_Number == "COG603023" ~ "COG318113",
    Permit_Number == "COG603298" ~ "COG318112",
    Permit_Number == "COG603363" ~ "COG318109",
    TRUE ~ Permit_Number  # Keep the original value if no condition is met
  ))

# 3: Merging with lat/longs ####

# Importing the file with lats and longs
SourceWater_LatLongs <- read_excel("01_Raw_Data/Ongoing_Data_Collection/2024_Permits_SourceWater_LatLongs.xlsx")


SourceWater_LatLongs_unique <- SourceWater_LatLongs %>%
  group_by(Permit_Number, Latitude, Longitude) %>%
  slice(1) %>%
  ungroup()

# Merge the lat longs dataset with the May 2024 dewatering source water data
Sourcewide_LatsLongs <- left_join(Sourcewide1,
                                  SourceWater_LatLongs_unique %>%
                                    select(Permit_Number, Latitude, Longitude),
                                  by = "Permit_Number")



# Identify NA lat longs
Sourcewide_LatsLongs_NA <- Sourcewide_LatsLongs %>% 
  filter(is.na(Latitude))



# 4: Rename and create new variables ####


# Rename variables to standardize across datasets
SourceWater1 <- Sourcewide_LatsLongs %>%
  rename(
    Site = Facility_Name,
    `Site ID` = Permit_Number,
    `Sample date` = Sample_Date,
    PFOSA =`PFOSA (or FOSA)`
  )


# Create new variables and remove variables

SourceWater2 <- SourceWater1 %>% mutate(
  Dataset = "Permits - Dewatering Source Water Data",
  Program = "CDPHE's Water Quality Control Division (WQCD) - Clean Water Program",
  Link = "https://drive.google.com/file/d/1mZ6yS0ms4knYAQcimgKGptnThPt6cScM/view",
  `Number of samples` = 1,
  Medium = "Groundwater",
  Units = "ng/L") |>
  mutate(`Sum of PFOA and PFOS` = PFOA + PFOS) |>
  mutate(`Sample ID` = paste0("Source_Water_", 1:nrow(SourceWater1))) 


SourceWater3 <- SourceWater2 %>%  select(-c(Notes,`Division Calculated Sum of seven`, `ICIS Sum of seven`))


# 5: Assign variable types ####

# Define the groups of columns based on your data dictionary
char_cols <- c("Dataset",
               "Program",
               "Medium",
               "Site",
               "Site ID",
               "Link",
               "Units")

numeric_cols <- c("Latitude", "Longitude", "Number of samples")

date_cols <- c("Sample date") # Note the date format is MM/DD/YYYY

# Apply the transformations using mutate() and across()
SourceWater4 <- SourceWater3 %>%
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


class(SourceWater4$`Sample date`)


# 6: Reorder variables ####

# Create a character vector containing the EXACT column names in the desired order.
desired_order <- c(
  "Dataset",
  "Program",
  "Medium",
  "Site",
  "Site ID",
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
  "PFBS"
)


# Reorder variables using the select function
Permits_SourceWater_Groundwater <- SourceWater4 |> select(all_of(desired_order), everything())


# 7: Export ####

library("writexl")
write_xlsx(Permits_SourceWater_Groundwater, "03_Clean_Data/Groundwater/Permits_Sourcewater_2025.xlsx")

