# Yolo Bypass Inlet-Outlet Study
# Purpose: A script that imports the flood event concentration data and the calculated
# particulate data, binds this data together, and formats it to be used in further calculations.
# Author: Dave Bosworth

# Load packages
library(readr)
library(dplyr)
library(stringr)

# Define path on SharePoint site for data
temp_sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR Documents - Open Water Final Report - Documents/Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Final"
  )
)

# Import Concentration Data
temp_conc_orig <- 
  read_csv(
    paste0(temp_sharepoint_path, "/NormalSamples.csv"),
    col_types = "????????????c"
  )

# Clean temp_conc_orig
temp_conc_clean <- temp_conc_orig %>% 
  # Remove samples with QualCode "R"
  filter(is.na(QualCode) | !str_detect(QualCode, "^R")) %>%
  # Create a new variable Conc, which is a numeric version of Result with the MDL and RL for the ND values
  mutate(
    Conc = case_when(
      Result == "< RL"  ~ RL,
      Result == "< MDL" ~ MDL,
      TRUE              ~ as.numeric(Result)
    )
  ) %>% 
  # Clean up df
  select(SampleCode:Analyte, Conc, ResQual:QualCode)

# Import calculated particulate concentration data
temp_part_conc_orig <- read_csv(paste0(temp_sharepoint_path, "/Particulate_Conc.csv"))

# Bind all concentration data
all_conc <- bind_rows(temp_conc_clean, temp_part_conc_orig)

# Clean up
rm(temp_conc_clean, temp_conc_orig, temp_part_conc_orig, temp_sharepoint_path)
