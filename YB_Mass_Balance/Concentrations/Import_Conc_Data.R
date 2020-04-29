# Yolo Bypass Inlet-Outlet Study
# Purpose: A script that imports the flood event concentration data and the calculated
# particulate data, binds this data together, and formats it to be used in further calculations.
# Author: Dave Bosworth

# Load packages
library(readr)
library(dplyr)
library(stringr)

# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR Documents - Open Water Final Report - Documents/Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Final"
  )
)

# Import Concentration Data
conc_orig <- 
  read_csv(
    paste0(sharepoint_path, "/NormalSamples.csv"),
    col_types = "????????????c"
  )

# Clean conc_orig
conc_clean <- conc_orig %>% 
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
part_conc_orig <- read_csv(paste0(sharepoint_path, "/Particulate_Conc.csv"))

# Bind all concentration data
all_conc <- bind_rows(conc_clean, part_conc_orig)

# Clean up
rm(conc_clean, conc_orig, part_conc_orig, sharepoint_path)
