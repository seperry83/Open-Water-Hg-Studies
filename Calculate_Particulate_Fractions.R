# Yolo Bypass Inlet-Outlet Study
# Calculate particulate fractions of THg, MeHg, and OC to be used throughout the analysis of this data

library(tidyverse)
library(readxl)
library(lubridate)

# Load common functions
source("inlet_outlet_common_functions.R")

# Import Concentration Data
conc <- read_excel("../../../Data/Lab_Final/YB_Inlet-Outlet_Conc_Data.xlsx", sheet = "For R Analysis") %>% 
  # Just keep MeHg, THg, TOC, and DOC; remove samples with QualCode "R"
  filter(
    str_detect(Analyte, "^MeHg|^THg|^DOC|^TOC"),
    is.na(QualCode) | !str_detect(QualCode, "^R")
  ) %>% 
  # Clean up date and time formatting- extract date and time from dttm variables
  mutate(
    SampleDate = as_date(SampleDate),
    CollectionTimePST = hms::as_hms(CollectionTimePST)
  ) %>%
  # Create a new variable Conc, which is a numeric version of Result with the MDL and RL for the ND values
  mod_result() %>% 
  # Keep only necessary variables
  select(
    StationName,
    SampleDate,
    CollectionTimePST,
    Analyte,
    Conc
  )

# Widen df and rename the Analytes to allow for subtraction
conc.part <- conc %>%
  pivot_wider(names_from = Analyte, values_from = Conc) %>% 
  rename(
    fMeHg = "MeHg- filtered",
    tMeHg = "MeHg- total",
    fTHg = "THg- filtered",
    tTHg = "THg- total"
  )

# Search for any cases when the total fraction is less than the filtered fraction
# All of these cases were dealt with in the YB_Inlet-Outlet_Conc_Data.xlsx file
# TOC and DOC
  conc.part %>% filter(TOC < DOC)  
# THg
  conc.part %>% filter(tTHg < fTHg)  
# MeHg
  conc.part %>% filter(tMeHg < fMeHg)
  
# Calculate the particulate concentrations by subtraction
conc.part <- conc.part %>% 
  mutate(
    POC = TOC - DOC,
    pMeHg = tMeHg - fMeHg,
    pTHg = tTHg - fTHg
  ) %>% 
  # Only keep particulate concentration variables
  select(-c(DOC:tMeHg)) %>% 
  # Rename Analytes
  rename(
    "MeHg- particulate" = pMeHg,
    "THg- particulate" = pTHg
  ) %>%
  # Pivot df back into long format
  pivot_longer(cols =  POC:"THg- particulate", names_to = "Analyte", values_to = "Conc") %>% 
  # Remove NA values in Conc variable
  filter(!is.na(Conc)) %>% 
  # Add a Units variable
  mutate(
    Units = if_else(
      str_detect(Analyte, "Hg-"),
      "ng/L",
      "mg/L as C"
    )
  ) %>% 
  # Round Conc to 3 significant figures
  mutate(Conc = signif(Conc, 3))

# Export conc.part df to a .csv file to be used in other scripts/analyses
conc.part %>% write_excel_csv("Concentrations/Particulate_Conc.csv")

