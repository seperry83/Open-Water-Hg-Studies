# Vegetation Sensecence Studies
# Purpose: Figures for Final Report
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(readxl)
library(openwaterhg)

# Datasets are on SharePoint site for the Open Water Final Report
# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR Documents - Open Water Final Report - Documents/Technical Appendices/Vegetation Senscence/Veg Sens. data and graphs"
  )
)  

# Figure 3 ----------------------------------------------------------------

# Import Data
vss_pilot2015_orig <- read_excel(path = paste0(sharepoint_path, "/VegSens_PilotStudies_Data.xlsx"), sheet = "Dec2015")
glimpse(vss_pilot2015_orig)
