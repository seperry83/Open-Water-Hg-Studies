
# Load packages
library(tidyverse)
library(readxl)

# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR Documents - Open Water Final Report - Documents/Technical Appendices/Inlet-Outlet/Data"
  )
)

# Import and modify data from DWR's Open Water Final Report SharePoint site
loads_calc <- read_csv(file = paste0(sharepoint_path, "/All_YB_Loads-R.csv"))

conc_data <- read_excel(path = paste0(sharepoint_path, "/YB_Inlet-Outlet_Conc_Data.xlsx"), sheet = "For R Analysis")
# may want to convert SampleDate and CollectionTimePST variables to date and time classes

daily_flow_data_se <- read_excel(path = paste0(sharepoint_path, "/DailyAvgFlows_All_and_SE.xlsx"), sheet = "Just Sampling Events")
# may want to add year at end of SamplingEvents

loads_flow_cf <- read_excel(path = paste0(sharepoint_path, "/YB_NetMeHgLoad_and_Flow_2006.xlsx"))
# may want to convert SampleDate variable to date class

part_conc_calc <- read_csv(file = paste0(sharepoint_path, "/Particulate_Conc.csv"))

field_data <- read_excel(path = paste0(sharepoint_path, "/FieldMeasurements.xlsx"))
# may want to convert SampleDate and CollectionTimePST variables to date and time classes
# may want to rename some variables

comb_param_calc <- read_csv(file = paste0(sharepoint_path, "/CombinedParameters.csv"))

qa_field_blanks <- read_excel(path = paste0(sharepoint_path, "/YB_Inlet-Outlet_Conc_Data.xlsx"), sheet = "Field and Filter Blanks")
# may want to convert SampleDate and CollectionTimePST variables to date and time classes
# may want to rename some variables

qa_field_dups <- read_excel(path = paste0(sharepoint_path, "/YB_Inlet-Outlet_Conc_Data.xlsx"), sheet = "Field Duplicates")
# may want to convert SampleDate and CollectionTimePST variables to date and time classes
# may want to rename some variables

# checked Concentrations, Field_Measurements, and Loads Folders
# Still need to go through the Flows folder

