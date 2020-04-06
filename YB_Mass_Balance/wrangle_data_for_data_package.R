# Wrangle all commonly used data for the YB Mass Balance study to be exported to the openwaterhg package

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(usethis)

# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR Documents - Open Water Final Report - Documents/Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data"
  )
)

# Import and modify data from DWR's Open Water Final Report SharePoint site
loads_calc <- 
  read_csv(
    file = paste0(
      sharepoint_path,
      "/All_YB_Loads-R.csv"
    )
  )

conc_data <- 
  read_excel(
    path = paste0(
      sharepoint_path,
      "/YB_Inlet-Outlet_Conc_Data.xlsx"
    ), 
    sheet = "For R Analysis"
  ) %>% 
  # Clean up date and time formatting- extract date and time from dttm variables
  mutate(
    SampleDate = as_date(SampleDate),
    CollectionTimePST = hms::as_hms(CollectionTimePST)
  )

daily_flow_data_se <- 
  read_excel(
    path = paste0(
      sharepoint_path,
      "/DailyAvgFlows_All_and_SE.xlsx"
    ),
    sheet = "Just Sampling Events"
  )

daily_flow_data_all <- 
  read_excel(
    path = paste0(
      sharepoint_path,
      "/DailyAvgFlows_All_and_SE.xlsx"
    ),
    sheet = "All data"
  ) %>% 
  # Clean up date formatting- extract date from dttm variable
  mutate(Date = as_date(Date))

loads_flow_cf <- 
  read_excel(
    path = paste0(
      sharepoint_path,
      "/YB_NetMeHgLoad_and_Flow_2006.xlsx"
    )
  ) %>% 
  # Clean up date formatting- extract date from dttm variable
  mutate(SampleDate = as_date(SampleDate))

part_conc_calc <- 
  read_csv(
    file = paste0(
      sharepoint_path,
      "/Particulate_Conc.csv"
    )
  )

field_data <- 
  read_excel(
    path = paste0(
      sharepoint_path,
      "/FieldMeasurements.xlsx"
    )
  ) %>% 
  # Clean up date and time formatting- extract date and time from dttm variables
  mutate(
    SampleDate = as_date(SampleDate),
    CollectionTimePST = hms::as_hms(CollectionTimePST)
  ) %>% 
  # Rename some variables
  rename(
    WaterTempC = "Water Temperature",
    SpCond = "Specific Conductance",
    DissOxy = "Dissolved Oxygen"
  )

comb_param_calc <- 
  read_csv(
    file = paste0(
      sharepoint_path,
      "/CombinedParameters.csv"
    )
  )

qa_field_blanks <- 
  read_excel(
    path = paste0(
      sharepoint_path,
      "/YB_Inlet-Outlet_Conc_Data.xlsx"
    ),
    sheet = "Field and Filter Blanks"
  ) %>% 
  # Rename some variables
  rename(
    SampleCode = "Sample Code",
    StationName = "Station Name",
    SampleDate = "Sample Date",
    CollectionTimePST = "Collection Time (PST)",
    LabComments = "Lab Comments",
    MME_Comments = "MME Comments",
    AmbSampConc = "Conc of ambient sample",
    Perc_BlankConc_AmbConc = "% Blank Conc/ Ambient Conc",
    QualCode = "Qual Code"
  ) %>% 
  # Clean up date and time formatting- extract date and time from dttm variables
  mutate(
    SampleDate = as_date(SampleDate),
    CollectionTimePST = hms::as_hms(CollectionTimePST)
  )

qa_field_dups <- 
  read_excel(
    path = paste0(
      sharepoint_path,
      "/YB_Inlet-Outlet_Conc_Data.xlsx"
    ),
    sheet = "Field Duplicates"
  ) %>% 
  # Rename some variables
  rename(
    SampleCode_PS = "Sample Code- Parent Sample",
    SampleCode_FD = "Sample Code- Field Dup",
    StationName = "Station Name",
    SampleDate = "Sample Date",
    CollectionTimePST_PS = "Collection Time (PST)- Parent Sample",
    CollectionTimePST_FD = "Collection Time (PST)- Field Dup",
    Result_PS = "Result- Parent Sample",
    Result_FD = "Result- Field Dup",
    LabComments = "Lab Comments",
    MME_Comments = "MME Comments",
    QualCode = "Qual Code"
  ) %>% 
  # Clean up date and time formatting- extract date and time from dttm variables
  mutate(
    SampleDate = as_date(SampleDate),
    CollectionTimePST_PS = hms::as_hms(CollectionTimePST_PS),
    CollectionTimePST_FD = hms::as_hms(CollectionTimePST_FD)
  )

# Create openwaterhg package data
use_data(
  comb_param_calc,
  conc_data,
  daily_flow_data_all,
  daily_flow_data_se,
  field_data,
  loads_calc,
  loads_flow_cf,
  part_conc_calc,
  qa_field_blanks,
  qa_field_dups
)

