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
# 2015 Pilot Study
# Bar plot of means of 4 groups with error bars indicating their standard deviations
# Means are of the mass in ng of filtered MeHg in the overlying water after 12 days of incubation

# Import Data
vss_pilot2015_orig <- read_excel(path = paste0(sharepoint_path, "/VegSens_PilotStudies_Data.xlsx"), sheet = "Dec2015")
glimpse(vss_pilot2015_orig)

# Prepare data for plot
vss_pilot2015_clean <- vss_pilot2015_orig %>% 
  # Just analyze Day 12 results due to issues with repeated measurements
  filter(Day == 12) %>% 
  # Create a variable CombineGroup that is a combination of Treatment and Aeration variables
  mutate(
    CombineGroup = case_when(
      Treatment == "Vegetated" & Aeration == "Yes" ~ "Veg with Air",
      Treatment == "Vegetated" & Aeration == "No" ~ "Veg without Air",
      Treatment == "Sediment Only" & Aeration == "Yes" ~ "Sed with Air",
      Treatment == "Sediment Only" & Aeration == "No" ~ "Sed without Air"
    )
  ) %>% 
  # Calculate means and standard deviations
  group_by(CombineGroup) %>% 
  summarize(
    avg = mean(dMeHg),
    stdev = sd(dMeHg)
  )

# Create figure
vss_pilot2015_fig <- vss_pilot2015_clean %>% 
  ggplot(aes(x = CombineGroup, y = avg)) +
  geom_col(fill = "#00588B") +
  geom_errorbar(
    aes(
      ymin = avg - stdev, 
      ymax = avg + stdev, 
      width = 0.25
    )
  )

