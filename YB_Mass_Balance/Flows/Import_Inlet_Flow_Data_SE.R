# Yolo Bypass Inlet-Outlet Study
# Purpose: A script that imports the daily average flow data for the inlets for the 11 
# sampling events and formats it to be used in further calculations. 
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(openwaterhg)

# Import daily average flow data for the inlets
temp_flows_inlet_orig <- daily_flow_data_se %>% filter(LocType == "Inlet")

# Sum the CCSB Flows
temp_ccsb_flow <- filter(temp_flows_inlet_orig, str_detect(StationName, "^CCSB"))

temp_ccsb_flow_sum <- temp_ccsb_flow %>%                      
  group_by(SamplingEvent, Year, LocType) %>% 
  summarize(Flow = sum(Flow)) %>% 
  ungroup() %>% 
  mutate(StationName = "CCSB")

# Add back the summed CCSB flows and format data for further analysis
flows_inlet_se <- temp_flows_inlet_orig %>% 
  anti_join(temp_ccsb_flow, by = c("SamplingEvent", "StationName")) %>% 
  bind_rows(temp_ccsb_flow_sum) %>% 
  # rename some station names
  mutate(
    StationName = case_when(
      StationName == "Knights Landing Ridge Cut" ~ "KLRC",
      StationName == "Putah Creek at Mace Blvd" ~ "Putah Creek",
      StationName == "Sac River above the Sacramento Weir" ~ "Sacramento Weir",
      TRUE ~ StationName
    )
  )

# Clean up
rm(temp_flows_inlet_orig, temp_ccsb_flow, temp_ccsb_flow_sum)

