# Yolo Bypass Inlet-Outlet Study
# Purpose: A script that imports the daily average flow data for the inlets 
# and formats it to be used in further calculations. 
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(openwaterhg)

# Import daily average flow data for the inlets
flows_inlet_orig <- daily_flow_data_all %>% filter(LocType == "Inlet")

# Sum the CCSB Flows
ccsb_flow <- filter(flows_inlet_orig, str_detect(StationName, "^CCSB"))

ccsb_flow_sum <- ccsb_flow %>%                      
  group_by(Date, Year, LocType) %>% 
  summarize(Flow = sum(Flow)) %>% 
  ungroup() %>% 
  mutate(StationName = "CCSB")

# Add back the summed CCSB flows and format for further analysis
flows_inlet_all <- flows_inlet_orig %>% 
  anti_join(ccsb_flow, by = c("Date", "StationName")) %>% 
  bind_rows(ccsb_flow_sum) %>% 
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
rm(flows_inlet_orig, ccsb_flow, ccsb_flow_sum)

