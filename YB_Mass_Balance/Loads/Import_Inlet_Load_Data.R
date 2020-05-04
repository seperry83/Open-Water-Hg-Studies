# Yolo Bypass Inlet-Outlet Study
# Purpose: A script that imports the load data for the inlets and formats it to be 
# used in further calculations. 
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(openwaterhg)

# Import load data for the inlets
temp_loads_inlet_orig <- loads_calc %>% filter(LocType == "Inlet") 

# Sum the CCSB Loads
temp_ccsb_loads <- filter(temp_loads_inlet_orig, str_detect(StationName, "^CCSB"))

temp_ccsb_loads_sum <- temp_ccsb_loads %>%                      
  group_by(SamplingEvent, Year, LocType, Analyte, LoadUnits) %>% 
  summarize(
    Load = sum(Load),
    digits = min(digits)
  ) %>% 
  ungroup() %>% 
  mutate(StationName = "CCSB")

# Add back the summed CCSB loads and format data for further analysis
temp_loads_inlet1 <- temp_loads_inlet_orig %>% 
  anti_join(temp_ccsb_loads, by = c("SamplingEvent", "StationName", "Analyte")) %>% 
  bind_rows(temp_ccsb_loads_sum) %>% 
  # rename some station names
  mutate(
    StationName = case_when(
      StationName == "Knights Landing Ridge Cut" ~ "KLRC",
      StationName == "Putah Creek at Mace Blvd" ~ "Putah Creek",
      StationName == "Sac River above the Sacramento Weir" ~ "Sacramento Weir",
      TRUE ~ StationName
    )
  )

# Pull out digits variable to be joined after completing the zero load values
temp_signif_digits <- temp_loads_inlet1 %>% 
  select(SamplingEvent, StationName, Analyte, digits)

# Add zero load values to Fremont and Sacramento Weirs when they weren't spilling
temp_loads_inlet_c <- temp_loads_inlet1 %>% 
  select(-digits) %>% 
  complete(
    nesting(SamplingEvent, Year), StationName, LocType, nesting(Analyte, LoadUnits),
    fill = list(Load = 0)
  ) %>% 
  filter(!(SamplingEvent == "Dec 22-23, 2014" & str_detect(Analyte, "^Iron|^Manganese"))) %>% 
  left_join(temp_signif_digits)

# Create a dataframe of these added zero load values to be removed from some of the
# analyses later
zero_loads <- 
  anti_join(
    temp_loads_inlet_c, 
    temp_loads_inlet1, 
    by = c("SamplingEvent", "StationName", "Analyte")
  ) %>% 
  select(SamplingEvent, StationName, Analyte)

# Keep the zero load values in loads_inlet df for now
loads_inlet <- temp_loads_inlet_c

# Clean up
rm(
  temp_ccsb_loads,
  temp_ccsb_loads_sum,
  temp_loads_inlet_c,
  temp_loads_inlet_orig,
  temp_loads_inlet1,
  temp_signif_digits
)

