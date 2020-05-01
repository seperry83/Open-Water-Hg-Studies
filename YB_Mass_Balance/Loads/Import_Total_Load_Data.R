# Yolo Bypass Inlet-Outlet Study
# Purpose: A script that calculates the total loads for the inputs, outputs, and below 
# Liberty Island and formats it to be used in further calculations. 
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(openwaterhg)

# Calculate total loads for each LocType
loads_total <- loads_calc %>% 
  # Group and sum load data
  group_by(SamplingEvent, Year, LocType, Analyte, LoadUnits) %>% 
  summarize(
    total_load = sum(Load),
    digits = min(digits)
  ) %>% 
  ungroup()

