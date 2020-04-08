# Yolo Bypass Mass Balance Study
# Purpose: Tables for Final Report - Technical Appendix
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(lubridate)
library(openwaterhg)

# Define Analyte order for Tables
analyte_order <- c(
  "THg- total",
  "THg- filtered",
  "THg- particulate",
  "MeHg- total",
  "MeHg- filtered",
  "MeHg- particulate",
  "TSS",
  "TOC",
  "DOC",
  "POC"
)


# Table B-6 ---------------------------------------------------------------
# Water Flows and Water Balances for each sampling event
# All 11 sampling events

# Sum the inlet and outlet flows for each sampling event
se_flows_sum <- daily_flow_data_se %>% 
  filter(LocType != "Below Liberty") %>% 
  group_by(SamplingEvent, LocType) %>% 
  summarize(total_flow = signif(sum(Flow), 4)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = LocType, values_from = total_flow)

# Calculate Below Liberty Island flows for each sampling event
se_flows_bli <- daily_flow_data_se %>% 
  filter(LocType == "Below Liberty") %>% 
  pivot_wider(names_from = StationName, values_from = Flow) %>% 
  rename(
    cache = "Cache Slough near Ryer Island",
    miner = "Miner Slough near Sac River"
  ) %>% 
  mutate(bli_flow = signif(cache - miner, 4)) %>% 
  # Remove data for April 11-12, 2017 sampling event
  filter(SamplingEvent != "Apr 11-12, 2017") %>% 
  select(SamplingEvent, bli_flow)

# Join se_flows_sum and se_flows_bli df's together
se_flows <- left_join(se_flows_sum, se_flows_bli)

# Calculate water balances for each sampling event
se_water_bal <- se_flows %>% 
  mutate(
    per_outlet = round(Outlet/Inlet * 100),
    per_bli = round(bli_flow/Inlet * 100)
  ) %>% 
  conv_fact_samplingevent() %>% 
  arrange(SamplingEvent)

# Clean up
rm(se_flows, se_flows_bli, se_flows_sum)

# Export Table B-6
se_water_bal %>% write_excel_csv("table_b-6.csv", na = "N/A")


# Table B-9 ---------------------------------------------------------------
# Total Water Volumes and Water Balances in the Yolo Bypass 
# For Flood Events in 2014, 2016, and 2017

# Prepare flow data for calculations
flow_data_clean <- daily_flow_data_all %>% 
  # Subtract one day from the Outlet and BLI stations to account for 1-day lag in travel time
  mutate(date_adj = if_else(LocType == "Inlet", Date, Date - 1)) %>% 
  # Filter time periods for calculations
  filter(
    (date_adj >= "2014-12-19" & date_adj <= "2014-12-23") |
    (date_adj >= "2016-03-14" & date_adj <= "2016-03-19") |
    (date_adj >= "2017-01-09" & date_adj <= "2017-05-04")
  )

# Calculate Below Liberty Island flows
flow_data_clean_bli <- flow_data_clean %>% 
  filter(LocType == "Below Liberty") %>% 
  pivot_wider(names_from = StationName, values_from = Flow) %>% 
  rename(
    cache = "Cache Slough near Ryer Island",
    miner = "Miner Slough near Sac River"
  ) %>% 
  mutate(Flow = cache - miner) %>% 
  select(Year, LocType, Flow)

# Calculate total water volumes for each year
water_vol_total <- flow_data_clean %>% 
  filter(LocType != "Below Liberty") %>% 
  bind_rows(flow_data_clean_bli) %>% 
  group_by(Year, LocType) %>% 
  summarize(total_vol_maf = signif(sum(Flow)*60*60*24/43560/1e6, 3)) %>% 
  ungroup()

# Calculate water balances for each year
water_bal <- water_vol_total %>% 
  pivot_wider(names_from = LocType, values_from = total_vol_maf) %>% 
  rename(bli = "Below Liberty") %>% 
  mutate(
    per_outlet = round(Outlet/Inlet * 100),
    per_bli = round(bli/Inlet * 100)
  )

# Clean up
rm(flow_data_clean, flow_data_clean_bli, water_vol_total)

# Export Table B-9
water_bal %>% write_excel_csv("table_b-9.csv", na = "N/A")


# Table B-10 --------------------------------------------------------------
# Summary statistics for total inlet loads of Hg, MeHg, Organic Carbon and Suspended Solids
# For just the sampling events in 2017

# Summarize inlet load data for Table B-10
inlet_loads_summ <- loads_calc %>% 
  # filter a subset of load data
  filter(
    Year == 2017,
    LocType == "Inlet",
    str_detect(Analyte, "OC$|Hg|TSS"),
  ) %>%
  # Calculate total inlet loads for each sampling event and analyte
  group_by(SamplingEvent, Analyte, LoadUnits) %>% 
  summarize(total_load = sum(Load)) %>% 
  ungroup() %>% 
  # Calculate summary statistics for each analyte
  summ_stat(total_load, Analyte, LoadUnits) %>% 
  # Round to 3 significant digits
  mutate_at(vars(Mean:IQR), signif, digits = 3) %>% 
  select(Analyte, LoadUnits, Minimum, Maximum, Median, Mean, StDev) %>% 
  # Define analyte order for table
  mutate(Analyte = factor(Analyte, levels = analyte_order)) %>% 
  arrange(Analyte)

# Export Table B-10
inlet_loads_summ %>% write_excel_csv("table_b-10.csv")



