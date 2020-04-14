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
# Summary statistics for total loads of Hg, MeHg, Organic Carbon and Suspended Solids
# Totals for inlets, outlet at the Stairsteps, and Below Liberty Island
# For just the sampling events in 2017

# Summarize total load data for Table B-10
total_loads_summ <- loads_calc %>% 
  # Filter load data
  filter(
    Year == 2017,
    str_detect(Analyte, "OC$|Hg|TSS"),
  ) %>% 
  # Calculate total loads for each sampling event and analyte
  group_by(SamplingEvent, Analyte, LoadUnits, LocType) %>% 
  summarize(total_load = sum(Load)) %>% 
  ungroup() %>% 
  # Define analyte order for table
  mutate(Analyte = factor(Analyte, levels = analyte_order)) %>% 
  # Calculate summary statistics for each LocType and analyte
  group_nest(LocType) %>% 
  mutate(
    summ_stats = map(
      data,
      .f = ~summ_stat(.x, total_load, Analyte, LoadUnits) %>% 
        mutate_at(vars(Mean:IQR), signif, digits = 3) %>% 
        select(Analyte, LoadUnits, Minimum, Maximum, Median, Mean, StDev)
    )
  )

# Export Table B-10
  # Inlets 
  total_loads_summ %>% 
    filter(LocType == "Inlet") %>% 
    pull(summ_stats) %>% 
    chuck(1) %>% 
    write_excel_csv("table_b-10_inlets.csv")

  # Outlets at Stairsteps
  total_loads_summ %>% 
    filter(LocType == "Outlet") %>% 
    pull(summ_stats) %>% 
    chuck(1) %>% 
    write_excel_csv("table_b-10_outlet.csv")
  
  # Below Liberty Island
  total_loads_summ %>% 
    filter(LocType == "Below Liberty") %>% 
    pull(summ_stats) %>% 
    chuck(1) %>% 
    write_excel_csv("table_b-10_bli.csv")
  

# Table B-11 --------------------------------------------------------------
# Averages and standard deviations for each inlet's load of Hg, MeHg, Organic Carbon and Suspended Solids
# For just the sampling events in 2017

# Filter Inlet Load data
inlet_loads <- loads_calc %>% 
  filter(
    Year == 2017,
    LocType == "Inlet",
    str_detect(Analyte, "OC$|Hg|TSS"),
  ) %>% 
  # remove a few variables
  select(-c(Year, Conc:LocType))

# Sum the CCSB Loads
ccsb_loads <- filter(inlet_loads, str_detect(StationName, "^CCSB"))

ccsb_loads_sum <- ccsb_loads %>%                      
  group_by(SamplingEvent, Analyte, LoadUnits) %>% 
  summarize(Load = sum(Load)) %>% 
  ungroup() %>% 
  mutate(StationName = "CCSB")

# Add back the summed CCSB loads
inlet_loads <- inlet_loads %>% 
  anti_join(ccsb_loads) %>% 
  bind_rows(ccsb_loads_sum) %>% 
  # rename some station names
  mutate(
    StationName = case_when(
      StationName == "Knights Landing Ridge Cut" ~ "KLRC",
      StationName == "Putah Creek at Mace Blvd" ~ "Putah Creek",
      StationName == "Sac River above the Sacramento Weir" ~ "Sacramento Weir",
      TRUE ~ StationName
    )
  ) %>% 
  # apply factor order to Inlets
  conv_fact_inlet_names()

# Clean up
rm(ccsb_loads, ccsb_loads_sum)
  
# Summarize inlet load data for Table B-11
inlet_loads_summ <- inlet_loads %>% 
  # Add zero load values to Fremont and Sacramento Weirs when they weren't spilling
  complete(
    SamplingEvent, StationName, nesting(Analyte, LoadUnits),
    fill = list(Load = 0)
  ) %>% 
  # Calculate averages and standard deviations of loads for each inlet
  group_by(StationName, Analyte, LoadUnits) %>% 
  summarize(
    avg_load = signif(mean(Load), 3),
    stdev_load = signif(sd(Load), 3)
  ) %>% 
  ungroup()

# Pivot the averages
inlet_loads_avg <- inlet_loads_summ %>% 
  pivot_wider(
    id_cols = c(Analyte, LoadUnits),
    names_from = StationName,
    values_from = avg_load
  )

# Pivot the standard deviations
inlet_loads_stdev <- inlet_loads_summ %>% 
  pivot_wider(
    id_cols = c(Analyte, LoadUnits),
    names_from = StationName,
    values_from = stdev_load
  )

# Join averages and standard deviations together into one df
inlet_loads_summ_c <- 
  left_join(
    inlet_loads_avg, inlet_loads_stdev,
    by = c("Analyte", "LoadUnits"),
    suffix = c("_avg", "_stdev")
  ) %>% 
  mutate(Analyte = factor(Analyte, levels = analyte_order)) %>% 
  arrange(Analyte) %>% 
  select(
    Analyte,
    LoadUnits,
    starts_with("KLRC"),
    starts_with("CCSB"),
    starts_with("Putah"),
    starts_with("Sac"),
    starts_with("Fre"),
  )

# Export Table B-11
inlet_loads_summ_c %>% write_excel_csv("table_b-11.csv")


# Table B-12 --------------------------------------------------------------
# Summary statistics for net loads of Hg, MeHg, Organic Carbon and Suspended Solids
# For 3 segments of the Yolo Bypass: upper reach, Liberty Island, and entire Bypass
# For just the sampling events in 2017

# Calculate total input and output loads for each sampling event and parameter
total_loads <- loads_calc %>% 
  # Only include a subset of the data
  filter(
    Year == 2017,
    str_detect(Analyte, "OC$|Hg|TSS")
  ) %>%
  # Group and sum load data
  group_by(SamplingEvent, Analyte, LoadUnits, LocType) %>% 
  summarize(total_load = sum(Load)) %>% 
  ungroup()

# Calculate net loads for each reach
net_loads <- total_loads %>% 
  pivot_wider(names_from = LocType, values_from = total_load) %>% 
  rename(below_liberty = "Below Liberty") %>% 
  # Calculate net loads for each reach
  mutate(
    Upper = Outlet - Inlet,
    Liberty = below_liberty - Outlet,
    Entire = below_liberty - Inlet
  ) %>% 
  select(-c(Inlet:below_liberty)) %>% 
  pivot_longer(
    cols = Upper:Entire,
    names_to = "Reach",
    values_to = "net_load"
  ) %>% 
  filter(!is.na(net_load))

# Calculate summary statistics of net loads for each reach
net_loads_summ <- net_loads %>% 
  mutate(Analyte = factor(Analyte, levels = analyte_order)) %>% 
  group_nest(Reach) %>% 
  mutate(
    summ_stats = map(
      data,
      .f = ~summ_stat(.x, net_load, Analyte, LoadUnits) %>% 
        mutate_at(vars(Mean:IQR), signif, digits = 3) %>% 
        select(Analyte, LoadUnits, Minimum, Maximum, Median, Mean, StDev)
    )
  )

# Export Table B-12
  # Upper reach 
  net_loads_summ %>% 
    filter(Reach == "Upper") %>% 
    pull(summ_stats) %>% 
    chuck(1) %>% 
    write_excel_csv("table_b-12_upper.csv")

  # Liberty Island reach 
  net_loads_summ %>% 
    filter(Reach == "Liberty") %>% 
    pull(summ_stats) %>% 
    chuck(1) %>% 
    write_excel_csv("table_b-12_lib.csv")
  
  # Entire Bypass
  net_loads_summ %>% 
    filter(Reach == "Entire") %>% 
    pull(summ_stats) %>% 
    chuck(1) %>% 
    write_excel_csv("table_b-12_entire.csv")

