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
  "VSS",
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
  summarize(total_flow = sum(Flow)) %>%
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
  mutate(bli_flow = cache - miner) %>% 
  # Remove data for April 11-12, 2017 sampling event
  filter(SamplingEvent != "Apr 11-12, 2017") %>% 
  select(SamplingEvent, bli_flow)

# Join se_flows_sum and se_flows_bli df's together
se_flows <- left_join(se_flows_sum, se_flows_bli)

# Calculate water balances for each sampling event
se_water_bal <- se_flows %>% 
  mutate(
    per_outlet = round(Outlet/Inlet * 100),
    per_bli = round(bli_flow/Inlet * 100),
  ) %>% 
  # Round total flows to 4 significant figures
  mutate_at(vars(Inlet, Outlet, bli_flow), signif, digits = 4) %>% 
  conv_fact_samplingevent() %>% 
  arrange(SamplingEvent)

# Export Table B-6
se_water_bal %>% write_excel_csv("table_b-6.csv", na = "N/A")

# Clean up
rm(se_flows, se_flows_bli, se_flows_sum, se_water_bal)


# Table B-7 ---------------------------------------------------------------
# Summary of Field and Filter Blanks

# Filter only Analytes used in the report
blanks <- qa_field_blanks %>% 
  filter(!str_detect(Analyte, "^Chl|^Iron|Manganese- f|^Pot|^Sul|UVA"))

# Count all blank samples
blanks_c_all <- blanks %>% 
  count(StationName, Analyte, Units) %>% 
  rename(N_all = n)

# Count # of detected blanks
blanks_c_det <- blanks %>% 
  filter(!str_detect(Result, "<")) %>% 
  count(StationName, Analyte) %>% 
  rename(N_det = n)

# Make a table of RL and MDL values
det_limits <- blanks %>% 
  count(Analyte, RL, MDL) %>% 
  select(-n) %>% 
  filter(!(Analyte == "UVA 254" & RL == 0.001))

# Summarize min and max values for just detected blanks
det_summ <- blanks %>% 
  filter(!str_detect(Result, "<")) %>% 
  mutate(Result = as.numeric(Result)) %>% 
  group_by(StationName, Analyte) %>% 
  summarize(
    Det_Min = min(Result),
    Det_Max = max(Result),
    Det_Med = median(Result)
  ) %>% 
  ungroup()

# Combine all df's together to make a summary table for report
blanks_summ <- 
  reduce(
    list(blanks_c_all, det_limits, blanks_c_det, det_summ),
    left_join
  ) %>% 
  replace_na(list(N_det = 0)) %>% 
  mutate(Per_Det = N_det/N_all) %>% 
  select(
    StationName,
    Analyte,
    Units,
    RL,
    MDL,
    N_all,
    N_det,
    Per_Det,
    Det_Min,
    Det_Max,
    Det_Med
  )

# Export Table B-7
blanks_summ %>% write_excel_csv("table_b-7.csv", na = "")

# Clean up
rm(blanks, blanks_c_all, blanks_c_det, blanks_summ, det_limits, det_summ)


# Table B-8 ---------------------------------------------------------------
# Summary of field duplicates

# Filter only Analytes used in the report
fdups <- qa_field_dups %>% 
  filter(!str_detect(Analyte, "^Chl|^Iron|Manganese- f|^Pot|^Sul|UVA"))

# Count the total # of Field Dups for each Analyte - before removing < MDL or < RL values
fdups_c_all <- fdups %>% 
  count(Analyte) %>% 
  rename(N_fd = n)

# Clean up field dups df for further analysis
fdups_clean <- fdups %>% 
  # don't include ND values in the analysis
  filter(!(str_detect(Result_PS, "<") | str_detect(Result_FD, "<"))) %>% 
  # convert a few character variables to numeric
  mutate(
    Result_PS = as.numeric(Result_PS),
    Result_FD = as.numeric(Result_FD)
  )

# Summarize RPD values for each Analyte
rpd_summ <- fdups_clean %>% 
  group_by(Analyte) %>% 
  summarize(
    Min_rpd = min(RPD),
    Max_rpd = max(RPD),
    Med_rpd = median(RPD)
  )

# Count # of duplicate pairs flagged with "FV"
fdups_fv <- fdups_clean %>% 
  filter(Flag == "FV") %>% 
  count(Analyte) %>% 
  rename(N_FV = n)

# Join df together to make a summary table for report
fdups_summ <- 
  reduce(
    list(fdups_c_all, rpd_summ, fdups_fv),
    left_join
  ) %>% 
  replace_na(list(N_FV = 0)) %>% 
  mutate(Per_FV = N_FV/N_fd)

# Export fdups.summ df
fdups_summ %>% write_excel_csv("table_b-8.csv")

# Clean up
rm(fdups, fdups_c_all, fdups_clean, fdups_fv, fdups_summ, rpd_summ)


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
  summarize(total_vol_maf = sum(Flow)*60*60*24/43560/1e6) %>%
  ungroup()

# Calculate water balances for each year
water_bal <- water_vol_total %>% 
  pivot_wider(names_from = LocType, values_from = total_vol_maf) %>% 
  rename(bli = "Below Liberty") %>% 
  mutate(
    per_outlet = round(Outlet/Inlet * 100),
    per_bli = round(bli/Inlet * 100)
  ) %>% 
  # Round total volumes to 3 significant figures
  mutate_at(vars(Inlet, Outlet, bli), signif, digits = 3)

# Export Table B-9
water_bal %>% write_excel_csv("table_b-9.csv", na = "N/A")

# Clean up
rm(flow_data_clean, flow_data_clean_bli, water_vol_total, water_bal)


# Table B-10 --------------------------------------------------------------
# Summary statistics for total inlet loads of Hg, MeHg, Organic Carbon and Suspended Solids
# For just the sampling events in 2017

# Bring in total load data
source("YB_Mass_Balance/Loads/Import_Total_Load_Data.R")

# Filter total inlet load data
loads_inlet_total <- loads_total %>% 
  # Filter load data
  filter(
    LocType == "Inlet",
    Year == 2017,
    str_detect(Analyte, "OC$|Hg|SS$"),
  )

# Calcuate averages and standard deviations on non-rounded data
loads_inlet_total_summ1 <- loads_inlet_total %>% 
  group_by(Analyte, LoadUnits) %>% 
  summarize(
    sign_digits = min(digits),
    Mean = signif(mean(total_load), sign_digits),
    StDev = signif(sd(total_load), sign_digits)
  ) %>% 
  ungroup() %>% 
  select(-sign_digits)

# Calculate all other summary statistics on rounded data
loads_inlet_total_summ2 <- loads_inlet_total %>% 
  mutate(total_load = signif(total_load, digits)) %>% 
  summ_stat(total_load, Analyte) %>% 
  select(-c(Mean, StDev))

# Join Summary Statistics together for Table B-10
loads_inlet_total_summ <- 
  left_join(loads_inlet_total_summ1, loads_inlet_total_summ2) %>% 
  select(Analyte, LoadUnits, Minimum, Maximum, Median, Mean, StDev) %>% 
  # Define analyte order for table
  mutate(Analyte = factor(Analyte, levels = analyte_order)) %>%
  arrange(Analyte)

# Export Table B-10
loads_inlet_total_summ %>% write_excel_csv("table_b-10.csv")

# Clean up
rm(
  loads_total,
  loads_inlet_total,
  loads_inlet_total_summ,
  loads_inlet_total_summ1,
  loads_inlet_total_summ2
)


# Table B-11 --------------------------------------------------------------
# Averages and standard deviations for each inlet's load of Hg, MeHg, 
  # Organic Carbon and Suspended Solids
# For just the sampling events in 2017

# Bring in inlet load data
source("YB_Mass_Balance/Loads/Import_Inlet_Load_Data.R")

# Filter Inlet Load data
loads_inlet_clean <- loads_inlet %>% 
  filter(
    Year == 2017,
    str_detect(Analyte, "OC$|Hg|SS$"),
  ) %>% 
  # apply factor order to Inlets
  conv_fact_inlet_names()

# Summarize inlet load data for Table B-11
loads_inlet_summ <- loads_inlet_clean %>% 
  # Calculate averages and standard deviations of loads for each inlet
  group_by(StationName, Analyte, LoadUnits) %>% 
  summarize(
    sign_digits = min(digits, na.rm = TRUE),
    Mean = signif(mean(Load), sign_digits),
    StDev = signif(sd(Load), sign_digits)
  ) %>% 
  ungroup() %>% 
  select(-sign_digits)

# Pivot the averages
loads_inlet_avg <- loads_inlet_summ %>% 
  pivot_wider(
    id_cols = -StDev,
    names_from = StationName,
    values_from = Mean
  )

# Pivot the standard deviations
loads_inlet_stdev <- loads_inlet_summ %>% 
  pivot_wider(
    id_cols = -Mean,
    names_from = StationName,
    values_from = StDev
  ) %>% 
  select(-LoadUnits)

# Join averages and standard deviations together into one df
loads_inlet_summ_c <- 
  left_join(
    loads_inlet_avg, loads_inlet_stdev,
    by = c("Analyte"),
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
loads_inlet_summ_c %>% write_excel_csv("table_b-11.csv")

# Clean up
rm(
  loads_inlet,
  loads_inlet_avg,
  loads_inlet_clean,
  loads_inlet_stdev,
  loads_inlet_summ,
  loads_inlet_summ_c,
  zero_loads
)


# Table B-12 --------------------------------------------------------------
# Summary statistics for total loads of Hg, MeHg, Organic Carbon and Suspended Solids
# Totals for the outlet at the Stairsteps and Below Liberty Island
# For just the sampling events in 2017

# Bring in total load data
source("YB_Mass_Balance/Loads/Import_Total_Load_Data.R")

# Filter total load data
loads_total_clean <- loads_total %>% 
  # Filter load data
  filter(
    LocType != "Inlet",
    Year == 2017,
    str_detect(Analyte, "OC$|Hg|SS$"),
  )

# Calcuate averages and standard deviations on non-rounded data
loads_total_clean_summ1 <- loads_total_clean %>%
  group_by(LocType, Analyte, LoadUnits) %>% 
  summarize(
    sign_digits = min(digits),
    Mean = signif(mean(total_load), sign_digits),
    StDev = signif(sd(total_load), sign_digits)
  ) %>% 
  ungroup() %>% 
  select(-sign_digits)

# Calculate all other summary statistics on rounded data
loads_total_clean_summ2 <- loads_total_clean %>% 
  mutate(total_load = signif(total_load, digits)) %>%
  summ_stat(total_load, LocType, Analyte) %>% 
  select(-c(Mean, StDev))

# Join Summary Statistics together for Table B-12
loads_total_clean_summ <- 
  left_join(loads_total_clean_summ1, loads_total_clean_summ2) %>% 
  select(LocType, Analyte, LoadUnits, Minimum, Maximum, Median, Mean, StDev) %>% 
  # Define analyte order for table
  mutate(Analyte = factor(Analyte, levels = analyte_order)) %>%
  arrange(Analyte) %>% 
  group_nest(LocType)

# Export Table B-12
  # Outlet Loads
  loads_total_clean_summ %>% 
    filter(LocType == "Outlet") %>% 
    pull(data) %>% 
    chuck(1) %>% 
    write_excel_csv("table_b-12_outlet.csv")
  
  # Below Liberty Island Loads
  loads_total_clean_summ %>% 
    filter(LocType == "Below Liberty") %>% 
    pull(data) %>% 
    chuck(1) %>% 
    write_excel_csv("table_b-12_bli.csv")
  
# Clean up
rm(
  loads_total,
  loads_total_clean,
  loads_total_clean_summ,
  loads_total_clean_summ1,
  loads_total_clean_summ2
)


# Table B-13 --------------------------------------------------------------
# Averages and standard deviations for net loads of Hg, MeHg, Organic Carbon and Suspended Solids
# For 3 segments of the Yolo Bypass: upper reach, Liberty Island, and entire Bypass
# For just the sampling events in 2017

# Bring in total load data
source("YB_Mass_Balance/Loads/Import_Net_Load_Data.R")

# Filter net load data
loads_net_clean <- loads_net %>% 
  # Filter load data
  filter(
    Year == 2017,
    str_detect(Analyte, "OC$|Hg|SS$"),
  )

# START HERE
# Summarize net load data for each reach for Table B-13
net_loads_summ <- net_loads %>% 
  # Calculate averages and standard deviations of net loads for each reach
  group_by(Reach, Analyte, LoadUnits) %>% 
  summarize(
    avg_load = signif(mean(net_load), 3),
    stdev_load = signif(sd(net_load), 3)
  ) %>% 
  ungroup()

# Pivot the averages
net_loads_avg <- net_loads_summ %>% 
  pivot_wider(
    id_cols = c(Analyte, LoadUnits),
    names_from = Reach,
    values_from = avg_load
  )

# Pivot the standard deviations
net_loads_stdev <- net_loads_summ %>% 
  pivot_wider(
    id_cols = c(Analyte, LoadUnits),
    names_from = Reach,
    values_from = stdev_load
  )

# Join averages and standard deviations together into one df
net_loads_summ_c <- 
  left_join(
    net_loads_avg, net_loads_stdev,
    by = c("Analyte", "LoadUnits"),
    suffix = c("_avg", "_stdev")
  ) %>% 
  mutate(Analyte = factor(Analyte, levels = analyte_order)) %>% 
  arrange(Analyte) %>% 
  select(
    Analyte,
    LoadUnits,
    starts_with("Upper"),
    starts_with("Liberty"),
    starts_with("Entire")
  )

# Export Table B-13
net_loads_summ_c %>% write_excel_csv("table_b-13.csv")

