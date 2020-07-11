# Yolo Bypass Mass Balance Study
# Purpose: Tables for Final Report - Technical Appendix
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(lubridate)
library(openwaterhg)


# Functions for script ----------------------------------------------------

# Create a function to rename inlet station names
rename_inlet_sta <- function(df) {
  df <- df %>% 
    mutate(
      StationName = case_when(
        str_detect(StationName, "^CCSB") ~ "CCSB",
        str_detect(StationName, "^Fre") ~ "Fremont",
        str_detect(StationName, "^Kni") ~ "KLRC",
        str_detect(StationName, "^Put") ~ "Putah",
        str_detect(StationName, "^Sac") ~ "Sac_Weir"
      )
    ) 
  
  return(df)
}

# Create a function to apply a plotting order for the analytes
conv_fact_analytes <- function(df) {
  analytes_order <- c(
    "THg- total",
    "THg- filtered",
    "THg- particulate",
    "MeHg- total",
    "MeHg- filtered",
    "MeHg- particulate",
    "TOC",
    "DOC",
    "POC",
    "TSS",
    "VSS"
  )
  
  df <- df %>% mutate(Analyte = factor(Analyte, levels = analytes_order))
  
  return(df)
}

obj_keep <- c("obj_keep", "rename_inlet_sta", "conv_fact_analytes")


# Table B-6 ---------------------------------------------------------------
# Water Flows and Water Balances for each sampling event
# All 11 sampling events

# Define table number for easier updating
tbl_num <- as.character(6)

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
    per_outlet = round(Outlet/Inlet, 2),
    per_bli = round(bli_flow/Inlet, 2),
  ) %>% 
  # Round total flows to 4 significant figures
  mutate_at(vars(Inlet, Outlet, bli_flow), signif, digits = 4) %>% 
  conv_fact_samplingevent() %>% 
  arrange(SamplingEvent)

# Export Table
se_water_bal %>% write_excel_csv(paste0("table_b-", tbl_num, ".csv"), na = "N/A")

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Table B-7 ---------------------------------------------------------------
# Summary of Field and Filter Blanks

# Define table number for easier updating
tbl_num <- as.character(7)

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
  select(-n)

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
  ungroup() %>% 
  # Round the medians for THg to appropriate number of decimal places
  mutate(Det_Med = if_else(str_detect(Analyte, "^THg"), round(Det_Med, 1), Det_Med))

# Combine all df's together to make a summary table for report
blanks_summ <- 
  reduce(
    list(blanks_c_all, det_limits, blanks_c_det, det_summ),
    left_join
  ) %>% 
  replace_na(list(N_det = 0)) %>% 
  mutate(Per_Det = round(N_det/N_all, 2)) %>% 
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

# Export Table
blanks_summ %>% write_excel_csv(paste0("table_b-", tbl_num, ".csv"), na = "")

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Table B-8 ---------------------------------------------------------------
# Summary of field duplicates

# Define table number for easier updating
tbl_num <- as.character(8)

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
  mutate(Per_FV = round(N_FV/N_fd, 2))

# Export Table
fdups_summ %>% write_excel_csv(paste0("table_b-", tbl_num, ".csv"))

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Table B-9 ---------------------------------------------------------------
# Total Water Volumes and Water Balances in the Yolo Bypass 
# For Flood Events in 2014, 2016, and 2017

# Define table number for easier updating
tbl_num <- as.character(9)

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
    per_outlet = round(Outlet/Inlet, 2),
    per_bli = round(bli/Inlet, 2)
  ) %>% 
  # Round total volumes to 3 significant figures
  mutate_at(vars(Inlet, Outlet, bli), signif, digits = 3)

# Export Table
water_bal %>% write_excel_csv(paste0("table_b-", tbl_num, ".csv"), na = "N/A")

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Table B-10 --------------------------------------------------------------
# MeHg concentrations of each tributary input for the sampling events in 2017
# Table provides concentrations for each event and the overall averages, standard deviations, 
  # and coefficients of variation
# Table is divided into three sections for uMeHg, fMeHg, and pMeHg

# Define table number for easier updating
tbl_num <- as.character(10)

# Bring in concentration data
source("YB_Mass_Balance/Concentrations/Import_Conc_Data.R")

# Only keep MeHg data for the tributary inlets and for 2017
inlet_mehg_conc <- all_conc %>% 
  filter(
    str_detect(StationName, "^CCSB|^Fre|^Kni|^Put|^Sac"),
    str_detect(Analyte, "^MeHg"),
    year(SampleDate) == 2017
  ) %>% 
  # Calculate the number of significant digits in the Conc values
  mutate(
    digits = case_when(
      Conc < 0.01 ~ 1,
      Conc < 0.1 ~ 2,
      TRUE ~ 3
    )
  ) %>% 
  # Rename stations
  rename_inlet_sta() %>% 
  # Define analyte order
  conv_fact_analytes()

# Pull out data for Fremont Weir and CCSB and calculate averages and stdev of 
  # the multiple stations representing these inputs
fre_ccsb_mehg_conc <- inlet_mehg_conc %>% 
  filter(str_detect(StationName, "^CCSB|^Fre")) %>% 
  group_by(SampleDate, StationName, Analyte) %>% 
  summarize(
    digits = min(digits),
    avg_conc = mean(Conc),
    sd_conc = sd(Conc)
  ) %>% 
  ungroup() %>% 
  rename(Conc = avg_conc)


# Add back the averages and standard deviations for Fremont Weir and CCSB to 
  # all other data for data table
  
  # Round averages and std deviations to correct number of significant digits
  fre_ccsb_mehg_conc_r <- fre_ccsb_mehg_conc %>% 
    mutate(
      Conc = signif(Conc, digits),
      sd_conc = case_when(
        digits == 2 & Conc >= 0.1 ~ round(sd_conc, 2),
        digits == 2 & Conc < 0.1 ~ round(sd_conc, 3),
        TRUE ~ round(sd_conc, 3)
      )
    ) %>% 
    select(-digits)
  
  # Pivot concentrations wider
  fre_ccsb_mehg_conc_w <- fre_ccsb_mehg_conc_r %>% 
    pivot_wider(
      id_cols = -sd_conc,
      names_from = StationName,
      values_from = Conc
    )
  
  # Pivot std deviations wider- only for Fremont Weir
  fre_mehg_stdev_w <- fre_ccsb_mehg_conc_r %>% 
    pivot_wider(
      id_cols = -Conc,
      names_from = StationName,
      values_from = sd_conc
    ) %>% 
    select(-CCSB) %>% 
    # remove two sampling events in March without values for all three sampling locations 
    filter(month(SampleDate) != 3) %>% 
    rename(Fremont_sd = Fremont)
  
  # Join dataframes together for data table
  inlet_mehg_conc_tbl <- inlet_mehg_conc %>% 
    filter(!str_detect(StationName, "^CCSB|^Fre")) %>% 
    select(StationName, SampleDate, Analyte, Conc) %>% 
    pivot_wider(names_from = StationName, values_from = Conc) %>% 
    left_join(fre_ccsb_mehg_conc_w) %>% 
    left_join(fre_mehg_stdev_w) %>% 
    select(SampleDate, Analyte, KLRC, CCSB, Putah, Sac_Weir, Fremont, Fremont_sd) %>% 
    arrange(Analyte, SampleDate)

  
# Calculate averages, stdev, and CV of all sampling events
  
  # Modify dataframe with average Fremont Weir and CCSB data to bind to all other data in order to
   # calculate overall summary statistics
  fre_ccsb_mehg_conc_mod <- fre_ccsb_mehg_conc %>% select(-sd_conc)
  
  # Join dataframes together and make calculations
  mehg_conc_summ <- inlet_mehg_conc %>% 
    filter(!str_detect(StationName, "^CCSB|^Fre")) %>% 
    bind_rows(fre_ccsb_mehg_conc_mod) %>% 
    group_by(StationName, Analyte) %>% 
    summarize(
      sign_digits = min(digits),
      avg_conc = mean(Conc),
      sd_conc = sd(Conc)
    ) %>% 
    ungroup() %>% 
    mutate(
      coef_var = signif(sd_conc/avg_conc, sign_digits),
      # round averages and std deviations after calculating CV
      avg_conc = signif(avg_conc, sign_digits),
      sd_conc = case_when(
        sign_digits == 2 & avg_conc >= 0.1 ~ round(sd_conc, 2),
        sign_digits == 2 & avg_conc < 0.1 ~ round(sd_conc, 3),
        TRUE ~ round(sd_conc, 3)
      )
    ) %>% 
    # Restructure dataframe
    select(-sign_digits) %>% 
    pivot_longer(
      cols = avg_conc:coef_var,
      names_to = "summ_stat_type",
      values_to = "summ_stat_value"
    ) %>% 
    pivot_wider(names_from = StationName, values_from = summ_stat_value) %>% 
    select(Analyte, summ_stat_type, KLRC, CCSB, Putah, Sac_Weir, Fremont)

# Export Tables
inlet_mehg_conc_tbl %>% write_excel_csv(paste0("table_b-", tbl_num, "_indiv_values.csv"), na = "")
mehg_conc_summ %>% write_excel_csv(paste0("table_b-", tbl_num, "_summary_values.csv"))

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Table B-11 --------------------------------------------------------------
# Percent of particulate MeHg in tributary inputs for the sampling events in 2017
# Table provides percentages for each event and the overall averages, standard deviations, 
  # and coefficients of variation

# Define table number for easier updating
tbl_num <- as.character(11)

# Bring in concentration data
source("YB_Mass_Balance/Concentrations/Import_Conc_Data.R")

# Only keep uMeHg and pMeHg data for the tributary inlets and for 2017
inlet_mehg_conc <- all_conc %>% 
  filter(
    str_detect(StationName, "^CCSB|^Fre|^Kni|^Put|^Sac"),
    Analyte %in% c("MeHg- total", "MeHg- particulate"),
    year(SampleDate) == 2017
  ) %>% 
  select(StationName, SampleDate, Analyte, Conc)

# Calculate the percent of pMeHg for the tributary inlets in 2017
inlet_perc_pmehg <- inlet_mehg_conc %>% 
  pivot_wider(names_from = Analyte, values_from = Conc) %>% 
  rename(
    uMeHg = "MeHg- total",
    pMeHg = "MeHg- particulate"
  ) %>% 
  mutate(perc_pmehg = pMeHg/uMeHg) %>% 
  rename_inlet_sta() %>% 
  select(-c(uMeHg, pMeHg))

# Pull out data for Fremont Weir and CCSB and calculate averages and stdev of 
  # the multiple stations representing these inputs
fre_ccsb_perc_pmehg <- inlet_perc_pmehg %>% 
  filter(str_detect(StationName, "^CCSB|^Fre")) %>% 
  group_by(SampleDate, StationName) %>% 
  summarize(
    avg_perc_pmehg = mean(perc_pmehg),
    sd_perc_pmehg = sd(perc_pmehg)
  ) %>% 
  ungroup() %>% 
  rename(perc_pmehg = avg_perc_pmehg)


# Add back the averages and standard deviations for Fremont Weir and CCSB to 
  # all other data for data table

  # Pivot average percentages wider
  fre_ccsb_perc_pmehg_w <- fre_ccsb_perc_pmehg %>% 
    pivot_wider(
      id_cols = -sd_perc_pmehg,
      names_from = StationName,
      values_from = perc_pmehg
    )
  
  # Pivot std deviations wider- only for Fremont Weir
  fre_stdev_w <- fre_ccsb_perc_pmehg %>% 
    pivot_wider(
      id_cols = -perc_pmehg,
      names_from = StationName,
      values_from = sd_perc_pmehg
    ) %>% 
    select(-CCSB) %>% 
    # remove two sampling events in March without values for all three sampling locations 
    filter(month(SampleDate) != 3) %>% 
    rename(Fremont_sd = Fremont)
  
  # Join dataframes together for data table
  inlet_perc_pmehg_tbl <- inlet_perc_pmehg %>% 
    filter(!str_detect(StationName, "^CCSB|^Fre")) %>% 
    pivot_wider(names_from = StationName, values_from = perc_pmehg) %>% 
    left_join(fre_ccsb_perc_pmehg_w) %>% 
    left_join(fre_stdev_w) %>% 
    # Round all values to two significant figures
    mutate_at(vars(-SampleDate), ~signif(.x, 2)) %>% 
    select(SampleDate, KLRC, CCSB, Putah, Sac_Weir, Fremont, Fremont_sd) %>% 
    arrange(SampleDate)

# Calculate averages, stdev, and CV of all sampling events
  
  # Modify dataframe with average Fremont Weir and CCSB data to bind to all other data in order to
    # calculate overall summary statistics
  fre_ccsb_perc_pmehg_mod <- fre_ccsb_perc_pmehg %>% select(-sd_perc_pmehg)
  
  # Join dataframes together and make calculations
  perc_pmehg_summ <- inlet_perc_pmehg %>% 
    filter(!str_detect(StationName, "^CCSB|^Fre")) %>% 
    bind_rows(fre_ccsb_perc_pmehg_mod) %>% 
    group_by(StationName) %>% 
    summarize(
      avg_perc_pmehg = mean(perc_pmehg),
      sd_perc_pmehg = sd(perc_pmehg)
    ) %>% 
    mutate(
      coef_var = signif(sd_perc_pmehg/avg_perc_pmehg, 2),
      # round averages and std deviations after calculating CV
      avg_perc_pmehg = signif(avg_perc_pmehg, 2),
      sd_perc_pmehg = signif(sd_perc_pmehg, 2)
    ) %>% 
    # Restructure dataframe
    pivot_longer(
      cols = avg_perc_pmehg:coef_var,
      names_to = "summ_stat_type",
      values_to = "summ_stat_value"
    ) %>% 
    pivot_wider(names_from = StationName, values_from = summ_stat_value) %>% 
    select(summ_stat_type, KLRC, CCSB, Putah, Sac_Weir, Fremont)

# Export Tables
inlet_perc_pmehg_tbl %>% write_excel_csv(paste0("table_b-", tbl_num, "_indiv_values.csv"), na = "")
perc_pmehg_summ %>% write_excel_csv(paste0("table_b-", tbl_num, "_summary_values.csv"))

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Table B-12 --------------------------------------------------------------
# Comparison of the percentage increase of Hg and MeHg with several biogoechemical parameters 
  # between the Fremont Weir tributary input and export at Liberty Island

# Define table number for easier updating
tbl_num <- as.character(12)

  
# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Table B-13 --------------------------------------------------------------
# Averages and standard deviations for inlet, outlet, and Below Liberty Island loads of Hg, MeHg, 
  # Organic Carbon and Suspended Solids
# For just the sampling events in 2017

# Define table number for easier updating
tbl_num <- as.character(13)

# Bring in total load data
source("YB_Mass_Balance/Loads/Import_Total_Load_Data.R")

# Calculate averages and standard deviations of the inlet, outlet, and BLI loads
loads_total_summ <- loads_total %>% 
  filter(
    Year == 2017,
    str_detect(Analyte, "OC$|Hg|SS$")
  ) %>% 
  # Apply order for analytes in the table
  conv_fact_analytes() %>% 
  # Rename Below Liberty Island
  mutate(LocType = if_else(str_detect(LocType, "^Bel"), "BLI", LocType)) %>% 
  group_by(LocType, Analyte) %>% 
  summarize(
    sign_digits = min(digits),
    avg_load = signif(mean(total_load), sign_digits),
    sd_load = signif(sd(total_load), sign_digits)
  ) %>% 
  ungroup() %>% 
  select(-sign_digits)

# Pivot the averages by LocType
loads_total_avg <- loads_total_summ %>% 
  pivot_wider(
    id_cols = -sd_load,
    names_from = LocType,
    values_from = avg_load,
    names_prefix = "avg_load_"
  ) 

# Pivot the standard deviations by LocType
loads_total_sd <- loads_total_summ %>% 
  pivot_wider(
    id_cols = -avg_load,
    names_from = LocType,
    values_from = sd_load,
    names_prefix = "sd_load_"
  ) 

# Join averages and standard deviations together for table
loads_total_summ_f <- 
  left_join(loads_total_avg, loads_total_sd) %>% 
  select(
    Analyte,
    ends_with("Inlet"),
    ends_with("Outlet"),
    ends_with("BLI")
  )

# Export Tables
loads_total_summ_f %>% write_excel_csv(paste0("table_b-", tbl_num, ".csv"))

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Table B-14 --------------------------------------------------------------
# Averages and standard deviations for the net loads within the Upper and Liberty Island reaches 
  # and for the entire Yolo Bypass of Hg, MeHg,Organic Carbon and Suspended Solids
# Tables also provides the percent differences of the net loads
# For just the sampling events in 2017

# Define table number for easier updating
tbl_num <- as.character(14)

# Bring in load data
source("YB_Mass_Balance/Loads/Import_Total_Load_Data.R")
source("YB_Mass_Balance/Loads/Import_Net_Load_Data.R")

# Only include total and net load data for 2017 for specific analytes
loads_all <- 
  list(
    total = loads_total,
    net = loads_net
  ) %>% 
  map(~filter(.x, Year == 2017, str_detect(Analyte, "OC$|Hg|SS$")))

# Calculate averages of the inlet loads for all 9 events in 2017
loads_inlet_avg9 <- loads_all$total %>% 
  filter(LocType == "Inlet") %>% 
  group_by(Analyte) %>% 
  summarize(inlet_load9 = mean(total_load))
  
# Calculate averages of the inlet and outlet loads for 8 events in 2017 to be comparable to 
  # net loads for Liberty Island and the entire Bypass
loads_total_avg8 <- loads_all$total %>% 
  filter(
    LocType != "Below Liberty",
    SamplingEvent != "Apr 11-12, 2017"
  ) %>% 
  group_by(Analyte, LocType) %>% 
  summarize(avg_load = mean(total_load)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = LocType, values_from = avg_load) %>% 
  rename(
    inlet_load8 = Inlet,
    outlet_load8 = Outlet 
  )

# Calculate averages and standard deviations of the net loads for each reach
loads_net_summ <- loads_all$net %>% 
  group_by(Reach, Analyte) %>% 
  summarize(
    sign_digits = min(digits),
    avg_net_load = mean(net_load),
    sd_net_load = sd(net_load)
  ) %>% 
  ungroup()

# Calculate percent differences for the net loads
# START HERE

# Prepare net load data to be combined with the total load data
loads_net_val <- loads_net %>% 
  pivot_wider(
    id_cols = -c(LoadUnits, digits),
    names_from = Reach,
    values_from = net_load,
    names_prefix = "val_"
  )

loads_net_dig <- loads_net %>% 
  pivot_wider(
    id_cols = -c(LoadUnits, net_load),
    names_from = Reach,
    values_from = digits,
    names_prefix = "digits_"
  )

# Combine total and net load data and only include data for 2017 for specific analytes
loads_c <- 
  reduce(
    list(
      loads_total_mod,
      loads_net_val,
      loads_net_dig
    ),
    .f = left_join
  ) %>% 
  filter(
    Year == 2017,
    str_detect(Analyte, "OC$|Hg|SS$")
  ) %>% 
  # Apply order for analytes in the data tables
  conv_fact_analytes()


# Table B-15 --------------------------------------------------------------
# Averages and standard deviations of MeHg for all reaches and import and export locations
# For just the 8 comparable sampling events in 2017

# Define table number for easier updating
tbl_num <- as.character(15)

# Bring in load data
source("YB_Mass_Balance/Loads/Import_Total_Load_Data.R")
source("YB_Mass_Balance/Loads/Import_Net_Load_Data.R")



loads_total_val <- loads_total_mod %>% 
  pivot_wider(
    id_cols = -digits,
    names_from = LocType,
    values_from = total_load,
    names_prefix = "val_"
  )

loads_total_dig <- loads_total_mod %>% 
  pivot_wider(
    id_cols = -total_load,
    names_from = LocType,
    values_from = digits,
    names_prefix = "digits_"
  )

# Prepare net load data to be combined with the total load data
loads_net_val <- loads_net %>% 
  pivot_wider(
    id_cols = -c(LoadUnits, digits),
    names_from = Reach,
    values_from = net_load,
    names_prefix = "val_"
  )

loads_net_dig <- loads_net %>% 
  pivot_wider(
    id_cols = -c(LoadUnits, net_load),
    names_from = Reach,
    values_from = digits,
    names_prefix = "digits_"
  )

# Combine total and net load data and only include data for 2017 for specific analytes
loads_c <- 
  reduce(
    list(
      loads_total_val,
      loads_total_dig,
      loads_net_val,
      loads_net_dig
    ),
    .f = left_join
  ) %>% 
  filter(
    Year == 2017,
    str_detect(Analyte, "OC$|Hg|SS$")
  ) %>% 
  # Apply order for analytes in the data tables
  conv_fact_analytes()

# Create three dataframes with load data for each Reach (Entire, Upper, and Liberty)
loads_c_entire <- loads_c %>% 
  select(
    SamplingEvent,
    Analyte,
    ends_with("Inlet"),
    ends_with("BLI"),
    ends_with("Entire")
  )

loads_c_upper <- loads_c %>% 
  select(
    SamplingEvent,
    Analyte,
    ends_with("Inlet"),
    ends_with("Outlet"),
    ends_with("Upper")
  )

loads_c_liberty <- loads_c %>% 
  select(
    SamplingEvent,
    Analyte,
    ends_with("Outlet"),
    ends_with("BLI"),
    ends_with("Liberty")
  )

# Make variable names consistent in the dataframes for each reach in order to summarize later
names(loads_c_entire) <- str_replace_all(names(loads_c_entire), "Inlet", "in")
names(loads_c_entire) <- str_replace_all(names(loads_c_entire), "BLI", "out")
names(loads_c_entire) <- str_replace_all(names(loads_c_entire), "Entire", "net")

names(loads_c_upper) <- str_replace_all(names(loads_c_upper), "Inlet", "in")
names(loads_c_upper) <- str_replace_all(names(loads_c_upper), "Outlet", "out")
names(loads_c_upper) <- str_replace_all(names(loads_c_upper), "Upper", "net")

names(loads_c_liberty) <- str_replace_all(names(loads_c_liberty), "Outlet", "in")
names(loads_c_liberty) <- str_replace_all(names(loads_c_liberty), "BLI", "out")
names(loads_c_liberty) <- str_replace_all(names(loads_c_liberty), "Liberty", "net")

# Combine three dataframes for each reach into a list to run functions on each
loads_list_reach <- 
  list(
    "entire" = loads_c_entire,
    "upper" = loads_c_upper,
    "liberty" = loads_c_liberty
  )

# Create summary tables of load data for each reach
loads_list_summ <- loads_list_reach %>% 
  # Remove April 11-12 sampling event for the Entire Bypass and Liberty reach
  map_at(
    c("entire", "liberty"), 
    ~filter(.x, str_detect(SamplingEvent, "^Apr 11", negate = TRUE))
  ) %>% 
  # Calculate averages and standard deviations
  map(~group_by(.x, Analyte)) %>% 
  map(
    ~summarize(
      .x,
      avg_in = mean(val_in),
      sd_in = sd(val_in),
      signdig_in = min(digits_in),
      avg_out = mean(val_out),
      sd_out = sd(val_out),
      signdig_out = min(digits_out),
      avg_net = mean(val_net),
      sd_net = sd(val_net),
      signdig_net = min(digits_net),
    ) 
  ) %>% 
  # Calculate percent differences 
  map(~mutate(.x, perc_diff = signif(avg_net/avg_in, 2))) %>% 
  # Round loads to proper number of significant figures
  map(~mutate_at(.x, c("avg_in", "sd_in"), ~signif(.x, signdig_in))) %>% 
  map(~mutate_at(.x, c("avg_out", "sd_out"), ~signif(.x, signdig_out))) %>% 
  map(~mutate_at(.x, c("avg_net", "sd_net"), ~signif(.x, signdig_net))) %>% 
  # Clean up data to be exported
  map(~select(.x, !starts_with("sign")))

# Export Tables
loads_list_summ$entire %>% write_excel_csv(paste0("table_b-", tbl_num_entire, ".csv"))
loads_list_summ$upper %>% write_excel_csv(paste0("table_b-", tbl_num_upper, ".csv"))
loads_list_summ$liberty %>% write_excel_csv(paste0("table_b-", tbl_num_lib, ".csv"))

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Table B-16 --------------------------------------------------------------



