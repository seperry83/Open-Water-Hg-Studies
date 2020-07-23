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
  df1 <- df %>% 
    mutate(
      StationName = case_when(
        str_detect(StationName, "^CCSB") ~ "CCSB",
        str_detect(StationName, "^Fre") ~ "Fremont",
        str_detect(StationName, "^Kni") ~ "KLRC",
        str_detect(StationName, "^Put") ~ "Putah",
        str_detect(StationName, "^Sac") ~ "Sac_Weir"
      )
    ) 
  
  return(df1)
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
  
  df1 <- df %>% mutate(Analyte = factor(Analyte, levels = analytes_order))
  
  return(df1)
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
fre_ccsb_mehg_summ <- inlet_mehg_conc %>% 
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
  fre_ccsb_mehg_summ_r <- fre_ccsb_mehg_summ %>% 
    mutate(
      Conc = signif(Conc, digits),
      sd_conc = case_when(
        digits == 2 & Conc >= 0.1 ~ round(sd_conc, 2),
        digits == 2 & Conc < 0.1 ~ round(sd_conc, 3),
        TRUE ~ round(sd_conc, 3)
      )
    ) %>% 
    select(-digits)
  
  # Pivot concentrations and standard deviations wider
  fre_ccsb_mehg_summ_w <- fre_ccsb_mehg_summ_r %>% 
    pivot_wider(names_from = StationName, values_from = c(Conc, sd_conc)) %>% 
    # remove standard deviation for CCSB
    select(-sd_conc_CCSB) %>% 
    # rename some variables
    rename(
      CCSB = Conc_CCSB,
      Fremont = Conc_Fremont,
      Fremont_sd = sd_conc_Fremont
    ) %>% 
    # remove std deviations for two sampling events in March without values for 
    # all three sampling locations at Fremont Weir
    mutate(Fremont_sd = if_else(month(SampleDate) == 3, NA_real_, Fremont_sd))
  
  # Join dataframes together for data table
  inlet_mehg_conc_tbl <- inlet_mehg_conc %>% 
    filter(!str_detect(StationName, "^CCSB|^Fre")) %>% 
    select(StationName, SampleDate, Analyte, Conc) %>% 
    pivot_wider(names_from = StationName, values_from = Conc) %>% 
    left_join(fre_ccsb_mehg_summ_w) %>% 
    select(SampleDate, Analyte, KLRC, CCSB, Putah, Sac_Weir, Fremont, Fremont_sd) %>% 
    arrange(Analyte, SampleDate)

  
# Calculate averages, stdev, and CV of all sampling events
  
  # Modify dataframe with average Fremont Weir and CCSB data to bind to all other data in order to
   # calculate overall summary statistics
  fre_ccsb_mehg_summ_mod <- fre_ccsb_mehg_summ %>% select(-sd_conc)
  
  # Join dataframes together and make calculations
  mehg_conc_summ <- inlet_mehg_conc %>% 
    filter(!str_detect(StationName, "^CCSB|^Fre")) %>% 
    bind_rows(fre_ccsb_mehg_summ_mod) %>% 
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

  # Pivot average percentages and standard deviations wider
  fre_ccsb_perc_pmehg_w <- fre_ccsb_perc_pmehg %>% 
    pivot_wider(names_from = StationName, values_from = c(perc_pmehg, sd_perc_pmehg)) %>% 
    # remove standard deviation for CCSB
    select(-sd_perc_pmehg_CCSB) %>% 
    # rename some variables
    rename(
      CCSB = perc_pmehg_CCSB,
      Fremont = perc_pmehg_Fremont,
      Fremont_sd = sd_perc_pmehg_Fremont
    ) %>% 
    # remove std deviations for two sampling events in March without values for 
    # all three sampling locations at Fremont Weir
    mutate(Fremont_sd = if_else(month(SampleDate) == 3, NA_real_, Fremont_sd))
  
  # Join dataframes together for data table
  inlet_perc_pmehg_tbl <- inlet_perc_pmehg %>% 
    filter(!str_detect(StationName, "^CCSB|^Fre")) %>% 
    pivot_wider(names_from = StationName, values_from = perc_pmehg) %>% 
    left_join(fre_ccsb_perc_pmehg_w) %>% 
    # Round all values to two significant figures
    mutate_if(is.numeric, signif, digits = 2) %>% 
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

# Bring in concentration data
source("YB_Mass_Balance/Concentrations/Import_Conc_Data.R")

# Only include necessary analytes for the concentration data
all_conc_mod <- all_conc %>% filter(str_detect(Analyte, "^Chl|Hg|^TSS|[^P]OC$"))

# Modify Specific Conductance data to bind with concentration and derived parameter data
spcond <- field_data %>% 
  mutate(Analyte = "SpCond") %>% 
  rename(Conc = SpCond) %>% 
  select(StationName, SampleDate, Analyte, Conc)
  
# Modify derived parameter data to combine with concentration and SpCond data
comb_param_calc_mod <- comb_param_calc %>% 
  rename(
    Analyte = Parameter,
    Conc = Value
  ) %>% 
  filter(str_detect(Analyte, "^MeHg C|^TOC")) %>% 
  select(StationName, SampleDate, Analyte, Conc, Units)

# Combine Specific Conductance, concentration, and derived parameters together
all_val <- bind_rows(all_conc_mod, spcond, comb_param_calc_mod)

# Modify all_val_clean dataframe
all_val_clean <- all_val %>% 
  # only keep data from 2017 and a subset of stations
  filter(
    str_detect(StationName, "^Fre|^Lib|Toe.+[n]$"),
    year(SampleDate) == 2017
  ) %>% 
  mutate(
    # Convert TOC/TSS to a proportion
    Conc = if_else(str_detect(Analyte, "TOC C"), Conc/1000, Conc),
    # Add variable for Fremont and export stations
    Loc = if_else(str_detect(StationName, "^Fre"), "Fremont", "Export")
  ) %>% 
  # Add SamplingEvent variable
  add_samplingevent() %>% 
  select(SamplingEvent, StationName, Loc, Analyte, Conc)

# Average all Analytes by Loc variable for each sampling event
all_val_avg <- all_val_clean %>% 
  group_by(SamplingEvent, Loc, Analyte) %>% 
  summarize(avg_val = mean(Conc)) %>% 
  ungroup()

# Pull out Specific Conductance values and find sampling events when the export values
  # were within 15% of the values observed at the Fremont Weir
similar_spcond <- all_val_avg %>% 
  filter(Analyte == "SpCond") %>% 
  pivot_wider(
    id_cols = -Analyte,
    names_from = Loc,
    values_from = avg_val
  ) %>% 
  mutate(perc_diff = Export/Fremont) %>% 
  filter(perc_diff >= 0.85 & perc_diff <= 1.15)

# Calculate overall averages for the Fremont and export locations of each analyte 
  # for the six sampling events with similar Specific Conductance values
similar_val_summ <- similar_spcond %>% 
  select(SamplingEvent) %>% 
  inner_join(all_val_avg) %>% 
  group_by(Loc, Analyte) %>% 
  summarize(value = mean(avg_val)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Loc, values_from = value)

# Calculate the percent increase in the Export concentration and restructure table for report
similar_val_summ_f <- similar_val_summ %>% 
  mutate(
    perc_incr = (Export-Fremont)/Fremont,
    perc_incr = if_else(abs(perc_incr) < 0.1, round(perc_incr, 3), round(perc_incr, 2))
  ) %>% 
  # Round average concentration values
  mutate_at(
    c("Export", "Fremont"),
    ~case_when(
      str_detect(Analyte, "^MeHg-|^TOC C") ~ round(.x, 3),
      str_detect(Analyte, "^MeHg C") ~ round(.x, 2),
      str_detect(Analyte, "^SpC|^TSS") ~ round(.x),
      TRUE ~ round(.x, 1)
    )
  ) %>% 
  # Reorder rows and columns
  mutate(
    Analyte = factor(
      Analyte,
      levels = c(
        "SpCond",
        "Chloride- filtered",
        "THg- total",
        "THg- filtered",
        "THg- particulate",
        "MeHg- total",
        "MeHg- filtered",
        "MeHg- particulate",
        "TOC",
        "DOC",
        "TSS",
        "MeHg Concentration on Solids",
        "TOC Concentration on Solids"
      )
    )
  ) %>% 
  arrange(Analyte) %>% 
  select(Analyte, Fremont, Export, perc_incr)

# Export Table
similar_val_summ_f %>% write_excel_csv(paste0("table_b-", tbl_num, ".csv"))
  
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

# Restructure load summary data for table
loads_total_summ_f <- loads_total_summ %>% 
  pivot_wider(names_from = LocType, values_from = c(avg_load, sd_load)) %>% 
  select(
    Analyte,
    ends_with("Inlet"),
    ends_with("Outlet"),
    ends_with("BLI")
  )

# Export Table
loads_total_summ_f %>% write_excel_csv(paste0("table_b-", tbl_num, ".csv"))

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Table B-14 --------------------------------------------------------------
# Averages and standard deviations for each inlet's load of Hg, MeHg, 
  # Organic Carbon and Suspended Solids
# For just the sampling events in 2017

# Define table number for easier updating
tbl_num <- as.character(14)

# Bring in inlet load data
source("YB_Mass_Balance/Loads/Import_Inlet_Load_Data.R")

# Clean and Prepare Inlet Load data
loads_inlet_clean <- loads_inlet %>% 
  filter(
    Year == 2017,
    str_detect(Analyte, "OC$|Hg|SS$")
  ) %>% 
  # Apply order for analytes in the table
  conv_fact_analytes() %>% 
  # Rename a few stations
  mutate(
    StationName = case_when(
      str_detect(StationName, "^Fre") ~ "Fremont",
      str_detect(StationName, "^Put") ~ "Putah",
      str_detect(StationName, "^Sac") ~ "Sac_Weir",
      TRUE ~ StationName
    )
  ) 

# Calculate averages and standard deviations of loads for each inlet
loads_inlet_summ <- loads_inlet_clean %>% 
  group_by(StationName, Analyte) %>% 
  summarize(
    sign_digits = min(digits, na.rm = TRUE),
    avg_load = signif(mean(Load), sign_digits),
    sd_load = signif(sd(Load), sign_digits)
  ) %>% 
  ungroup() %>% 
  select(-sign_digits)

# Restructure load summary data for table
loads_inlet_summ_f <- loads_inlet_summ %>% 
  pivot_wider(names_from = StationName, values_from = c(avg_load, sd_load)) %>% 
  select(
    Analyte,
    ends_with("KLRC"),
    ends_with("CCSB"),
    ends_with("Putah"),
    ends_with("Sac_Weir"),
    ends_with("Fremont")
  )

# Export Table
loads_inlet_summ_f %>% write_excel_csv(paste0("table_b-", tbl_num, ".csv"))

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])
  

# Table B-15 --------------------------------------------------------------
# Averages and standard deviations for the net loads within the Upper and Liberty Island reaches 
  # and for the entire Yolo Bypass of Hg, MeHg,Organic Carbon and Suspended Solids
# Tables also provides the percent differences of the net loads
# For just the sampling events in 2017

# Define table number for easier updating
tbl_num <- as.character(15)

# Bring in load data
source("YB_Mass_Balance/Loads/Import_Total_Load_Data.R")
source("YB_Mass_Balance/Loads/Import_Net_Load_Data.R")

# Only include total and net load data for 2017 for specific analytes
loads_all <- 
  list(
    total = loads_total,
    net = loads_net
  ) %>% 
  map(~filter(.x, Year == 2017, str_detect(Analyte, "OC$|Hg|SS$"))) %>% 
  map(conv_fact_analytes)

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
loads_perc_diff <- loads_net_summ %>% 
  pivot_wider(
    id_cols = -c(sign_digits, sd_net_load),
    names_from = Reach,
    values_from = avg_net_load
  ) %>% 
  left_join(loads_inlet_avg9) %>% 
  left_join(loads_total_avg8) %>% 
  mutate(
    pd_Entire = Entire/inlet_load8,
    pd_Upper = Upper/inlet_load9,
    pd_Liberty = Liberty/outlet_load8
  ) %>% 
  select(-c(Entire:outlet_load8)) %>% 
  mutate_if(
    is.numeric, 
    ~case_when(
      abs(.x) < 0.01 ~ signif(.x, 1),
      abs(.x) < 1 ~ signif(.x, 2), 
      TRUE ~ signif(.x, 3)
    )
  )

# Prepare net load summary data to be combined with the percent differences
loads_net_summ_r <- loads_net_summ %>% 
  # Round the net load averages and standard deviations to the proper number of significant figures
  mutate_at(vars(ends_with("load")), ~signif(.x, sign_digits)) %>% 
  # Restructure data
  pivot_wider(
    id_cols = -sign_digits,
    names_from = Reach, 
    values_from = c(avg_net_load, sd_net_load)
  )

# Combine net load summary data with the calculated percent differences for table
loads_summ_c <- 
  left_join(loads_net_summ_r, loads_perc_diff) %>% 
  select(
    Analyte,
    ends_with("Upper"),
    ends_with("Liberty"),
    ends_with("Entire")
  )

# Export Table
loads_summ_c %>% write_excel_csv(paste0("table_b-", tbl_num, ".csv"))

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Table B-16 --------------------------------------------------------------
# Averages and standard deviations of MeHg for all reaches and import and export locations
# For just the 8 comparable sampling events in 2017

# Define table number for easier updating
tbl_num <- as.character(16)

# Bring in load data
source("YB_Mass_Balance/Loads/Import_Total_Load_Data.R")
source("YB_Mass_Balance/Loads/Import_Net_Load_Data.R")

# Rename some variables in total and net load dataframes to be able to bind them together
loads_total_mod <- loads_total %>% 
  rename(
    Loc_Reach = LocType,
    load = total_load
  )

loads_net_mod <- loads_net %>% 
  rename(
    Loc_Reach = Reach,
    load = net_load
  )

# Bind data together and only include load data for 8 comparable events in 2017 for MeHg
loads_mehg_all <- 
  bind_rows(loads_total_mod, loads_net_mod) %>% 
  filter(
    Year == 2017, 
    str_detect(Analyte, "^MeHg"), 
    !str_detect(SamplingEvent, "^Apr 11")
  ) %>% 
  # Rename Analytes to make them easier to work with
  mutate(
    Analyte = recode(
      Analyte,
      "MeHg- total" = "uMeHg",
      "MeHg- filtered" = "fMeHg",
      "MeHg- particulate" = "pMeHg"
    )
  )

# Calculate averages and standard deviations of loads for each Loc_Reach
loads_mehg_summ <- loads_mehg_all %>% 
  group_by(Loc_Reach, Analyte) %>% 
  summarize(
    sign_digits = min(digits),
    avg_load = signif(mean(load), sign_digits),
    sd_load = signif(sd(load), sign_digits)
  ) %>% 
  ungroup() %>% 
  # Restructure load summary data for table
  pivot_wider(
    id_cols = -sign_digits,
    names_from = Analyte,
    values_from = c(avg_load, sd_load)
  ) %>% 
  mutate(
    Loc_Reach = factor(
      Loc_Reach, 
      levels = c(
        "Inlet", 
        "Outlet", 
        "Below Liberty",
        "Upper",
        "Liberty",
        "Entire"
      )
    )
  ) %>% 
  arrange(Loc_Reach) %>% 
  select(
    Loc_Reach,
    ends_with("uMeHg"),
    ends_with("fMeHg"),
    ends_with("pMeHg")
  )

# Export Table
loads_mehg_summ %>% write_excel_csv(paste0("table_b-", tbl_num, ".csv"))

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])

