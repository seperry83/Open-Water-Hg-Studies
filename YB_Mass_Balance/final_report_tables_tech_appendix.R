# Yolo Bypass Mass Balance Study
# Purpose: Tables for Final Report - Technical Appendix
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(lubridate)
library(openwaterhg)


# Functions for script ----------------------------------------------------

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

obj_keep <- c("obj_keep", "conv_fact_analytes")


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
  mutate(
    StationName = case_when(
      str_detect(StationName, "^CCSB") ~ "CCSB",
      str_detect(StationName, "^Fre") ~ "Fremont Weir",
      str_detect(StationName, "^Kni") ~ "KLRC",
      str_detect(StationName, "^Put") ~ "Putah Creek",
      str_detect(StationName, "^Sac") ~ "Sacramento Weir"
    )
  ) %>% 
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
  # all other data for data tables
  
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
    ) %>% 
    rename(Fremont = "Fremont Weir")
  
  # Pivot std deviations wider
  fre_ccsb_mehg_stdev_w <- fre_ccsb_mehg_conc_r %>% 
    pivot_wider(
      id_cols = -Conc,
      names_from = StationName,
      values_from = sd_conc
    ) %>% 
    # only keep values for Fremont Weir
    select(-CCSB) %>% 
    # remove two sampling events in March without values for all three sampling locations 
    filter(month(SampleDate) != 3) %>% 
    rename(Fremont_sd = "Fremont Weir")
  
  # Join dataframes together for data tables
  inlet_mehg_conc_tbl <- inlet_mehg_conc %>% 
    filter(!str_detect(StationName, "^CCSB|^Fre")) %>% 
    select(StationName, SampleDate, Analyte, Conc) %>% 
    pivot_wider(names_from = StationName, values_from = Conc) %>% 
    rename(
      Putah = "Putah Creek",
      Sac_Weir = "Sacramento Weir"
    ) %>% 
    left_join(fre_ccsb_mehg_conc_w) %>% 
    left_join(fre_ccsb_mehg_stdev_w) %>% 
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
    ungroup() %>% 
    # Restructure dataframe
    select(-sign_digits) %>% 
    pivot_longer(
      cols = avg_conc:coef_var,
      names_to = "summ_stat_type",
      values_to = "summ_stat_value"
    ) %>% 
    conv_fact_inlet_names() %>% 
    arrange(StationName) %>% 
    pivot_wider(names_from = StationName, values_from = summ_stat_value)

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
  # Calculate the number of significant digits in the Conc values
  mutate(
    digits = case_when(
      Conc < 0.01 ~ 1,
      Conc < 0.1 ~ 2,
      TRUE ~ 3
    )
  ) %>% 
  select(StationName, SampleDate, Analyte, Conc, digits)

# Calculate the percent of pMeHg for the tributary inlets in 2017
inlet_perc_pmehg <- inlet_mehg_conc %>% 
  pivot_wider(
    id_cols = -digits,
    names_from = Analyte,
    values_from = Conc
  ) %>% 
  rename(
    uMeHg = "MeHg- total",
    pMeHg = "MeHg- particulate"
  ) %>% 
  mutate(perc_pmehg = pMeHg/uMeHg)

# Calculate the minimum number of significant digits to assign to each percentage value
inlet_mehg_sig_dig <- inlet_mehg_conc %>% 
  group_by(StationName, SampleDate) %>% 
  summarize(sign_digits = min(digits)) %>% 
  ungroup()

# Rename stations
mutate(
  StationName = case_when(
    str_detect(StationName, "^CCSB") ~ "CCSB",
    str_detect(StationName, "^Fre") ~ "Fremont Weir",
    str_detect(StationName, "^Kni") ~ "KLRC",
    str_detect(StationName, "^Put") ~ "Putah Creek",
    str_detect(StationName, "^Sac") ~ "Sacramento Weir"
  )
) %>% 

# Export Table
loads_inlet_summ_c %>% write_excel_csv(paste0("table_b-", tbl_num, ".csv"))

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Table B-12 --------------------------------------------------------------
# Summary statistics for total loads of Hg, MeHg, Organic Carbon and Suspended Solids
# Totals for the outlet at the Stairsteps and Below Liberty Island
# For just the sampling events in 2017

# Define table number for easier updating
tbl_num <- as.character(12)

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
  select(-c(Mean, StDev)) %>% 
  # Round some medians to appropriate number of significant digits
  mutate(
    Median = case_when(
      LocType == "Below Liberty" & Analyte %in% c("MeHg- particulate", "TSS", "DOC") ~ signif(Median, 2),
      LocType == "Below Liberty" & Analyte == "VSS" ~ signif(Median, 1),
      TRUE ~ Median
    )
  )

# Join Summary Statistics together for Table
loads_total_clean_summ <- 
  left_join(loads_total_clean_summ1, loads_total_clean_summ2) %>% 
  select(LocType, Analyte, LoadUnits, Minimum, Maximum, Median, Mean, StDev) %>% 
  # Define analyte order for table
  mutate(Analyte = factor(Analyte, levels = analyte_order)) %>%
  arrange(Analyte) %>% 
  group_nest(LocType)

# Export Table
  # Outlet Loads
  loads_total_clean_summ %>% 
    filter(LocType == "Outlet") %>% 
    pull(data) %>% 
    chuck(1) %>% 
    write_excel_csv(paste0("table_b-", tbl_num, "_outlet.csv"))
  
  # Below Liberty Island Loads
  loads_total_clean_summ %>% 
    filter(LocType == "Below Liberty") %>% 
    pull(data) %>% 
    chuck(1) %>% 
    write_excel_csv(paste0("table_b-", tbl_num, "_bli.csv"))
  
# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Table B-13 --------------------------------------------------------------
# Averages and standard deviations for net loads of Hg, MeHg, Organic Carbon and Suspended Solids
# For 3 segments of the Yolo Bypass: upper reach, Liberty Island, and entire Bypass
# For just the sampling events in 2017

# Define table number for easier updating
tbl_num <- as.character(13)

# Bring in total load data
source("YB_Mass_Balance/Loads/Import_Net_Load_Data.R")

# Filter net load data
loads_net_clean <- loads_net %>% 
  # Filter load data
  filter(
    Year == 2017,
    str_detect(Analyte, "OC$|Hg|SS$"),
  )

# Summarize net load data for each reach for Table B-13
loads_net_clean_summ <- loads_net_clean %>% 
  # Calculate averages and standard deviations of net loads for each reach
  group_by(Reach, Analyte, LoadUnits) %>% 
  summarize(
    sign_digits = min(digits),
    Mean = signif(mean(net_load), sign_digits),
    StDev = signif(sd(net_load), sign_digits)
  ) %>% 
  ungroup() %>% 
  select(-sign_digits)

# Pivot the averages
loads_net_avg <- loads_net_clean_summ %>% 
  pivot_wider(
    id_cols = -StDev,
    names_from = Reach,
    values_from = Mean
  )

# Pivot the standard deviations
loads_net_stdev <- loads_net_clean_summ %>% 
  pivot_wider(
    id_cols = -Mean,
    names_from = Reach,
    values_from = StDev
  ) %>% 
  select(-LoadUnits)

# Join averages and standard deviations together into one df
loads_net_summ_c <- 
  left_join(
    loads_net_avg, loads_net_stdev,
    by = c("Analyte"),
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

# Export Table
loads_net_summ_c %>% write_excel_csv(paste0("table_b-", tbl_num, ".csv"))

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])

