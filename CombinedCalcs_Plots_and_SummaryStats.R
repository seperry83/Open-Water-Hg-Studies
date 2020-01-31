# Yolo Bypass Inlet-Outlet Study
# Calculate combined parameter calculations
# Create plots and summarize combined parameters for all sampling events

library(tidyverse)
library(readxl)
library(lubridate)

# Load common functions
source("inlet_outlet_common_functions.R")

# 1. Import Data ----------------------------------------------------------

# Import concentration data
conc_orig <- read_excel("../../../Data/Lab_Final/YB_Inlet-Outlet_Conc_Data.xlsx", sheet = "For R Analysis")  
  
# Clean conc_orig
conc_clean <- conc_orig %>% 
  # Remove samples with QualCode "R"
  filter(is.na(QualCode) | !str_detect(QualCode, "^R")) %>%
  # Clean up date and time formatting- extract date and time from dttm variables
  mutate(
    SampleDate = as_date(SampleDate),
    CollectionTimePST = hms::as_hms(CollectionTimePST)
  ) %>%
  # Create a new variable Conc, which is a numeric version of Result with the MDL and RL for the ND values
  mod_result() %>% 
  # Select only necessary variables
  select(
    StationName,
    SampleDate,
    CollectionTimePST,
    Analyte,
    Conc
  )

# Import calculated particulate concentration data
part_conc_orig <- read_csv("Concentrations/Particulate_Conc.csv") 

# Clean part_conc_orig
part_conc_clean <- part_conc_orig %>% 
  select(-Units)

# Bind all concentration data
all_conc <- bind_rows(conc_clean, part_conc_clean)

# Filter and clean concentration data
all_conc_clean <- all_conc %>% 
  filter(
    !str_detect(StationName, "^YB" ),
    SampleDate !="2014-12-12"
  ) %>% 
  # Create some new variables
  mutate(Year = year(SampleDate)) %>% 
  add_samplingevent() %>% 
  # Shorten StationNames
  add_short_sta_names() %>% 
  # Keep only necessary variables
  select(
    StationName,
    ShortName,
    SampleDate,
    CollectionTimePST,
    Year,
    SamplingEvent,
    Analyte,
    Conc
  )


# 2. Calculate all combined parameters ------------------------------------

comb_param <- all_conc_clean %>% 
  filter(
    str_detect(Analyte, "^MeHg|^THg") | Analyte %in% c("TSS", "DOC", "TOC", "POC", "Aluminum- total"),
    Conc != 0  # Remove any obs with values of zero
  ) %>% 
  pivot_wider(names_from = Analyte, values_from = Conc) %>% 
  rename(
    fMeHg = "MeHg- filtered",
    tMeHg = "MeHg- total",
    pMeHg = "MeHg- particulate",
    fTHg = "THg- filtered",
    tTHg = "THg- total",
    pTHg = "THg- particulate",
    tAl = "Aluminum- total"
  ) %>% 
  mutate(
    THg_Solids = pTHg/TSS*1e3,
    MeHg_Solids = pMeHg/TSS*1e3,
    THg_Kd = log10(THg_Solids/fTHg*1e3),
    MeHg_Kd = log10(MeHg_Solids/fMeHg*1e3),
    Per_tMeHg_of_tTHg = tMeHg/tTHg*100,
    Per_fMeHg_of_fTHg = fMeHg/fTHg*100,
    Per_pMeHg_of_pTHg = pMeHg/pTHg*100,
    tTHg_norm_TOC = tTHg/TOC,
    fTHg_norm_DOC = fTHg/DOC,
    pTHg_norm_POC = pTHg/POC,
    tMeHg_norm_TOC = tMeHg/TOC,
    fMeHg_norm_DOC = fMeHg/DOC,
    pMeHg_norm_POC = pMeHg/POC,
    TOC_on_Solids = TOC/TSS*1e3,
    POC_on_Solids = POC/TSS*1e3,
    tAl_Solids = tAl/TSS*1e3
  ) %>% 
  select(-c(DOC:pTHg)) %>% 
  pivot_longer(
    cols = THg_Solids:tAl_Solids,
    names_to = "Parameter",
    values_to = "Value"
  ) %>% 
  mutate(Value = signif(Value, 3)) %>% 
  filter(!is.na(Value))

# Rename combined parameters and add a units variable
cpc_names <- sort(unique(comb_param$Parameter))

cpc_key <- tibble(
  Parameter = cpc_names,
  Parameter2 = c(
    "Filtered MeHg normalized by DOC",
    "Filtered THg normalized by DOC",
    "MeHg Partitioning Coefficient (Kd)",
    "MeHg Concentration on Solids",
    "Percent fMeHg Conc of the fTHg Conc",
    "Percent pMeHg Conc of the pTHg Conc",
    "Percent tMeHg Conc of the tTHg Conc",
    "Particulate MeHg normalized by POC",
    "POC Concentration on Solids",
    "Particulate THg normalized by POC",
    "Total Aluminum Conc on Solids",
    "THg Partitioning Coefficient (Kd)",
    "THg Concentration on Solids",
    "Total MeHg normalized by TOC",
    "TOC Concentration on Solids",
    "Total THg normalized by TOC"
  ),
  Parameter_Short = c(
    "fMeHg/DOC",
    "fTHg/DOC",
    "MeHg Kd",
    "MeHg Conc on Solids",
    "% fMeHg/fTHg",
    "% pMeHg/pTHg",
    "% tMeHg/tTHg",
    "pMeHg/POC",
    "POC Conc on Solids",
    "pTHg/POC",
    "tAl Conc on Solids",
    "THg Kd",
    "THg Conc on Solids",
    "tMeHg/TOC",
    "TOC Conc on Solids",
    "tTHg/TOC"
  ),
  Units = c(
    "ng/mg C",
    "ng/mg C",
    "log L/kg",
    "ng/g",
    "Percent",
    "Percent",
    "Percent",
    "ng/mg C",
    "mg C/g",
    "ng/mg C",
    "mg/g",
    "log L/kg",
    "ng/g",
    "ng/mg C",
    "mg C/g",
    "ng/mg C"
  )
)

comb_param_clean <- comb_param %>% 
  left_join(cpc_key, by = "Parameter") %>% 
  select(
    StationName:SamplingEvent,
    Parameter2,
    Parameter_Short,
    Value,
    Units
  ) %>% 
  rename(Parameter = Parameter2)

# Clean up
rm(cpc_names, cpc_key)


# 3. Create Plots ---------------------------------------------------------

# Setup plotting order
params_long <- sort(unique(comb_param_clean$Parameter))
params_long_order <-  params_long[c(11,3,12,4,9,7,8,16,2,6,15,1,5,13,10,14)]

params_short <- sort(unique(comb_param_clean$Parameter_Short))
params_short_order <-  params_short[c(12,6,13,7,3,1,2,16,5,10,14,4,8,15,9,11)]

comb_param_clean <- comb_param_clean %>% 
  conv_fact_samplingevent() %>% 
  conv_fact_long_sta_names() %>% 
  conv_fact_short_sta_names() %>% 
  mutate(
    Parameter = factor(Parameter, levels = params_long_order),
    Parameter_Short = factor(Parameter_Short, levels = params_short_order)
  )

# Create a df of just the 2017 concentration data
comb_param_clean_17 <- comb_param_clean %>% 
  filter(Year == 2017) %>% 
  mutate(SamplingEvent = fct_drop(SamplingEvent))

# Clean up
rm(params_long, params_long_order, params_short, params_short_order)


# 3.1 Plot all combined parameters ----------------------------------------

# Grouped by station
pdf(file = "Concentrations/CombinedParameter_Plots_byStation.pdf", w=15, h=8.5)
  # All sampling events
  comb_param_clean %>% 
    group_by(StationName) %>% 
    do(plot = {
      print(.$StationName[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = SamplingEvent, 
            y = Value
          )
        ) +
        geom_col() +
        scale_x_discrete(drop = FALSE) +
        facet_wrap(
          vars(Parameter_Short, Units),
          scales = "free_y"
        ) +
        labs(
          title = paste0("Combined Parameters at ", .$StationName[1]),
          subtitle = "All Sampling Events",
          x = "Sampling Event",
          y = NULL
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
    })
  
  # Just 2017 sampling events
  comb_param_clean_17 %>% 
    group_by(StationName) %>% 
    do(plot = {
      print(.$StationName[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = SamplingEvent, 
            y = Value
          )
        ) +
        geom_line(aes(group = 1)) +
        geom_point() +
        scale_x_discrete(drop = FALSE) +
        facet_wrap(
          vars(Parameter_Short, Units),
          scales = "free_y"
        ) +
        labs(
          title = paste0("Combined Parameters at ", .$StationName[1]),
          subtitle = "Just 2017 Sampling Events",
          x = "Sampling Event",
          y = NULL
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
      
    })
  
dev.off()

# Grouped by Parameter
pdf(file = "Concentrations/CombinedParameter_Plots_byParameter.pdf", w=15, h=8.5)
  # All sampling events
  comb_param_clean %>% 
    group_by(Parameter) %>% 
    do(plot = {
      print(.$Parameter[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = SamplingEvent, 
            y = Value 
          )
        ) +
        geom_col() +
        facet_wrap(vars(ShortName)) +
        labs(
          title = .$Parameter[1],
          subtitle = "All Sampling Events",
          x = "Sampling Event",
          y = .$Units[1]
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
    })
  
  # Just 2017 sampling events
  comb_param_clean_17 %>% 
    group_by(Parameter) %>% 
    do(plot = {
      print(.$Parameter[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = SamplingEvent, 
            y = Value
          )
        ) +
        geom_line(aes(group = 1)) +
        geom_point() +
        facet_wrap(vars(ShortName)) +
        labs(
          title = .$Parameter[1],
          subtitle = "Just 2017 Sampling Events",
          x = "Sampling Event",
          y = .$Units[1]
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
      
    })
  
dev.off()
    
# Boxplots
pdf(file = "Concentrations/CombinedParameter_Boxplots.pdf", w=15, h=8.5)
  comb_param_clean %>% 
    mutate(Year = as.character(Year)) %>% 
    group_by(Parameter) %>% 
    do(plot = {
      print(.$Parameter[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = ShortName, 
            y = Value
          )
        ) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(
          width = 0.25,
          aes(color = Year)
        ) +
        labs(
          title = paste0("Boxplots of ", .$Parameter[1]),
          subtitle = "All Sampling Events",
          x = "Station",
          y = .$Units[1]
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
      
    })

dev.off()


# 3.2 Plot the Combined parameters for the Toe Drain Transect -------------

comb_param_clean_tdt <- comb_param_clean %>% 
  filter(
    StationName %in% c(
      "Toe Drain at County Road 22",
      "Toe Drain at Interstate 80",
      "Toe Drain at Lisbon Weir",
      "Toe Drain at 1/2 Lisbon",
      "Prospect Slough"      
    )
  )

# Create plots
pdf(file = "Concentrations/CombinedParam_ToeDrTransect_Plots.pdf", w=11, h=8.5)  
  # Facet by sampling event, grouped by combined parameter
  comb_param_clean_tdt %>% 
    group_by(Parameter) %>% 
    do(plot = {
      print(.$Parameter[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = ShortName, 
            y = Value
          )
        ) +
        geom_line(aes(group = 1)) +
        geom_point() +
        facet_wrap(vars(SamplingEvent)) +
        labs(
          title = "Toe Drain Transect",
          subtitle = .$Parameter[1],
          x = "Station",
          y = .$Units[1]
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
      
    })
  
  # Facet by combined parameter, grouped by sampling event
  comb_param_clean_tdt %>% 
    group_by(SamplingEvent) %>% 
    do(plot = {
      print(.$SamplingEvent[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = ShortName, 
            y = Value
          )
        ) +
        geom_line(aes(group = 1)) +
        geom_point() +
        facet_wrap(
          vars(Parameter_Short, Units),
          scales = "free_y"
        ) +
        labs(
          title = "Toe Drain Transect",
          subtitle = .$SamplingEvent[1],
          x = "Station",
          y = NULL
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
      
    })
  
dev.off()

# 4. Calculate Summary Statistics -----------------------------------------

# Bring in Summary Stats script
source("../../General_R_Code/Summary_Stats_1or2_Groups.R")

# Summarize combined parameters by Station
comb_param_clean_list <- 
  list(
    AllEvents = comb_param_clean,
    Just2017 = comb_param_clean_17
  )

comb_param_clean_summ <- map(comb_param_clean_list, ~SummStat(.x, Value, Parameter, StationName))

# Add Units to the Summary Statistics
unit_key <- comb_param_clean %>% 
  count(Parameter, Units) %>% 
  select(-n)

comb_param_clean_summ <- comb_param_clean_summ %>% 
  map(~left_join(.x, unit_key))

# Export Summary Statistics
comb_param_clean_summ$AllEvents %>% write_excel_csv("CPC_SummaryStats_all.csv")
comb_param_clean_summ$Just2017 %>% write_excel_csv("CPC_SummaryStats_2017.csv")

# All summary stats for the combined parameters are stored in the following spreadsheet:
  # M:\YB_Inlet-Outlet_Study\Data_Analysis\Final_Report\Conc_Data_Analysis.xlsx

# Export Combined Parameters
comb_param_clean %>% 
  select(
    StationName,
    SampleDate,
    CollectionTimePST,
    Year,
    SamplingEvent,
    Parameter,
    Value,
    Units
  ) %>% 
  write_excel_csv("Concentrations/CombinedParameters.csv")

comb_param_clean %>% 
  select(
    ShortName,
    SamplingEvent,
    Parameter_Short,
    Value
  ) %>% 
  pivot_wider(names_from = ShortName, values_from = Value) %>% 
  write_excel_csv("CombinedParameters_wide.csv", na = "")

# The raw data for the Combined parameters is stored in the following spreadsheet:
  # M:\Data_Analysis\Open_Water_R\YB_Inlet_Outlet\Concentrations\CombinedParameters.csv

