# Yolo Bypass Inlet-Outlet Study

# Calculate Loads for all sampling events
# Compare two different approaches to calculate loads for:
  # Outlet- SCHISM vs. Balanced (Outlet flow = sum of Inlet flows)
  # Below Liberty Island- raw vs. scaled (adjusted using the sum of Inlet flows)

library(tidyverse)
library(readxl)
library(lubridate)

# Load common functions
source("inlet_outlet_common_functions.R")

# 1. Concentration Data ------------------------------------------------------

# 1.1 Import and Clean data -----------------------------------------------
# Import concentration data
ConcData <- read_excel("../../../Data/Lab_Final/YB_Inlet-Outlet_Conc_Data.xlsx", sheet = "For R Analysis") %>%
  # Remove samples with QualCode "R"
  filter(is.na(QualCode) | !str_detect(QualCode, "^R")) %>% 
  # Clean up date formatting- extract date from dttm variable
  mutate(SampleDate = as_date(SampleDate)) %>% 
  # Create a new variable Conc, which is a numeric version of Result with the MDL and RL for the ND values
  mod_result() %>% 
  # Select only necessary variables
  select(
    StationName,
    SampleDate,
    Analyte,
    Conc,
    Units
  )

# Import calculated particulate concentration data
PartData <- read_csv("Concentrations/Particulate_Conc.csv") %>% 
  select(-CollectionTimePST)

# Bind particulate concentration data to all other data
ConcData <- bind_rows(ConcData, PartData)

# Create a vector of all stations to include
Stations <- c(
  "Cache Slough near Ryer Island",
  "CCSB- Low Flow Channel",
  "CCSB Overflow Weir- North",
  "CCSB Overflow Weir- South",
  "Fremont Weir- East Side",
  "Fremont Weir- Middle",
  "Fremont Weir- West Side",
  "Knights Landing Ridge Cut",
  "Liberty Cut below Stairsteps",
  "Miner Slough near Sac River",
  "Putah Creek at Mace Blvd",           
  "Sac River above the Sacramento Weir",
  "Shag Slough below Stairsteps",
  "Toe Drain at 1/2 Lisbon"            
)
  
# Create a vector of all analytes to include
Analytes <- c(
  "Chloride- filtered",
  "DOC",
  "Iron- filtered",
  "Manganese- filtered",
  "MeHg- filtered",
  "MeHg- particulate",
  "MeHg- total",
  "POC",
  "THg- filtered",
  "THg- particulate",
  "THg- total",
  "TOC",
  "TSS",
  "VSS"
)

# Structure ConcData df to be used for load calculations
ConcData <- ConcData %>% 
  # Keep only necessary data
  filter(
    SampleDate != "2014-12-12",  #exclude data from this date
    StationName %in% Stations,
    Analyte %in% Analytes
  ) %>% 
  # Create a standardized variable to identify each unique sampling event, and a Year variable
  mutate(Year = year(SampleDate)) %>%
  add_samplingevent() %>% 
  # Keep only necessary variables
  select(
    SamplingEvent,
    Year,
    StationName,
    Analyte,
    Conc,
    Units
  )

# Clean up
rm(PartData)


# 1.2 Average Concentration data for a few of the Input stations ------------
# CCSB Overflow Weir- North and South
CCSB <- ConcData %>% 
  filter(str_detect(StationName, "Overflow")) %>% 
  # Group and summarize to average the North and South stations
  group_by(SamplingEvent, Year, Analyte, Units) %>% 
  summarize("CCSB- Overflow Weir" = mean(Conc)) %>%
  ungroup() %>% 
  # Pivot df back into long format
  pivot_longer(
    cols = "CCSB- Overflow Weir", 
    names_to = "StationName", 
    values_to = "Conc"
  )

# Fremont Weir
Fremont <- ConcData %>% 
  # Filter out Fremont Weir stations
  filter(str_detect(StationName, "^Fremont Weir")) %>% 
  # Group and summarize to average the Fremont Weir stations
  group_by(SamplingEvent, Year, Analyte, Units) %>% 
  summarize("Fremont Weir" = mean(Conc)) %>%
  ungroup() %>% 
  # Pivot df back into long format
  pivot_longer(
    cols = "Fremont Weir", 
    names_to = "StationName", 
    values_to = "Conc"
  )

# Add back CCSB and Fremont Weir df's to ConcData df
ConcData <- ConcData %>% 
  filter(!str_detect(StationName, "Overflow|^Fremont")) %>% 
  bind_rows(CCSB, Fremont)

# Clean up
rm(CCSB, Fremont)


# 1.3 Estimate some Organic Carbon data -----------------------------------
# The DOC concentration was greater than the TOC concentration at Liberty Cut on 2/16/2017
# Need to estimate these values in order to calculate loads by using the averages of
# the Toe Drain at 1/2 Lisbon and Shag Slough stations
oc_out_feb16 <- ConcData %>% 
  filter(
    StationName %in% c("Toe Drain at 1/2 Lisbon", "Shag Slough below Stairsteps"),
    Analyte %in% c("TOC", "DOC", "POC"),
    SamplingEvent == "Feb 14-15, 2017"
  ) %>% 
  group_by(SamplingEvent, Year, Analyte, Units) %>% 
  summarize(Conc = signif(mean(Conc), 2)) %>% 
  ungroup() %>% 
  mutate(StationName = "Liberty Cut below Stairsteps")

ConcData <- bind_rows(ConcData, oc_out_feb16)

# Clean up
rm(oc_out_feb16)

# 1.4 Create some additional outlet sampling locations ----------------------
# This is necessary to match the flow data from the SCHISM model
# 2016 sampling event
OutSta_2016 <- ConcData %>% 
  # Filter out the outlet stations
  filter(
    Year == 2016,
    StationName %in% c(
      "Liberty Cut below Stairsteps",
      "Shag Slough below Stairsteps",
      "Toe Drain at 1/2 Lisbon"
      )
  ) %>% 
  # Pivot Conc by StationName to stack three stations next to each other
  pivot_wider(names_from = StationName, values_from = Conc) %>% 
  # Rename the StationNames to allow for averaging
  rename(
    Liberty = "Liberty Cut below Stairsteps",
    Shag = "Shag Slough below Stairsteps",
    Toe = "Toe Drain at 1/2 Lisbon"
  ) %>% 
  # Create new variables for Liberty Island Breach locations
  mutate(
    "Liberty Island Breach 1" = Shag,  #this breach is closest to the Shag Slough site
    "Liberty Island Breach 2" = Liberty,  #this breach is closest to the Liberty Cut site
    "Liberty Island Breach 3" = Toe  #this breach is closest to the Toe Drain at 1/2 Lisbon site
  ) %>% 
  # Remove Liberty, Shag, and Toe since they are no longer necessary
  select(-c(Liberty:Toe)) %>% 
  # Pivot df back into long format
  pivot_longer(
    cols = "Liberty Island Breach 1":"Liberty Island Breach 3",
    names_to = "StationName",
    values_to = "Conc"
  )

# 2017 sampling events
OutSta_2017 <- ConcData %>% 
  # Filter out the outlet stations
  filter(
    Year == 2017,
    StationName %in% c(
      "Liberty Cut below Stairsteps",
      "Shag Slough below Stairsteps",
      "Toe Drain at 1/2 Lisbon"
      )
  ) %>% 
  # Pivot Conc by StationName to stack three stations next to each other
  pivot_wider(names_from = StationName, values_from = Conc) %>%
  # Rename the StationNames to allow for averaging
  rename(
    Liberty = "Liberty Cut below Stairsteps",
    Shag = "Shag Slough below Stairsteps",
    Toe = "Toe Drain at 1/2 Lisbon"
  ) %>% 
  # Create a new variable for Little Holland and assign values
  mutate(
    "Little Holland" = if_else(
      SamplingEvent != "Apr 11-12",
      (Toe + Liberty)/2,
      NULL  #only sampled Toe Drain on April 12
    )
  ) %>% 
  # Create a new variable for Main Liberty and assign values
  mutate(
    "Main Liberty" = case_when(
      #Sampling events with good mixing across the Bypass- take the average of Liberty Cut and Shag Slough
      SamplingEvent %in% c("Jan 11-12", "Jan 31-Feb 1", "Feb 14-15", "Mar 1-2", "Mar 15-16") ~ (Liberty + Shag)/2,
      #Sampling events when Shag Slough was much different- use the concentrations from Liberty Cut
      SamplingEvent %in% c("Jan 24-25", "Mar 28-29", "Apr 25-26") ~ Liberty
      #Only sampled Toe Drain on April 12, so not necessary to assign value for this event
    )
  ) %>% 
  # Remove Liberty, Shag, and Toe since they are no longer necessary
  select(-c(Liberty:Toe)) %>% 
  # Pivot df back into long format
  pivot_longer(
    cols = "Little Holland":"Main Liberty",
    names_to = "StationName",
    values_to = "Conc"
  ) %>% 
  # Remove NA values
  filter(!is.na(Conc))
  
# Add OutSta2016 and OutSta2017 df's to ConcData df
ConcData <- 
  bind_rows(ConcData, OutSta_2016, OutSta_2017) %>% 
  # Round Conc data to 3 significant digits
  mutate(Conc = signif(Conc, 3))

# Create vectors to identify Inlet and Outlet stations
inlet.sta <- c(
  "CCSB- Low Flow Channel",
  "CCSB- Overflow Weir",
  "Fremont Weir",
  "Knights Landing Ridge Cut",
  "Putah Creek at Mace Blvd",           
  "Sac River above the Sacramento Weir"
)

outlet.sta <- c(
  "Liberty Cut below Stairsteps",
  "Liberty Island Breach 1",
  "Liberty Island Breach 2",
  "Liberty Island Breach 3",
  "Little Holland",
  "Main Liberty",
  "Shag Slough below Stairsteps",
  "Toe Drain at 1/2 Lisbon"            
)

# Add a new variable LocType to identify inlet, outlet, and Below Liberty stations
ConcData <- ConcData %>% 
  mutate(
    LocType = case_when(
      StationName %in% inlet.sta ~ "Inlet",
      StationName %in% outlet.sta ~ "Outlet",
      TRUE ~ "BelowLiberty"
    )
  )

# Clean up
rm(OutSta_2016, OutSta_2017)


# 2. Flow Data ------------------------------------------------------------
# Import Flow data
FlowData <- read_excel("Flows/DailyAvgFlows_All_and_SE.xlsx", sheet = "Just Sampling Events")

# Sum the CCSB flows for the sampling events where we didn't collect the Low Flow Channel
ccsb.flow <- FlowData %>% 
  # Filter out CCSB stations
  filter(
    str_detect(StationName, "^CCSB") &
    !SamplingEvent %in% c("Mar 28-29", "Apr 11-12", "Apr 25-26")
  ) %>% 
  # Group and summarize to sum the flows of the LFC and Overflow Weir stations
  group_by(SamplingEvent, Year, LocType) %>% 
  summarize("CCSB- Overflow Weir" = sum(Flow)) %>%
  ungroup() %>% 
  # Pivot df back into long format
  pivot_longer(
    cols = "CCSB- Overflow Weir",
    names_to = "StationName",
    values_to = "Flow"
  )

# Add the CCSB flow data back to the FlowData df
FlowData <- FlowData %>% 
  # Remove the CCSB Stations in FlowData df to prevent duplicates
  filter(
    !(
      str_detect(StationName, "^CCSB") &
      !SamplingEvent %in% c("Mar 28-29", "Apr 11-12", "Apr 25-26")
    )
  ) %>%
  # Bind all df back together
  bind_rows(ccsb.flow)

# Clean up
rm(ccsb.flow)

# Sum flows for all outlet stations for April 11-12 sampling event and assign to 1/2 Lisbon station
OutFlow_Apr12 <- FlowData %>% filter(SamplingEvent == "Apr 11-12", LocType == "Outlet")
Flow_Lis_Apr12 <- sum(OutFlow_Apr12$Flow)
OutFlow_Apr12 <- OutFlow_Apr12 %>% filter(StationName == "Toe Drain at 1/2 Lisbon")
OutFlow_Apr12$Flow <- Flow_Lis_Apr12

# Add the April 12 outflow data back to the FlowData df
FlowData <- FlowData %>% 
  # Remove the Outlet stations for the April 11-12 sampling event in FlowData df to prevent duplicates
  filter(!(SamplingEvent == "Apr 11-12" & LocType == "Outlet")) %>%
  # Bind all df back together
  bind_rows(OutFlow_Apr12)

# Clean up
rm(OutFlow_Apr12, Flow_Lis_Apr12)


# 2.1 Create a new Flow df for the balanced flows approach ----------------
# Make a new df that summarizes the total input and output flows for each sampling event
FlowSummary <- FlowData %>%
  filter(LocType != "Below Liberty") %>% 
  group_by(SamplingEvent, Year, LocType) %>% 
  summarize(TotalFlow = sum(Flow)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = LocType, values_from = TotalFlow)

OutFlow <- FlowData %>% 
  # Pull out the flow data for outlet locations
  filter(LocType == "Outlet") %>% 
  # Join summary df
  left_join(FlowSummary) %>% 
  # Create a new variable for the adjusted flow values: (flow at site/total outflow) * total inflow
  mutate(FlowB = round((Flow/Outlet)*Inlet, 1)) %>% 
  # Remove a few variables
  select(-c(Flow, Inlet, Outlet)) %>% 
  # Rename FlowB
  rename(Flow = FlowB)

# Add the adjusted outflow data back to the inflow data
FlowDataBalanced <- FlowData %>% 
  # Remove the flow data for outlet locations to prevent duplicates
  filter(LocType != "Outlet") %>%
  # Bind OutFlow to FlowData
  bind_rows(OutFlow)

# Clean up
rm(OutFlow)


# 3. Calculate Loads ---------------------------------------------------------

# Remove concentration data for a few samples since they won't be used in load calculations
  #I decided to not calculate loads for Cache and Miner Sloughs for the 2016 sampling event since
    #the Below Liberty flows during this flood event were much lower than the sum of the input flows

ConcData <- ConcData %>%
  filter(
    !(
      Year == 2016 &
      StationName %in% c(
        "Cache Slough near Ryer Island",
        "Miner Slough near Sac River"
      )
    )
  )

# Split ConcData into 3 df based on LocType to calculate loads
ConcData.split <- ConcData %>% split(.$LocType)

Loads <- 
  list(
    Inlet = ConcData.split$Inlet,
    Outlet.SCHISM = ConcData.split$Outlet,
    Outlet.Bal = ConcData.split$Outlet,
    BelowLib = ConcData.split$BelowLiberty
  ) %>% 
  map_at(
    c("Inlet", "Outlet.SCHISM", "BelowLib"), 
    ~ left_join(.x, FlowData, by = c("SamplingEvent", "Year", "StationName"))
  ) %>% 
  map_at(
    c("Outlet.Bal"), 
    ~ left_join(.x, FlowDataBalanced, by = c("SamplingEvent", "Year", "StationName"))
  ) %>% 
  bind_rows(.id = "Calc.type") %>% 
  select(-LocType.x) %>% 
  rename(LocType = LocType.y) %>% 
  # Create a new variable to calculate loads
  mutate(
    Load = signif(Conc * Flow * 28.317*60*60*24/1e9, 4),  #The same conversion factor is used for all calculations
    LoadUnits = case_when(
      str_detect(Units, "mg/L") ~ "1,000 kg/day",
      Units == "ug/L"           ~ "kg/day",
      Units == "ng/L"           ~ "g/day"
    )
  )

# Calculate Below Liberty Island loads using subtraction (Cache Sl - Miner Sl)
Loads.bl <- Loads %>% 
  filter(LocType == "Below Liberty") %>% 
  select(-c(Conc, Units, Flow)) %>% 
  pivot_wider(names_from = StationName, values_from = Load) %>% 
  rename(
    Cache = "Cache Slough near Ryer Island",
    Miner = "Miner Slough near Sac River"
  ) %>% 
  mutate(
    Load = signif(Cache - Miner, 4),
    StationName = "Below Liberty Island"
  ) %>% 
  select(-c(Cache, Miner))

# Calculate Below Liberty Island loads using a balanced flows approach (Below Liberty flow = sum of input flows)
  # Make a new df that summarizes the Below Liberty Island flows for each sampling event
  FlowSummary.bl <- FlowData %>% 
    filter(LocType == "Below Liberty") %>% 
    select(-LocType) %>% 
    pivot_wider(names_from = StationName, values_from = Flow) %>% 
    rename(
      Cache = "Cache Slough near Ryer Island",
      Miner = "Miner Slough near Sac River"
    ) %>% 
    mutate(BelowLibertyFlow = Cache - Miner) %>% 
    select(-c(Cache, Miner))
    
  # Calculate loads for Below Liberty Island that are scaled to the sum of the input flows
  Loads.bl.Bal <- FlowSummary %>% 
    select(-Outlet) %>% 
    #Join the Below Liberty Island and Input flows for each sampling event
    right_join(FlowSummary.bl) %>% 
    #Join the Below Liberty Island loads for each sampling event
    right_join(Loads.bl) %>%  
    mutate(ScaledLoad = signif(Load * (Inlet/BelowLibertyFlow), 4)) %>% 
    select(-c(BelowLibertyFlow, Inlet, Load)) %>% 
    rename(Load = ScaledLoad)
  
# Bind df's for each of the load calculation approaches into a list
Loads.list <- Loads %>% split(.$Calc.type) 
Loads.list[["BelowLib"]] <- NULL
Loads.list <- Loads.list %>% 
  append(
    list(
      BelowLib = Loads.bl,
      BelowLib.Bal = Loads.bl.Bal
    )
  )

# Clean up
rm(ConcData.split, FlowSummary.bl, Loads, Loads.bl, Loads.bl.Bal)


# 4. Compare the two load calculation approaches -----------------------------

# 4.1 Outlet Flows --------------------------------------------------------
# using either the SCHISM flows or the Balanced flows approach

# Summarize the outlet flows for the two approaches- just 2017 events
  # SCHISM
  FlowSCHISM.Summ <- FlowSummary %>% 
    filter(Year == 2017) %>%
    # Remove Inlet flows
    select(-Inlet) %>% 
    rename(OutletFlow = Outlet) %>% 
    # create a new variable to identify which approach was used
    mutate(Approach = "SCHISM")
  
  # Balanced
  FlowBalanced.Summ <- FlowDataBalanced %>% 
    filter(
      Year == 2017,
      LocType == "Outlet"
    ) %>%
    group_by(SamplingEvent, Year, LocType) %>% 
    summarize(OutletFlow = sum(Flow)) %>% 
    ungroup() %>% 
    select(-LocType) %>% 
    # create a new variable to identify which approach was used
    mutate(Approach = "Balanced")
  
# Create a summary df by binding the two summary dfs together
FlowSummary <- 
  bind_rows(FlowSCHISM.Summ, FlowBalanced.Summ) %>%
  # Pivot by Approach
  pivot_wider(names_from = Approach, values_from = OutletFlow) %>% 
  # Create new variables for the differences and the % differences
  mutate(
    Difference = Balanced - SCHISM,
    PerDiff = round((Balanced - SCHISM)/SCHISM * 100, 1)
  ) %>% 
  # Pivot back to long format
  pivot_longer(
    cols = SCHISM:Difference,
    names_to = "Approach",
    values_to = "OutletFlow"
  ) %>% 
  # Modify PerDiff variable
  mutate(PerDiff = as.character(paste0(PerDiff, "%"))) %>%  #convert PerDiff to character data type
  mutate(PerDiff = if_else(Approach == "SCHISM", PerDiff, NULL)) %>% 
  # Convert variables in dataframe to apply plot order
  mutate(
    SamplingEvent = factor(
      SamplingEvent,
      levels = c(
        "Jan 11-12",
        "Jan 24-25",
        "Jan 31-Feb 1",
        "Feb 14-15",
        "Mar 1-2",
        "Mar 15-16",
        "Mar 28-29",
        "Apr 11-12",
        "Apr 25-26"
      )
    ),
    Approach = factor(
      Approach,
      levels = c(
        "SCHISM",
        "Balanced",
        "Difference"
      )
    )
  )

# Plot Outlet flow comparison
FlowSummary %>%
  ggplot(
    aes(
      x = SamplingEvent,  #barplot by Sampling Event
      y = OutletFlow,
      fill = Approach,  #make each Approach a different fill color
      label = PerDiff  #label plots with Percent Differences
    )
  ) +   
  geom_col(position = "dodge") + 
  geom_text(position = position_dodge(width = 0), size = 3, vjust = -1) +
  labs(
    title = "Comparison Barplot for Outlet Flows used in two different load calculation approaches",
    subtitle = "Labels are the percent differences between the outlet flows used in the two load calculation approaches",
    caption = "Difference is Balanced - SCHISM; Percent Difference is (Balanced-SCHISM)/SCHISM",
    x = "Sampling Event",
    y = "Daily Average Flow (cfs)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  #x-axis labels at 45 degrees

# Clean up
rm(FlowData, FlowDataBalanced, FlowSCHISM.Summ, FlowBalanced.Summ, FlowSummary)


# 4.2 Outlet Loads --------------------------------------------------------
# using either the SCHISM flows or the Balanced flows approach for the outlet loads

# Summarize Loads for each approach
Loads.Summ <- Loads.list %>% 
  # Only look at 2017
  map(~filter(.x, Year == 2017)) %>% 
  # summarize by SamplingEvent and Analyte, then sum(Load)
  map(~group_by(.x, LocType, SamplingEvent, Analyte, LoadUnits)) %>% 
  map(~summarize(.x, TotalLoad = round(sum(Load), 1))) %>%   #round to the tenth's place
  map(~ungroup(.x)) 

# Create a df used to plot comparisons between the two load calculation approaches
Loads.comp.io <- 
  list(
    SCHISM = bind_rows(Loads.Summ$Inlet, Loads.Summ$Outlet.SCHISM),
    Balanced = bind_rows(Loads.Summ$Inlet, Loads.Summ$Outlet.Bal)
  ) %>% 
  # Pivot by LocType and calculate net loads for each sampling event
  map(~pivot_wider(.x, names_from = LocType, values_from = TotalLoad)) %>% 
  map(~mutate(.x, NetLoad = Outlet - Inlet)) %>% 
  # Pivot back to long format
  map(
    ~pivot_longer(
      .x,
      cols = Inlet:NetLoad,
      names_to = "LoadType",
      values_to = "Load" 
    )
  ) %>% 
  # bind two elements together into a df
  bind_rows(.id = "Approach") %>% 
  # Convert variables in dataframe to apply plot order
  mutate(
    SamplingEvent = factor(
      SamplingEvent,
      levels = c(
        "Jan 11-12",
        "Jan 24-25",
        "Jan 31-Feb 1",
        "Feb 14-15",
        "Mar 1-2",
        "Mar 15-16",
        "Mar 28-29",
        "Apr 11-12",
        "Apr 25-26"
      )
    ),
    LoadType = factor(
      LoadType,
      levels = c(
        "Inlet",
        "Outlet",
        "NetLoad"
      )
    ),
    Approach = factor(
      Approach,
      levels = c(
        "SCHISM",
        "Balanced"
      )
    )
  )

# Create a new df with calcuated RPD's between approaches
LoadsRPD <- Loads.comp.io %>%  
  pivot_wider(names_from = Approach, values_from = Load) %>% 
  mutate(RPD = round(abs(SCHISM - Balanced)/((abs(SCHISM) + abs(Balanced))/2), 3) * 100) %>% 
  mutate(RPD = as.character(paste0(RPD, "%"))) %>%  #convert RPD to character data type
  mutate(RPD = if_else(LoadType != "Inlet", RPD, NULL)) %>% 
  mutate(RPD = if_else(RPD != "NaN%", RPD, NULL)) %>%   #remove one NaN value
  select(-c(LoadUnits, SCHISM, Balanced))

# Join LoadsRPD df to LoadsSummary df to include RPD calculations in the plots
Loads.comp.io <- left_join(Loads.comp.io, LoadsRPD) %>% 
  # Remove the RPD values for the Balanced approach rows to prevent duplicate labels in the plot
  mutate(RPD = if_else(Approach == "SCHISM", RPD, NULL))

# Plot the two approaches together for each analyte- RPD's as labels
pdf(file = 'Loads/2017_Output_Load_Calc_comparison.pdf', w=11, h=8.5)
  # Comparison Barplots for each Analyte separated by LoadType
  Loads.comp.io %>% group_by(Analyte) %>% do(plot={
    print(.$Analyte[1])
    p = ggplot(
      data = .,
      aes(
        x = SamplingEvent,  #barplot by Sampling Event
        y = Load,
        fill = Approach,  #make each Approach a different fill color
        label = RPD  #label plots with RPD's
      )) +   
      geom_col(position = "dodge") + 
      geom_text(position = position_dodge(0)) +
      facet_grid(
        rows = vars(LoadType),  #horizontal facets for each LoadType (Output and NetLoad)
        #Rename facet labels
        labeller = labeller(LoadType = c(   
          "Inlet" = "Input Loads",
          "Outlet" = "Output Loads", 
          "NetLoad" = "Net Loads"
        ))  
      ) +
      labs(
        title = paste0("Comparison Barplots for ", .$Analyte[1], " Loads"),
        subtitle = "Labels are RPD's between the two load calculation approaches",
        x = "Sampling Event",
        y = paste0("Load (", .$LoadUnits[1], ")")
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  #x-axis labels at 45 degrees
    print(p)
  })
dev.off()

# Plot the two approaches together for each analyte- loads as labels
pdf(file = 'Loads/2017_Output_Load_Calc_comparison2.pdf', w=11, h=8.5)
  # Comparison Barplots for each Analyte separated by LoadType
  Loads.comp.io %>% group_by(Analyte) %>% do(plot={
    print(.$Analyte[1])
    p = ggplot(
      data = .,
      aes(
        x = SamplingEvent,  #barplot by Sampling Event
        y = Load,
        fill = Approach,  #make each Approach a different fill color
        label = round(Load, 0)  #label plots with Load values
      )) +   
      geom_col(position = "dodge") + 
      geom_text(position = position_dodge(width = 0.9), size = 3) +
      facet_grid(
        rows = vars(LoadType),  #horizontal facets for each LoadType (Output and NetLoad)
        #Rename facet labels
        labeller = labeller(
          LoadType = c(   
            "Inlet" = "Input Loads",
            "Outlet" = "Output Loads", 
            "NetLoad" = "Net Loads"
          )
        )  
      ) +
      labs(
        title = paste0("Comparison Barplots for ", .$Analyte[1], " Loads"),
        subtitle = "Labels are load values for the two calculation approaches",
        x = "Sampling Event",
        y = paste0("Load (", .$LoadUnits[1], ")")
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  #x-axis labels at 45 degrees
    print(p)
  })
dev.off()

# Plot just the net loads for the two approaches and their difference
# Add a few items to the LoadsSummary df for these plots
Loads.comp.io.mod <- Loads.comp.io %>% 
  # Just keep the Net Load Calculations
  filter(LoadType == "NetLoad") %>% 
  # Remove a couple of unnecessary variables
  select(-c(LoadType, RPD)) %>% 
  # Pivot by Approach in order to calculate differences between them
  pivot_wider(names_from = Approach, values_from = Load) %>% 
  # Create a new variable for the differences in the net flows
  mutate(Difference = Balanced - SCHISM) %>% 
  # Pivot to put the df back to long format
  pivot_longer(
    cols = SCHISM:Difference,
    names_to = "Approach",
    values_to = "Load"
  ) %>% 
  mutate(
    Approach = factor(
      Approach,
      levels = c(
        "SCHISM",
        "Balanced",
        "Difference"
      )
    )
  ) %>% 
  # Create Analyte groups for more efficient plots
  mutate(
    AnalyteGroup = case_when(
      str_detect(Analyte, "^THg")                 ~ "THg",
      str_detect(Analyte, "^MeHg")                ~ "MeHg",
      Analyte %in% c("TOC", "DOC", "POC")         ~ "Organic Carbon",
      Analyte %in% c("VSS", "Chloride- filtered") ~ "VSS and Chloride",
      Analyte == "TSS"                            ~ "TSS",
      Analyte == "Iron- filtered"                 ~ "Filtered Fe",
      Analyte == "Manganese- filtered"            ~ "Filtered Mn"
    )
  )

# Create Load plots
pdf(file = "Loads/2017_Net_Loads_comparison.pdf", w = 11, h = 8.5)
  # Comparison Barplots of the Net Loads for each AnalyteGroup 
  Loads.comp.io.mod %>% group_by(AnalyteGroup) %>% do(plot={
    print(.$AnalyteGroup[1])
    p = ggplot(
      data = .,
      aes(
        x = SamplingEvent,  #barplot by Sampling Event
        y = Load,
        fill = Approach,  #make each Approach a different fill color
        label = round(Load, 0)  #label plots with Load values
      )) +   
      geom_col(position = "dodge") + 
      geom_text(position = position_dodge(width = 0.9), size = 2.5) +
      facet_grid(rows = vars(Analyte)) +  #horizontal facets for each Analyte within group
      labs(
        title = paste0("Comparison Barplots of the Net Loads for ", .$AnalyteGroup[1]),
        subtitle = "Labels are net load values and their differences for the two calculation approaches",
        caption = "Difference is Balanced - SCHISM",
        x = "Sampling Event",
        y = paste0("Net Load (", .$LoadUnits[1], ")")
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  #x-axis labels at 45 degrees
    print(p)
  })
dev.off()

# Clean up
rm(Loads.comp.io, Loads.comp.io.mod, LoadsRPD)

# *****************
# Outlet loads:
  # Since the overall water balance between the sum of the input flows and the SCHISM flows for the 2017 flood
  # from Jan-May was close to balanced, the technical team decided that the Balanced approach is the best way
  # to calculate outlet loads for the 2017 sampling event
  # The water balances for the 2014 and 2016 floods were incomplete and unbalanced, but I decided to use
  # the Balanced approach to calculate outlet loads for these events as well to be consistent
# *****************


# 4.3 Below Liberty Island Loads ----------------------------------------------
# using raw vs. scaled (adjusted using the sum of Inlet flows) loads for Below Liberty Island
  
# Calculate net loads (Below Liberty - Outlet) for both load calculation approaches
Loads.comp.bl <-
  list(
    Raw = bind_rows(Loads.Summ$Outlet.Bal, Loads.Summ$BelowLib),
    Balanced = bind_rows(Loads.Summ$Outlet.Bal, Loads.Summ$BelowLib.Bal)
  ) %>% 
  # Remove one event when we didn't collect samples at Cache and Miner Sloughs
  map(~filter(.x, SamplingEvent != "Apr 11-12")) %>% 
  # Pivot by LocType and calculate net loads for each sampling event
  map(~pivot_wider(.x, names_from = LocType, values_from = TotalLoad)) %>% 
  map(~rename(.x, BelowLiberty = "Below Liberty")) %>% 
  map(~mutate(.x, NetLoad = BelowLiberty - Outlet)) %>% 
  # bind two elements together into a df
  bind_rows(.id = "Approach") %>% 
  # Convert variables in dataframe to apply plot order
  mutate(
    SamplingEvent = factor(
      SamplingEvent,
      levels = c(
        "Jan 11-12",
        "Jan 24-25",
        "Jan 31-Feb 1",
        "Feb 14-15",
        "Mar 1-2",
        "Mar 15-16",
        "Mar 28-29",
        "Apr 25-26"
      )
    )
  ) %>% 
  filter(!is.na(NetLoad))

# Plot the net loads for the two approaches
pdf(file = "Loads/BelowLiberty_Net_Loads_comparison.pdf", w = 11, h = 8.5)
  Loads.comp.bl %>% group_by(Analyte) %>% do(plot={
    print(.$Analyte[1])
    p = ggplot(
      data = .,
      aes(
        x = SamplingEvent,  #barplot by Sampling Event
        y = NetLoad,
        fill = Approach  #make each Approach a different fill color
      )) +   
      geom_col(position = "dodge") + 
      labs(
        title = paste0("Comparison Barplots of the Net Loads for ", .$Analyte[1]),
        subtitle = "Net Load = Below Liberty - Yolo Bypass Output",
        x = "Sampling Event",
        y = paste0("Net Load (", .$LoadUnits[1], ")")
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  #x-axis labels at 45 degrees
    print(p)
  })
dev.off()

# Clean up
rm(Loads.comp.bl)

# *****************
# Below Liberty Island loads:
  # Since the overall water balance between the sum of the input flows and the Below Liberty Island flows 
  # for the 2017 flood from Jan-May was very close to balanced, I decided that the Balanced or scaled approach 
  # is the best way to calculate the Below Liberty loads
# *****************


# 5. Export calculated loads -------------------------------------------------

# Keep the Outlet and Below Liberty loads using the Balanced approach for both
Loads.final <- 
  bind_rows(Loads.list$Inlet, Loads.list$Outlet.Bal, Loads.list$BelowLib.Bal) %>% 
  select(
    SamplingEvent,
    Year,
    StationName,
    Analyte,
    Conc,
    Units,
    Flow,
    LocType,
    Load,
    LoadUnits
  ) %>% 
  rename(ConcUnits = Units) %>% 
  mutate(SamplingEvent = paste0(SamplingEvent, ", ", Year)) %>% 
  mutate(
    SamplingEvent = factor(
      SamplingEvent,
      levels = c(
        "Dec 22-23, 2014",
        "Mar 15-16, 2016",
        "Jan 11-12, 2017",
        "Jan 24-25, 2017",
        "Jan 31-Feb 1, 2017",
        "Feb 14-15, 2017",
        "Mar 1-2, 2017",
        "Mar 15-16, 2017",
        "Mar 28-29, 2017",
        "Apr 11-12, 2017",
        "Apr 25-26, 2017"
      )
    )
  ) %>% 
  arrange(SamplingEvent, LocType, StationName, Analyte)

# Export Loads to be stored in a spreadsheet
Loads.final %>% write_excel_csv("Loads/All_YB_Loads-R.csv", na = "")
# This .csv will be used to create plots and summary statistics

# Restructure and export Inlet loads to be placed in a summary spreadsheet
Loads.final %>% 
  filter(LocType == "Inlet") %>% 
  select(SamplingEvent, StationName, Analyte, Load) %>% 
  pivot_wider(names_from = StationName, values_from = Load) %>% 
  write_excel_csv("InletLoads.csv", na = "0")

# Restructure and export Outlet and Below Liberty loads to be placed in a summary spreadsheet
Loads.final %>% 
  filter(LocType != "Inlet") %>%
  group_by(SamplingEvent, LocType, Analyte) %>% 
  summarize(TotalLoad = sum(Load)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = LocType, values_from = TotalLoad) %>% 
  write_excel_csv("Outlet_BL_Loads.csv", na = "")

# Summarize loads by LocType and export to be placed in a summary spreadsheet
Loads.final %>% 
  group_by(LocType, SamplingEvent, Analyte) %>% 
  summarize(TotalLoad = sum(Load)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Analyte, values_from = TotalLoad) %>% 
  write_excel_csv("Total_Loads.csv", na = "")

Loads.total <- Loads.final %>% 
  group_by(LocType, SamplingEvent, Year, Analyte, LoadUnits) %>% 
  summarize(TotalLoad = sum(Load)) %>% 
  ungroup()

Loads.total %>% write_excel_csv("Total_Loads2.csv")

# Bring in Summary Stats script
source("../../General_R_Code/Summary_Stats_1or2_Groups.R")

SummStat(Loads.total, TotalLoad, LocType, Analyte) %>% 
  write_excel_csv("Total_Loads_summ.csv")


