# Yolo Bypass Inlet-Outlet Study
# Analysis of Below Liberty Island flows for the Water Balance for the 2017 Yolo Bypass flood event

library(tidyverse)
library(readxl)
library(scales)
library(lubridate)
library(janitor)

# Cache and Miner Slough Flows --------------------------------------------

# Bring in Filtered Flow data for Cache and Miner Sloughs
CacheSl <- read_excel("../../../Data/Flow/Cache_&_Miner_Sloughs/CacheSl_FilteredFlow_2017.xlsx", sheet = "Filtered Flow") %>% 
  clean_names(case = "upper_camel")

CacheSlDaily <- read_excel("../../../Data/Flow/Cache_&_Miner_Sloughs/CacheSl_FilteredFlow_2017.xlsx", sheet = "Daily Average Flow") %>% 
  clean_names(case = "upper_camel")

MinerSl <- read_excel("../../../Data/Flow/Cache_&_Miner_Sloughs/MinerSl_FilteredFlow_2017.xlsx", sheet = "Filtered Flow") %>% 
  clean_names(case = "upper_camel")

MinerSlDaily <- read_excel("../../../Data/Flow/Cache_&_Miner_Sloughs/MinerSl_FilteredFlow_2017.xlsx", sheet = "Daily Average Flow") %>% 
  clean_names(case = "upper_camel")

# Create plots of the tidally filtered flow data
pdf(file = 'Flows/Cache&MinerSl_Flow_2017.pdf',w=11,h=8.5)
  # Cache Slough
  CacheSl %>% ggplot(aes(DateAndTime, FilteredFlow, color = Interpolated)) +
    geom_line(aes(group = 1)) +
    scale_x_datetime(  #define x-axis as DateTime
      name = 'Date',  #define x-axis label
      date_breaks = '7 day',  #define x-axis tick interval
      date_labels = '%b-%d'  #custom formatting for date labels
    ) + 
    scale_y_continuous(
      name = 'Tidally Filtered Flow (cfs)',
      labels = scales::comma,
      breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)
    ) +
    labs(
      title = 'Cache Slough at Ryer Island USGS Station',
      subtitle = "Tidally Filtered Flow"
      ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  #vertical x-axis labels
  
  CacheSlDaily %>% ggplot(aes(Date, DailyAverageNetFlow, color = Interpolated)) +
    geom_line(aes(group = 1)) +
    scale_x_datetime(  #define x-axis as DateTime
      name = 'Date',  #define x-axis label
      date_breaks = '7 day',  #define x-axis tick interval
      date_labels = '%b-%d'  #custom formatting for date labels
    ) + 
    scale_y_continuous(
      name = 'Daily Average Tidally Filtered Flow (cfs)',
      labels = scales::comma,
      breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)
    ) +
    labs(
      title = 'Cache Slough at Ryer Island USGS Station',
      subtitle = "Tidally Filtered Flow- Daily Averages"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  #vertical x-axis labels
  
  # Miner Slough
  MinerSl %>% ggplot(aes(DateAndTime, FilteredFlow, color = Interpolated)) +
    geom_line(aes(group = 1)) +
    scale_x_datetime(  #define x-axis as DateTime
      name = 'Date',  #define x-axis label
      date_breaks = '7 day',  #define x-axis tick interval
      date_labels = '%b-%d'  #custom formatting for date labels
    ) + 
    scale_y_continuous(
      name = 'Tidally Filtered Flow (cfs)',
      labels = scales::comma
    ) +
    labs(
      title = 'Miner Slough near Sacramento River DWR-NCRO Station',
      subtitle = "Tidally Filtered Flow"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  #vertical x-axis labels
  
  MinerSlDaily %>% ggplot(aes(Date, DailyAverageNetFlow, color = Interpolated)) +
    geom_line(aes(group = 1)) +
    scale_x_datetime(  #define x-axis as DateTime
      name = 'Date',  #define x-axis label
      date_breaks = '7 day',  #define x-axis tick interval
      date_labels = '%b-%d'  #custom formatting for date labels
    ) + 
    scale_y_continuous(
      name = 'Daily Average Tidally Filtered Flow (cfs)',
      labels = scales::comma
    ) +
    labs(
      title = 'Miner Slough near Sacramento River DWR-NCRO Station',
      subtitle = "Tidally Filtered Flow- Daily Averages"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  #vertical x-axis labels

dev.off()


# Comparison with SCHISM Flows --------------------------------------------

# Bring in SCHISM Flow Data
schism <- 
  read_excel(
    path = "Flows/2017_YB_Flood_Flows.xlsx", 
    sheet = "Output Flows - SCHISM", 
    range = "A2:H11233",
    col_names = c(
      "DateTime",
      "Date",
      "Toe Drain at 1/2 Lisbon",
      "Little Holland",
      "Liberty Cut below Stairsteps",
      "Main Liberty",
      "Shag Slough below Stairsteps",
      "TotalOutflow"
    ),
    col_types = c(
      "date",
      "date", 
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    )
  ) %>% 
  select(DateTime, TotalOutflow)

# Create a df with calculated Below Liberty Island flows
CacheSl <- mutate(CacheSl, Location = "CacheSl")
MinerSl <- mutate(MinerSl, Location = "MinerSl")
BelowLI <- bind_rows(CacheSl, MinerSl) %>% 
  select(DateAndTime, FilteredFlow, Location) %>% 
  spread(Location, FilteredFlow) %>% 
  mutate(BelowLiberty = CacheSl - MinerSl) %>% 
  select(-c(CacheSl, MinerSl)) %>% 
  gather(Location, FilteredFlow, BelowLiberty) %>% 
  rename(DateTime = DateAndTime)

# Combine SCHISM and Below Liberty Island flows into one df for plotting
CombFlows <- schism %>% 
  select(DateTime, TotalOutflow) %>% 
  mutate(Location = "SCHISM") %>% 
  rename(FilteredFlow = TotalOutflow) %>% 
  bind_rows(BelowLI)

# Subtract 12 hours from DateTime in the BelowLI df to account for travel time lag
BelowLI_12 <- BelowLI %>% 
  mutate(DateTime = DateTime + hours(-12))

# Combine SCHISM and Below Liberty Island flows lagged by 12 hours into one df for plotting
CombFlows_12 <- schism %>% 
  select(DateTime, TotalOutflow) %>% 
  mutate(Location = "SCHISM") %>% 
  rename(FilteredFlow = TotalOutflow) %>% 
  bind_rows(BelowLI_12)

# Calculate daily averages for both CombFlows and CombFlows_12 df's 
CombFlowsAvg <- CombFlows %>% 
  mutate(Date = date(DateTime)) %>% 
  group_by(Date, Location) %>% 
  summarize(DailyAvgFlow = mean(FilteredFlow)) %>% 
  ungroup()

CombFlows_12Avg <- CombFlows_12 %>% 
  mutate(Date = date(DateTime)) %>% 
  group_by(Date, Location) %>% 
  summarize(DailyAvgFlow = mean(FilteredFlow)) %>% 
  ungroup()

# Bring in Daily averages of the sum of input flows
InputAvg <- read_excel("Flows/DailyAvgFlows_All_and_SE.xlsx", sheet = "All data") %>% 
  mutate(Date = as_date(Date)) %>% 
  filter(
    Date >= "2017-01-01" & Date <= "2017-05-04",
    LocType == "Inlet"
  ) %>% 
  group_by(Date) %>% 
  summarize(DailyAvgFlow = sum(Flow)) %>% 
  ungroup() %>% 
  mutate(Location = "Sum of Inputs")

# Combine input flows with SCHISM and Below Liberty
AllFlows <- CombFlowsAvg %>% 
  # lag SCHISM and Below Liberty daily average flows by one day
  mutate(Date = Date + days(-1)) %>% 
  bind_rows(InputAvg) %>% 
  # Convert variables in dataframe to apply plot order
  mutate(
    Location = factor(
      Location,
      levels = c(
        "Sum of Inputs",
        "SCHISM",
        "BelowLiberty"
      )
    )
  )

# Create plots to compare SCHISM flows with the calculated Below Liberty Island flows
pdf(file = 'Flows/SCHISM_BelowLI_FlowComparison_2017.pdf',w=11,h=8.5)
  # 15-minute data
    # No lag
    CombFlows %>% ggplot(aes(DateTime, FilteredFlow)) +
      geom_line(aes(color = Location)) +
      scale_x_datetime(  #define x-axis as DateTime
        name = 'Date',  #define x-axis label
        date_breaks = '7 day',  #define x-axis tick interval
        date_labels = '%b-%d'  #custom formatting for date labels
      ) + 
      scale_y_continuous(
        name = 'Tidally Filtered Flow (cfs)',
        labels = scales::comma,
        breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)
      ) +
      labs(
        title = 'SCHISM vs. Below Liberty Island Flows- 15 minute data',
        subtitle = "No Lag time included",
        caption = "BelowLiberty = Cache Sl - Miner Sl"
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  #vertical x-axis labels 
  
    # Below Liberty with 12 hour lag
    CombFlows_12 %>% ggplot(aes(DateTime, FilteredFlow)) +
      geom_line(aes(color = Location)) +
      scale_x_datetime(  #define x-axis as DateTime
        name = 'Date',  #define x-axis label
        date_breaks = '7 day',  #define x-axis tick interval
        date_labels = '%b-%d'  #custom formatting for date labels
      ) + 
      scale_y_continuous(
        name = 'Tidally Filtered Flow (cfs)',
        labels = scales::comma,
        breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)
      ) +
      labs(
        title = 'SCHISM vs. Below Liberty Island Flows- 15 minute data',
        subtitle = "Below Liberty is lagged for 12 hours",
        caption = "BelowLiberty = Cache Sl - Miner Sl"
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  #vertical x-axis labels  
  
  # Daily Averages
    # No lag
    CombFlowsAvg %>% ggplot(aes(Date, DailyAvgFlow)) +
      geom_line(aes(color = Location)) +
      scale_x_date(  #define x-axis as DateTime
        name = 'Date',  #define x-axis label
        date_breaks = '7 day',  #define x-axis tick interval
        date_labels = '%b-%d'  #custom formatting for date labels
      ) + 
      scale_y_continuous(
        name = 'Daily Average Tidally Filtered Flow (cfs)',
        labels = scales::comma,
        breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)
      ) +
      labs(
        title = 'SCHISM vs. Below Liberty Island Flows- Daily Averages',
        subtitle = "No Lag time included",
        caption = "BelowLiberty = Cache Sl - Miner Sl"
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  #vertical x-axis labels 
    
    # Below Liberty with 12 hour lag
    CombFlows_12Avg %>% ggplot(aes(Date, DailyAvgFlow)) +
      geom_line(aes(color = Location)) +
      scale_x_date(  #define x-axis as DateTime
        name = 'Date',  #define x-axis label
        date_breaks = '7 day',  #define x-axis tick interval
        date_labels = '%b-%d'  #custom formatting for date labels
      ) + 
      scale_y_continuous(
        name = 'Daily Average Tidally Filtered Flow (cfs)',
        labels = scales::comma,
        breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)
      ) +
      labs(
        title = 'SCHISM vs. Below Liberty Island Flows- Daily Averages',
        subtitle = "Below Liberty is lagged for 12 hours",
        caption = "BelowLiberty = Cache Sl - Miner Sl"
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  #vertical x-axis labels 
  
  # Comparing all flows
  AllFlows %>% ggplot(aes(Date, DailyAvgFlow)) +
    geom_line(aes(color = Location)) +
    scale_x_date(  #define x-axis as DateTime
      name = 'Date',  #define x-axis label
      date_breaks = '7 day',  #define x-axis tick interval
      date_labels = '%b-%d'  #custom formatting for date labels
    ) + 
    scale_y_continuous(
      name = 'Daily Average Flow (cfs)',
      labels = scales::comma,
      breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)
    ) +
    labs(
      title = 'Sum of Inputs, SCHISM, and Below Liberty Island Flows- Daily Averages',
      subtitle = "SCHISM and Below Liberty are tidally filtered and lagged for 1 day",
      caption = "BelowLiberty = Cache Sl - Miner Sl"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  #vertical x-axis labels 

dev.off()


# Sampling Events Only ----------------------------------------------------

# Import Flow data for 2017
SamplingEvents <- read_excel("Flows/DailyAvgFlows_All_and_SE.xlsx", sheet = "Just Sampling Events") %>% 
  filter(Year == 2017) %>% 
  # Round flow values to tenths place
  mutate(Flow = round(Flow, 1))


# Pull out data for Cache and Miner Sloughs and calculate Below Liberty Island flows
BLibIs <- SamplingEvents %>% 
  filter(LocType == "Below Liberty") %>%
  spread(StationName, Flow) %>% 
  clean_names(case = "upper_camel") %>% 
  mutate(BelowLiberty = CacheSloughNearRyerIsland - MinerSloughNearSacRiver) %>% 
  select(-c(CacheSloughNearRyerIsland, MinerSloughNearSacRiver)) %>% 
  gather(StationName, Flow, BelowLiberty)

# Combine Below Liberty Island flows back to SamplingEvents df and summarize by location
SamplingEventsSum <- SamplingEvents %>% 
  filter(LocType != "Below Liberty") %>%
  bind_rows(BLibIs) %>% 
  mutate(Location = case_when(
    LocType == "Inlet"  ~ "Sum of Inputs",
    LocType == "Outlet" ~ "SCHISM",
    TRUE                ~ "Below Liberty"
    )
  ) %>% 
  group_by(SamplingEvent, Location) %>% 
  summarize(TotalFlow = sum(Flow)) %>% 
  ungroup() %>% 
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
    Location = factor(
      Location,
      levels = c(
        "Sum of Inputs",
        "SCHISM",
        "Below Liberty"
      )
    )
  )

# Plot flows by Location
SamplingEventsSum %>% 
  ggplot(data = .,aes(
    x = SamplingEvent,  #barplot by Sampling Event
    y = TotalFlow,
    fill = Location,  #make each Approach a different fill color
    label = round(TotalFlow, 0)  #label plots with Flow values
  )) +   
  geom_col(position = "dodge") + 
  geom_text(position = position_dodge(width = 0.9), size = 2.5) +
  labs(
    title = "Sampling Event Flows",
    subtitle = "Labels are the flow values",
    x = "Sampling Event",
    y = "Daily Average Flow (cfs)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  #x-axis labels at 45 degrees


  