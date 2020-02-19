# Yolo Bypass Inlet-Outlet Study
# Create plots of the Inlet flows for all sampling events

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(rlang)
library(patchwork)

# Import flow data and setup for plotting ---------------------------------

# Import data
FlowData <- read_excel("Flows/DailyAvgFlows_All_and_SE.xlsx", sheet = "Just Sampling Events") %>% 
  # Only keep inlet flows
  filter(LocType == "Inlet") %>% 
  select(-LocType)

# Sum the CCSB Flows
CCSB_Flows <- FlowData %>% 
  filter(str_detect(StationName, "^CCSB")) %>% 
  group_by(SamplingEvent, Year) %>% 
  summarize(TotalFlow = sum(Flow)) %>% 
  ungroup() %>% 
  rename(Flow = TotalFlow) %>% 
  add_column(StationName = "CCSB")

# Add back the summed CCSB Flows
FlowData <- FlowData %>% 
  filter(!str_detect(StationName, "^CCSB")) %>%
  bind_rows(CCSB_Flows)

# Make some modifications to the FlowData df for plotting
Flows_plot <- FlowData %>% 
  mutate(
    # add year to SamplingEvent
    SamplingEvent = paste0(SamplingEvent, ", ", Year),
    # Shorten some of the StationNames
    StationName = case_when(
      StationName == "Knights Landing Ridge Cut" ~ "KLRC",
      StationName == "Putah Creek at Mace Blvd" ~ "Putah Ck",
      StationName == "Sac River above the Sacramento Weir" ~ "Sac Weir",
      TRUE ~ StationName
    )
  )

# Setup plotting order
SampEvents <- sort(unique(Flows_plot$SamplingEvent))
SampEventsOrder <- SampEvents[c(3,9,5:7,4,8,10:11,1:2)]

InletOrder <- c(
  "KLRC", 
  "CCSB", 
  "Putah Ck", 
  "Sac Weir", 
  "Fremont Weir"
)

Flows_plot <- Flows_plot %>% 
  mutate(
    SamplingEvent = factor(SamplingEvent, levels = SampEventsOrder),
    StationName = factor(StationName, levels = InletOrder)
  )


# Create Plots of the Inlet Flows -----------------------------------------

# Total Inlet flow for each sampling event
p1 <- Flows_plot %>% 
  ggplot(
    aes(
      x = SamplingEvent,
      y = Flow,
      fill = StationName
    )
  ) +
  geom_col() +
  labs(
    title = "Total Inlet flows for each sampling event",
    x = NULL,
    y = "Daily Average Flow (cfs)"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  #x-axis labels at 90 degrees

# Inlet flows stacked by proportion of total flow for each sampling event
p2 <- Flows_plot %>% 
  ggplot(
    aes(
      x = SamplingEvent,
      y = Flow,
      fill = StationName
    )
  ) +
  geom_col(position = "fill") +
  labs(
    title = "Proportion of each input to the total inflow for each sampling event",
    x = "Sampling Event",
    y = "Proportion of Total Inflow"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  #x-axis labels at 90 degrees

# Print Inlet Flow plots to a pdf file
pdf(file = "Flows/Inlet_Flows.pdf", w = 11, h = 8.5)

  p1 + p2 + plot_layout(ncol = 1)
  
dev.off()

# Calculate a few summary statistics of the flows -------------------------

TotalInflows <- Flows_plot %>% 
  group_by(SamplingEvent) %>% 
  summarize(TotalFlow = sum(Flow)) %>% 
  ungroup()

TotalInflows %>% write_excel_csv("TotalInflow.csv")

InletFlows_Per <- Flows_plot %>% 
  left_join(TotalInflows) %>% 
  mutate(PerFlow = (Flow/TotalFlow)*100)

# Create function to spread data to be used on a different variable each time
SpreadFlowData <- function(df, SpreadVar) {
  SpreadVar.quo <- enquo(SpreadVar)
  
  dfSpread <- df %>% 
    select(
      c(SamplingEvent,
        Year,
        StationName,
        !!SpreadVar.quo
      )
    ) %>% 
    spread(StationName, !!SpreadVar.quo)
  
  return(dfSpread)
}

# Spread InletFlows_Per df by Flow and export as .csv
SpreadFlowData(InletFlows_Per, Flow) %>% 
  write_excel_csv("InputFlows.csv", na = "0")

# Spread InletFlows_Per df by PerFlow and export as .csv
SpreadFlowData(InletFlows_Per, PerFlow) %>% 
  write_excel_csv("InputFlows_per.csv", na = "0")


