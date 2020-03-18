# Yolo Bypass Mass Balance Study
# Purpose: Figures for Final Report - Chapter 3
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(lubridate)
library(scales)
library(openwaterhg)


# Figure 3-5 --------------------------------------------------------------
# Line plot showing daily average flow in cfs for the Fremont Weir and the CCSB
# Red markers indicate the sampling events
# Hydrographs arranged horizontally
# 2017 flood event only

# Filter daily average flow data to only include 2017 and Fremont Weir and CCSB data
daily_flow_clean1 <- daily_flow_data_all %>% 
  filter(
    Year == 2017,
    str_detect(StationName, "^CCSB|^Fremont")
  )

# Create a df of CCSB data
ccsb <- filter(daily_flow_clean1, str_detect(StationName, "^CCSB"))

# Sum the CCSB flows
ccsb_total <- ccsb %>% 
  group_by(Date) %>% 
  summarize(Flow = sum(Flow)) %>% 
  mutate(StationName = "Cache Creek Settling Basin")

# Add the summed CCSB flows to the daily flow df
daily_flow_clean <- daily_flow_clean1 %>% 
  anti_join(ccsb, by = c("Date", "StationName")) %>% 
  bind_rows(ccsb_total) %>% 
  select(Date, StationName, Flow)

# Clean up
rm(daily_flow_clean1, ccsb, ccsb_total)

# Create a df of sampling events to mark these on the hydrographs
se_dates <- tibble(
  Date = as_date(
    c("2017-01-11",
      "2017-01-24",
      "2017-01-31",
      "2017-02-14",
      "2017-03-01",
      "2017-03-15",
      "2017-03-28",
      "2017-04-11",
      "2017-04-25"
    )
  )
)

# Join se_dates df to daily_flow_clean to pull out data for just the sampling events
se_flows <- inner_join(daily_flow_clean, se_dates)


# Create Figure 3-5
figure_3_5 <- daily_flow_clean %>% 
  ggplot(aes(x = Date, y = Flow)) +
  geom_line() +
  geom_point(
    data = se_flows, 
    color = "red",
    size = 2
  ) +
  labs(
    x = NULL,
    y = "Daily Average Flow (cfs)"
  ) +
  facet_wrap(
    vars(StationName),
    scales = "free_y"
  ) +
  scale_x_date(
    breaks = breaks_pretty(10),
    labels = label_date_short(),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = label_comma(),
    limits = c(0, NA)
  ) +
  theme_owhg()

# Export figure 3-5
ggsave(
  "Ch3_final_report_fig3-5.jpg", 
  plot = figure_3_5,
  dpi = 300,
  width = 6.5, 
  height = 4, 
  units = "in"
)
  

# Figure 3-6 --------------------------------------------------------------
# Filled bar plot showing percentage of total inflow for each inlet
# 2017 sampling events only

daily_flow_data_se


# Figure 3-8 --------------------------------------------------------------
# Filled bar plot showing percentage of total input load for each inlet
# 2017 sampling events only

loads_calc


# Figure 3-10 -------------------------------------------------------------
# Filled bar plot showing percentage of filtered and particulate MeHg fractions
# Facets for each inlet
# 2017 sampling events only

loads_calc


# Figure 3-12 -------------------------------------------------------------
# Bar plots showing export loads at the Stairsteps
# Facets for each analyte/parameter
# 2017 sampling events only

loads_calc


# Figure 3-13 -------------------------------------------------------------
# Bar plots showing net loads of the upper and lower reaches
# Upper reach is between the inlets to the Stairsteps
# Lower reach is between the Stairsteps and Below Liberty Island
# Facets for each analyte/parameter
# 2017 sampling events only

loads_calc
