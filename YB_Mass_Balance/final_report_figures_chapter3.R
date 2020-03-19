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

# Create a df of CCSB flow data
ccsb_flow <- filter(daily_flow_clean1, str_detect(StationName, "^CCSB"))

# Sum the CCSB flows
ccsb_flow_total <- ccsb_flow %>% 
  group_by(Date) %>% 
  summarize(Flow = sum(Flow)) %>% 
  mutate(StationName = "Cache Creek Settling Basin")

# Add the summed CCSB flows to the daily flow df
daily_flow_clean <- daily_flow_clean1 %>% 
  anti_join(ccsb_flow, by = "StationName") %>% 
  bind_rows(ccsb_flow_total) %>% 
  select(Date, StationName, Flow)

# Clean up
rm(daily_flow_clean1, ccsb_flow, ccsb_flow_total)

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
  facet_wrap(
    vars(StationName),
    scales = "free_y"
  ) +
  labs(
    x = NULL,
    y = "Daily Average Flow (cfs)"
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
  height = 3.5, 
  units = "in"
)
  

# Figure 3-6 --------------------------------------------------------------
# Filled bar plot showing percentage of total inflow for each inlet
# 2017 sampling events only

# Filter sampling event flow data to only include inlet stations and 2017 data
se_flow_clean1 <- daily_flow_data_se %>% 
  filter(
    LocType == "Inlet",
    Year == 2017
  )

# Create a df of CCSB flow data
se_ccsb_flow <- filter(se_flow_clean1, str_detect(StationName, "^CCSB"))

# Sum the CCSB flows
se_ccsb_flow_total <- se_ccsb_flow %>% 
  group_by(SamplingEvent) %>% 
  summarize(Flow = sum(Flow)) %>% 
  mutate(StationName = "CCSB")

# Add the summed CCSB flows to the se_flow df and prepare for plotting
se_flow_clean <- se_flow_clean1 %>% 
  anti_join(se_ccsb_flow, by = "StationName") %>% 
  bind_rows(se_ccsb_flow_total) %>% 
  # change some of the station names
  mutate(
    StationName = case_when(
      StationName == "Knights Landing Ridge Cut" ~ "KLRC",
      StationName == "Putah Creek at Mace Blvd" ~ "Putah Creek",
      StationName == "Sac River above the Sacramento Weir" ~ "Sacramento Weir",
      TRUE ~ StationName
    )
  ) %>% 
  # convert variables to factors to apply plot order
  conv_fact_inlet_names() %>% 
  conv_fact_samplingevent() %>% 
  select(SamplingEvent, StationName, Flow)

# Clean up
rm(se_flow_clean1, se_ccsb_flow, se_ccsb_flow_total)

# Create Figure 3-6
figure_3_6 <- se_flow_clean %>% 
  ggplot(aes(x = SamplingEvent, y = Flow, fill = StationName)) +
  geom_col(position = "fill") +
  labs(
    x = NULL,
    y = "Percentage of Total Inflow"
  ) +
  add_inlet_color_pal("fill", legend_title = "Inlet") +
  theme_owhg(x_axis_v = TRUE) +
  scale_y_continuous(labels = percent_format())

# Export figure 3-6
ggsave(
  "Ch3_final_report_fig3-6.jpg", 
  plot = figure_3_6,
  dpi = 300,
  width = 5.5, 
  height = 4, 
  units = "in"
)


# Process Load Data for Figures 3-8 through 3-10 ------------------------------

# Filter load data to only include inlet stations, 2017 data, and necessary parameters
in_loads_clean1 <- loads_calc %>% 
  filter(
    LocType == "Inlet",
    Year == 2017,
    str_detect(Analyte, "^MeHg|^THg|OC$|^TSS")
  )

# Create a df of CCSB load data
ccsb_loads <- filter(in_loads_clean1, str_detect(StationName, "^CCSB"))

# Sum the CCSB loads
ccsb_loads_total <- ccsb_loads %>% 
  group_by(SamplingEvent, Analyte, LoadUnits) %>% 
  summarize(Load = sum(Load)) %>% 
  mutate(StationName = "CCSB")

# Add the summed CCSB loads to the inlet loads df and prepare for plotting
in_loads_clean <- in_loads_clean1 %>% 
  anti_join(ccsb_loads, by = "StationName") %>% 
  bind_rows(ccsb_loads_total) %>% 
  # change some of the station names
  mutate(
    StationName = case_when(
      StationName == "Knights Landing Ridge Cut" ~ "KLRC",
      StationName == "Putah Creek at Mace Blvd" ~ "Putah Creek",
      StationName == "Sac River above the Sacramento Weir" ~ "Sacramento Weir",
      TRUE ~ StationName
    )
  ) %>% 
  # convert variables to factors to apply plot order
  conv_fact_inlet_names() %>% 
  conv_fact_samplingevent() %>% 
  select(SamplingEvent, StationName, Analyte, Load, LoadUnits)

# Clean up
rm(in_loads_clean1, ccsb_loads, ccsb_loads_total)


# Figure 3-8 --------------------------------------------------------------
# Filled bar plot showing percentage of total input load for each inlet
# Facets for each Analyte
# 2017 sampling events only

# Prepare in_loads_clean df for figure 3-8
in_loads_clean_all1 <- in_loads_clean %>% 
  # change some of the analyte names
  mutate(
    Analyte = case_when(
      str_detect(Analyte, "OC$|^TSS") ~ paste0(Analyte, " (", LoadUnits, ")"),
      Analyte == "MeHg- filtered" ~ paste0("fMeHg (", LoadUnits, ")"),
      Analyte == "MeHg- particulate" ~ paste0("pMeHg (", LoadUnits, ")"),
      Analyte == "MeHg- total" ~ paste0("uMeHg (", LoadUnits, ")"),
      Analyte == "THg- filtered" ~ paste0("fHg (", LoadUnits, ")"),
      Analyte == "THg- particulate" ~ paste0("pHg (", LoadUnits, ")"),
      Analyte == "THg- total" ~ paste0("uHg (", LoadUnits, ")")
    )
  )

# Convert Analyte variable to a factor to apply plot order
analytes <- sort(unique(in_loads_clean_all1$Analyte))
analytes_order <- analytes[c(2,4,9,3,5,10,1,6:8)]
in_loads_clean_all <- mutate(in_loads_clean_all1, Analyte = factor(Analyte, levels = analytes_order))

# Clean up
rm(in_loads_clean_all1, analytes, analytes_order)

# Create Figure 3-8
figure_3_8 <- in_loads_clean_all %>% 
  ggplot(aes(x = SamplingEvent, y = Load, fill = StationName)) +
  geom_col(position = "fill") +
  facet_wrap(
    vars(Analyte),
    ncol = 3
  ) +
  labs(
    x = NULL,
    y = "Percentage of Total Inlet Load"
  ) +
  add_inlet_color_pal("fill") +
  theme_owhg(x_axis_v = TRUE) +
  theme(
    legend.margin = margin(0, 0, 0, 0),
    legend.position = c(0.65, -0.1)
  ) +
  guides(fill = guide_legend(keyheight = 0.95)) +
  scale_y_continuous(labels = percent_format())

# Export figure 3-8
ggsave(
  "Ch3_final_report_fig3-8.jpg", 
  plot = figure_3_8,
  dpi = 300,
  width = 5.75, 
  height = 6.5, 
  units = "in"
)


# Figures 3-9 and 3-10 -------------------------------------------------------------
# Filled bar plots showing percentage of filtered and particulate fractions of:
  # Hg - Figure 3-9
  # MeHg - Figure 3-10
# Facets for each inlet
# 2017 sampling events only

# Prepare in_loads_clean df for figures 3-9 and 3-10
in_loads_clean_hg <- in_loads_clean %>% 
  filter(str_detect(Analyte, "filtered|particulate")) %>% 
  separate(Analyte, into = c("Analyte", "Fraction"), sep = "- ") %>% 
  mutate(Fraction = str_to_title(Fraction)) %>% 
  group_nest(Analyte) %>% 
  mutate(Analyte = if_else(Analyte == "THg", "Hg", Analyte))

# Create function for figures 3-9 and 3-10
plot_per_frac <- function(df, param) { 
  p <- 
    ggplot(
      data = df,
      aes(
        x = SamplingEvent, 
        y = Load, 
        fill = Fraction
      )
    ) +
    geom_col(
      color = "gray30",
      position = "fill"
    ) +
    facet_wrap(
      vars(StationName),
      ncol = 3
    ) +
    labs(
      x = NULL,
      y = paste0("Percentage of each ", param, " Fraction")
    ) +
    add_gen_color_pal(2) +
    theme_owhg(x_axis_v = TRUE) +
    theme(
      legend.margin = margin(0, 0, 0, 0),
      legend.position = c(0.85, -0.1)
    ) +
    scale_y_continuous(labels = percent_format())
  
  return(p)  
}

# Create Figures 3-9 and 3-10
in_loads_clean_hg_figs <- in_loads_clean_hg %>% 
  mutate(figures = map2(data, Analyte, .f = plot_per_frac))

# Export figure 3-9
ggsave(
  "Ch3_final_report_fig3-9.jpg", 
  plot = in_loads_clean_hg_figs$figures[[2]],
  dpi = 300,
  width = 5.75, 
  height = 4, 
  units = "in"
)

# Export figure 3-10
ggsave(
  "Ch3_final_report_fig3-10.jpg", 
  plot = in_loads_clean_hg_figs$figures[[1]],
  dpi = 300,
  width = 5.75, 
  height = 4, 
  units = "in"
)


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
