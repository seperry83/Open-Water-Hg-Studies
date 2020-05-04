# Yolo Bypass Mass Balance Study
# Purpose: Figures for Final Report - Chapter 3
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(lubridate)
library(scales)
library(openwaterhg)


# Functions for script ----------------------------------------------------

# Create a function to rename Analytes for figures of load data
rename_ana <- function(df) {
  # define plot order for the analytes
  analytes_order <- c(
    "fHg (g/day)",
    "pHg (g/day)",
    "uHg (g/day)",
    "fMeHg (g/day)",
    "pMeHg (g/day)",
    "uMeHg (g/day)",
    "DOC (1,000 kg/day)",
    "POC (1,000 kg/day)",
    "TOC (1,000 kg/day)",
    "TSS (1,000 kg/day)"
  )
  
  # rename analytes and convert to factor to apply plot order
  df <- df %>% 
    mutate(
      Analyte = case_when(
        str_detect(Analyte, "OC$|^TSS") ~ paste0(Analyte, " (", LoadUnits, ")"),
        Analyte == "MeHg- filtered" ~ paste0("fMeHg (", LoadUnits, ")"),
        Analyte == "MeHg- particulate" ~ paste0("pMeHg (", LoadUnits, ")"),
        Analyte == "MeHg- total" ~ paste0("uMeHg (", LoadUnits, ")"),
        Analyte == "THg- filtered" ~ paste0("fHg (", LoadUnits, ")"),
        Analyte == "THg- particulate" ~ paste0("pHg (", LoadUnits, ")"),
        Analyte == "THg- total" ~ paste0("uHg (", LoadUnits, ")")
      ),
      Analyte = factor(Analyte, levels = analytes_order)
    )
  
  return(df)
}


# Figure 3-5 --------------------------------------------------------------
# Line plot showing daily average flow in cfs for the Fremont Weir and the CCSB
# Red markers indicate the sampling events
# Hydrographs arranged horizontally
# 2017 flood event only

# Bring in daily flow data for the inlets
source("YB_Mass_Balance/Flows/Import_Inlet_Flow_Data_all.R")

# Filter daily average flow data to only include 2017 and Fremont Weir and CCSB data
daily_flow_clean <- flows_inlet_all %>% 
  filter(
    Year == 2017,
    str_detect(StationName, "^CCSB|^Fremont")
  )

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
  
# Clean up 
rm(daily_flow_clean, figure_3_5, flows_inlet_all, se_dates, se_flows)


# Figure 3-6 --------------------------------------------------------------
# Filled bar plot showing percentage of total inflow for each inlet
# 2017 sampling events only

# Bring in daily flow data for the inlets for just sampling events
source("YB_Mass_Balance/Flows/Import_Inlet_Flow_Data_SE.R")

# Prepare data for plotting
se_flow_clean <- flows_inlet_se %>% 
  # Filter sampling event flow data to only include 2017 data
  filter(Year == 2017) %>% 
  # convert variables to factors to apply plot order
  conv_fact_inlet_names() %>% 
  conv_fact_samplingevent() %>% 
  select(SamplingEvent, StationName, Flow)

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

# Clean up
rm(figure_3_6, flows_inlet_se, se_flow_clean)


# Figure 3-7 --------------------------------------------------------------
# Bar plots showing total input loads
# Facets for each analyte/parameter
# 2017 sampling events only

# Bring in total load data
source("YB_Mass_Balance/Loads/Import_Total_Load_Data.R")

# Prepare total load data for figure 3-7
loads_total_inlet <- loads_total %>% 
  # Filter data to only include inlet loads, 2017 data, and necessary parameters
  filter(
    Year == 2017,
    str_detect(Analyte, "^MeHg|^THg|OC$|^TSS"),
    LocType == "Inlet"
  ) %>% 
  # Rename analytes
  rename_ana() %>% 
  # Convert SamplingEvent to factor to apply plot order
  conv_fact_samplingevent() %>% 
  # Round total loads to proper number of significant figures
  mutate(total_load = signif(total_load, digits))

# Create Figure 3-7
figure_3_7 <- loads_total_inlet %>% 
  ggplot(aes(x = SamplingEvent, y = total_load)) +
  geom_col() +
  facet_wrap(
    vars(Analyte),
    ncol = 3,
    scales = "free_y"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_owhg(x_axis_v = TRUE)

# Export figure 3-7
ggsave(
  "Ch3_final_report_fig3-7.jpg", 
  plot = figure_3_7,
  dpi = 300,
  width = 6.5,
  height = 6.5,
  units = "in"
)

# Clean up
rm(figure_3_7, loads_total, loads_total_inlet)


# Process Inlet Load data for Figures 3-8 to 3-10 -------------------------

# Bring in inlet load data
source("YB_Mass_Balance/Loads/Import_Inlet_Load_Data.R")

# Prepare inlet load data for figures
loads_inlet_clean <- loads_inlet %>% 
  # Remove added zeros for events when the weirs weren't spilling
  anti_join(zero_loads) %>% 
  # Filter load data to only include 2017 data and necessary parameters
  filter(
    Year == 2017,
    str_detect(Analyte, "^MeHg|^THg|OC$|^TSS")
  ) %>% 
  # Rename analytes
  rename_ana() %>% 
  # Convert variables to factor to apply plot order
  conv_fact_samplingevent() %>% 
  conv_fact_inlet_names() %>% 
  # Round loads to proper number of significant figures
  mutate(Load = signif(Load, digits))

rm(loads_inlet, zero_loads)


# Figure 3-8 --------------------------------------------------------------
# Filled bar plot showing percentage of total input load for each inlet
# Facets for each Analyte
# 2017 sampling events only

# Create Figure 3-8
figure_3_8 <- loads_inlet_clean %>% 
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

# Clean up
rm(figure_3_8)


# Figures 3-9 and 3-10 -------------------------------------------------------------
# Filled bar plots showing percentage of filtered and particulate fractions of:
  # Hg - Figure 3-9
  # MeHg - Figure 3-10
# Facets for each inlet
# 2017 sampling events only

# Prepare inlet load data for figures 3-9 and 3-10
loads_inlet_hg_frac <- loads_inlet_clean %>% 
  # Pull out just Hg and MeHg
  filter(str_detect(Analyte, "Hg")) %>% 
  # create new variables
  mutate(
    Analyte = as.character(Analyte),
    Fraction = case_when(
      str_detect(Analyte, "^f") ~ "Filtered",
      str_detect(Analyte, "^p") ~ "Particulate",
      str_detect(Analyte, "^u") ~ "Unfiltered"
    ),
    Analyte = if_else(str_detect(Analyte, "MeHg"), "MeHg", "Hg")
  ) %>% 
  # remove Unfiltered data
  filter(Fraction != "Unfiltered") %>% 
  # nest df based on Analyte variable
  group_nest(Analyte)

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
in_loads_clean_hg_figs <- loads_inlet_hg_frac %>% 
  mutate(figures = map2(data, Analyte, .f = plot_per_frac))

# Export figure 3-9
ggsave(
  "Ch3_final_report_fig3-9.jpg", 
  plot = in_loads_clean_hg_figs$figures[[1]],
  dpi = 300,
  width = 5.75, 
  height = 4, 
  units = "in"
)

# Export figure 3-10
ggsave(
  "Ch3_final_report_fig3-10.jpg", 
  plot = in_loads_clean_hg_figs$figures[[2]],
  dpi = 300,
  width = 5.75, 
  height = 4, 
  units = "in"
)

# Clean up
rm(in_loads_clean_hg_figs, loads_inlet_clean, loads_inlet_hg_frac, plot_per_frac)


# Figure 3-12 -------------------------------------------------------------
# Bar plots showing export loads at the Stairsteps
# Facets for each analyte/parameter
# 2017 sampling events only

# Bring in total load data
source("YB_Mass_Balance/Loads/Import_Total_Load_Data.R")

# Prepare total load data for figure 3-12
loads_total_out <- loads_total %>% 
  # Filter data to only include outlet loads, 2017 data, and necessary parameters
  filter(
    Year == 2017,
    str_detect(Analyte, "^MeHg|^THg|OC$|^TSS"),
    LocType == "Outlet"
  ) %>% 
  # Rename analytes
  rename_ana() %>% 
  # Convert SamplingEvent to factor to apply plot order
  conv_fact_samplingevent() %>% 
  # Round total loads to proper number of significant figures
  mutate(total_load = signif(total_load, digits))

# Create Figure 3-12
figure_3_12 <- loads_total_out %>% 
  ggplot(aes(x = SamplingEvent, y = total_load)) +
  geom_col() +
  facet_wrap(
    vars(Analyte),
    ncol = 3,
    scales = "free_y"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_owhg(x_axis_v = TRUE)

# Export figure 3-12
ggsave(
  "Ch3_final_report_fig3-12.jpg", 
  plot = figure_3_12,
  dpi = 300,
  width = 6.5,
  height = 6.5,
  units = "in"
)

# Clean up
rm(figure_3_12, loads_total, loads_total_out)
  

# Figure 3-13 -------------------------------------------------------------
# Bar plots showing net loads of the upper reach and the entire Yolo Bypass
# Upper reach is between the inlets to the Stairsteps
# Entire Bypass is between the inlets and Below Liberty Island
# Facets for each analyte/parameter
# 2017 sampling events only

# Bring in net load data
source("YB_Mass_Balance/Loads/Import_Net_Load_Data.R")

# Prepare net load data for figure 3-13
loads_net_clean <- loads_net %>% 
  # Filter load data to only include the upper and entire reaches,
  # 2017 data, and necessary parameters
  filter(
    Year == 2017,
    str_detect(Analyte, "^MeHg|^THg|OC$|^TSS"),
    Reach %in% c("Entire", "Upper")
  ) %>% 
  # Rename analytes
  rename_ana() %>% 
  # Rename Reaches
  mutate(
    Reach = case_when(
      Reach == "Entire" ~ "Inlets to Below Liberty Island", 
      Reach == "Upper" ~ "Inlets to Stairsteps"
    ),
    # Convert Reach to a factor to apply plot order
    Reach = factor(Reach, levels = c("Inlets to Stairsteps", "Inlets to Below Liberty Island"))
  ) %>% 
  # Convert SamplingEvent to factor to apply plot order
  conv_fact_samplingevent() %>% 
  # Round net loads to proper number of significant figures
  mutate(net_load = signif(net_load, digits))

# Create Figure 3-13
figure_3_13 <- loads_net_clean %>% 
  ggplot(aes(x = SamplingEvent, y = net_load, fill = Reach)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  facet_wrap(
    vars(Analyte),
    ncol = 3,
    scales = "free_y"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  add_gen_color_pal(2) +
  theme_owhg(x_axis_v = TRUE) +
  theme(
    legend.margin = margin(0, 0, 0, 0),
    legend.position = c(0.53, -0.07)
  )

# Export figure 3-13
ggsave(
  "Ch3_final_report_fig3-13.jpg", 
  plot = figure_3_13,
  dpi = 300,
  width = 6.3,
  height = 6.3,
  units = "in"
)

# Clean up
rm(figure_3_13, loads_net, loads_net_clean)

