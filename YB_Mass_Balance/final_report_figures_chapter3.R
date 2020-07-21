# Yolo Bypass Mass Balance Study
# Purpose: Figures for Final Report - Chapter 3
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(broom)
library(openwaterhg)


# Functions for script ----------------------------------------------------

# Create a function to rename Analytes for figures of load data
rename_analytes <- function(df) {
  df <- df %>% 
    mutate(
      Analyte = case_when(
        str_detect(Analyte, "OC$|SS$") ~ paste0(Analyte, " (Mg/day)"),
        Analyte == "MeHg- filtered" ~ paste0("fMeHg (", LoadUnits, ")"),
        Analyte == "MeHg- particulate" ~ paste0("pMeHg (", LoadUnits, ")"),
        Analyte == "MeHg- total" ~ paste0("uMeHg (", LoadUnits, ")"),
        Analyte == "THg- filtered" ~ paste0("fHg (", LoadUnits, ")"),
        Analyte == "THg- particulate" ~ paste0("pHg (", LoadUnits, ")"),
        Analyte == "THg- total" ~ paste0("uHg (", LoadUnits, ")")
      )
    )
  
  return(df)
}

# Create a function to apply a plotting order for the analytes
conv_fact_analytes <- function(df) {
  analytes_order <- c(
    "uHg (g/day)",
    "fHg (g/day)",
    "pHg (g/day)",
    "uMeHg (g/day)",
    "fMeHg (g/day)",
    "pMeHg (g/day)",
    "TOC (Mg/day)",
    "DOC (Mg/day)",
    "POC (Mg/day)",
    "TSS (Mg/day)"
  )
  
  df <- df %>% mutate(Analyte = factor(Analyte, levels = analytes_order))
  
  return(df)
}

obj_keep <- c("obj_keep", "rename_analytes", "conv_fact_analytes")


# Figure 3-5 --------------------------------------------------------------
# Line plot showing daily average flow in cfs for the Fremont Weir and the CCSB
# Red markers indicate the sampling events
# Hydrographs arranged horizontally
# 2017 flood event only

# Define figure number for easier updating
fig_num <- as.character(5)

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

# Create Figure
figure <- daily_flow_clean %>% 
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

# Export Figure
ggsave(
  paste0("Ch3_final_report_fig3-", fig_num, ".jpg"), 
  plot = figure,
  dpi = 300,
  width = 6.5, 
  height = 3.5, 
  units = "in"
)
  
# Clean up 
rm(list= ls()[!(ls() %in% obj_keep)])


# Figure 3-6 --------------------------------------------------------------
# Filled bar plot showing percentage of total inflow for each inlet
# 2017 sampling events only

# Define figure number for easier updating
fig_num <- as.character(6)

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

# Create Figure
figure <- se_flow_clean %>% 
  ggplot(aes(x = SamplingEvent, y = Flow, fill = StationName)) +
  geom_col(position = "fill") +
  labs(
    x = NULL,
    y = "Percentage of Total Inflow"
  ) +
  add_inlet_color_pal("fill", legend_title = "Inlet") +
  theme_owhg(x_axis_v = TRUE) +
  scale_y_continuous(labels = percent_format())

# Export Figure
ggsave(
  paste0("Ch3_final_report_fig3-", fig_num, ".jpg"), 
  plot = figure,
  dpi = 300,
  width = 5.5, 
  height = 4, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figure 3-7 --------------------------------------------------------------
# Same as Figure B-9 in Technical Appendix


# Figure 3-8 --------------------------------------------------------------
# Bar plots showing total input loads with each individual inlet stacked on top of each other 
  # for each event
# Facets for each analyte/parameter
# 2017 sampling events only

# Define figure number for easier updating
fig_num <- as.character(8)

# Bring in inlet load data
source("YB_Mass_Balance/Loads/Import_Inlet_Load_Data.R")

# Prepare data for plotting
loads_inlet_clean <- loads_inlet %>% 
  # Remove added zeros for events when the weirs weren't spilling
  anti_join(zero_loads) %>% 
  # Filter data to only include 2017 data and necessary parameters
  filter(
    Year == 2017,
    str_detect(Analyte, "^MeHg|^THg|OC$|^TSS")
  ) %>% 
  # Rename analytes
  rename_analytes() %>% 
  # Convert variables to factor to apply plot order
  conv_fact_analytes() %>% 
  conv_fact_samplingevent() %>% 
  conv_fact_inlet_names() %>% 
  # Round total loads to proper number of significant figures
  mutate(Load = signif(Load, digits)) %>% 
  select(SamplingEvent, StationName, Analyte, Load)

# Create Figure
figure <- loads_inlet_clean %>% 
  ggplot(aes(x = SamplingEvent, y = Load, fill = StationName)) +
  geom_col(position = "stack") +
  facet_wrap(
    vars(Analyte),
    ncol = 3,
    scales = "free_y"
  ) +
  scale_y_continuous(
    name = NULL,
    labels = label_comma()
  ) +
  xlab(NULL) +
  add_inlet_color_pal("fill") +
  theme_owhg(x_axis_v = TRUE) +
  theme(
    legend.margin = margin(0, 0, 0, 0),
    legend.position = c(0.55, -0.04)
  ) +
  guides(fill = guide_legend(keyheight = 0.95))

# Export Figure
ggsave(
  paste0("Ch3_final_report_fig3-", fig_num, ".jpg"), 
  plot = figure,
  dpi = 300,
  width = 6.5,
  height = 8,
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figure 3-9 --------------------------------------------------------------
# Filled bar plot showing percentage of total input load for each inlet
# Facets for each Analyte
# 2017 sampling events only

# Define figure number for easier updating
fig_num <- as.character(9)

# Bring in inlet load data
source("YB_Mass_Balance/Loads/Import_Inlet_Load_Data.R")

# Prepare inlet load data for figure
loads_inlet_clean <- loads_inlet %>% 
  # Remove added zeros for events when the weirs weren't spilling
  anti_join(zero_loads) %>% 
  # Filter load data to only include 2017 data and necessary parameters
  filter(
    Year == 2017,
    str_detect(Analyte, "^MeHg|^THg|OC$|^TSS")
  ) %>% 
  # Rename analytes
  rename_analytes() %>% 
  # Convert variables to factor to apply plot order
  conv_fact_analytes() %>% 
  conv_fact_samplingevent() %>% 
  conv_fact_inlet_names()

# Create Figure
figure <- loads_inlet_clean %>% 
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

# Export Figure
ggsave(
  paste0("Ch3_final_report_fig3-", fig_num, ".jpg"), 
  plot = figure,
  dpi = 300,
  width = 5.75, 
  height = 6.5, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figures 3-10 and 3-11 -------------------------------------------------------------
# Filled bar plots showing percentage of filtered and particulate fractions of:
  # Hg - Figure 3-10
  # MeHg - Figure 3-11
# Facets for each inlet
# 2017 sampling events only

# Define figure numbers for easier updating
fig_num_hg <- as.character(10)
fig_num_mehg <- as.character(11)

# Bring in inlet load data
source("YB_Mass_Balance/Loads/Import_Inlet_Load_Data.R")

# Prepare inlet load data for figures
loads_inlet_hg_frac <- loads_inlet %>% 
  # Remove added zeros for events when the weirs weren't spilling
  anti_join(zero_loads) %>% 
  # Filter load data to only include 2017 data and necessary parameters
  filter(
    Year == 2017,
    str_detect(Analyte, "Hg")
  ) %>% 
  # Create new variables for Fraction and Analyte
  separate(Analyte, into = c("Analyte", "Fraction"), sep = "- ") %>% 
  mutate(Fraction = str_to_title(Fraction)) %>% 
  # Rename THg to Hg
  mutate(Analyte = if_else(Analyte == "THg", "Hg", Analyte)) %>% 
  # remove Unfiltered data
  filter(Fraction != "Total") %>%
  # Convert variables to factor to apply plot order
  conv_fact_samplingevent() %>% 
  conv_fact_inlet_names() %>% 
  # nest df based on Analyte variable
  group_nest(Analyte)

# Create function for filled barplots
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

# Create Figures
loads_inlet_hg_frac_figs <- loads_inlet_hg_frac %>% 
  mutate(figures = map2(data, Analyte, .f = plot_per_frac))

# Export Figure for THg
ggsave(
  paste0("Ch3_final_report_fig3-", fig_num_hg, ".jpg"),
  plot = loads_inlet_hg_frac_figs$figures[[1]],
  dpi = 300,
  width = 5.75, 
  height = 4, 
  units = "in"
)

# Export Figure for MeHg
ggsave(
  paste0("Ch3_final_report_fig3-", fig_num_mehg, ".jpg"),
  plot = loads_inlet_hg_frac_figs$figures[[2]],
  dpi = 300,
  width = 5.75, 
  height = 4, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figure 3-12 -------------------------------------------------------------
# Bar plots showing export loads at the Stairsteps
# Facets for each analyte/parameter
# 2017 sampling events only

# Define figure number for easier updating
fig_num <- as.character(12)

# Bring in total load data
source("YB_Mass_Balance/Loads/Import_Total_Load_Data.R")

# Prepare total load data for figure
loads_total_out <- loads_total %>% 
  # Filter data to only include outlet loads, 2017 data, and necessary parameters
  filter(
    Year == 2017,
    str_detect(Analyte, "^MeHg|^THg|OC$|^TSS"),
    LocType == "Outlet"
  ) %>% 
  # Rename analytes
  rename_analytes() %>% 
  # Convert variables to factor to apply plot order
  conv_fact_analytes() %>% 
  conv_fact_samplingevent() %>% 
  # Round total loads to proper number of significant figures
  mutate(total_load = signif(total_load, digits))

# Create Figure
figure <- loads_total_out %>% 
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

# Export Figure
ggsave(
  paste0("Ch3_final_report_fig3-", fig_num, ".jpg"), 
  plot = figure,
  dpi = 300,
  width = 6.5,
  height = 6.5,
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])
  

# Figure 3-13 -------------------------------------------------------------
# Same as Figure B-20 in Technical Appendix


# Figure 3-14 -------------------------------------------------------------
# Same as Figure B-21 in Technical Appendix


# Figure 3-15 -------------------------------------------------------------
# Same as Figure B-22 in Technical Appendix


# Figure 3-16 -------------------------------------------------------------
# Seven scatterplots showing net loads for the Upper reach as a function of total Bypass inflow
# The top row of three figures shows the plots for each fraction of Hg
# The middle row of three figures shows the plots for each fraction of MeHg
# The bottom row shows the plot for TSS
# 2017 sampling events only

# Define figure number for easier updating
fig_num <- as.character(16)

# Bring in net load data
source("YB_Mass_Balance/Loads/Import_Net_Load_Data.R")

# Prepare flow data to join with net load data
total_inflows <- daily_flow_data_se %>% 
  # Only include inlet flows
  filter(LocType == "Inlet") %>% 
  # Group by and sum flow data
  group_by(SamplingEvent) %>% 
  summarize(total_inflow = sum(Flow)) %>% 
  ungroup()

# Prepare net load and flow data for plotting
net_loads_flow <- loads_net %>% 
  # Filter data
  filter(
    str_detect(Analyte, "Hg|TSS"),
    Reach == "Upper",
    Year == 2017
  ) %>%
  # Join flow data
  left_join(total_inflows) %>% 
  # Rename analytes and convert variable to factor to apply plot order
  mutate(
    Analyte = recode_factor(
      Analyte,
      "THg- total" = "Unfiltered Hg",
      "THg- filtered" = "Filter-passing Hg",
      "THg- particulate" = "Particulate Hg",
      "MeHg- total" = "Unfiltered MeHg",
      "MeHg- filtered" = "Filter-passing MeHg",
      "MeHg- particulate" = "Particulate MeHg",
      TSS = "TSS"
    ),
    # Change LoadUnits for TSS to Mg/day
    LoadUnits = if_else(Analyte == "TSS", "Mg/day", LoadUnits)
  ) %>% 
  select(Analyte, LoadUnits, net_load, total_inflow)

# Create plot function for the Net Load vs Flow scatterplots
plot_net_load_flow <- function(df, param, rsq, pval) {
  # Create base plot
  p <- 
    ggplot(
      data = df,
      aes(
        x = total_inflow,
        y = net_load
      )
    ) +
    geom_point() +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE
    ) +
    labs(
      title = param,
      subtitle = paste0("R Squared = ", rsq, "%\np-value = ", pval)
    ) +
    scale_x_continuous(labels = label_comma()) +
    scale_y_continuous(labels = label_comma()) +
    theme_owhg()
  
  # Format plots differently based upon param
    # Only add y-axis labels to first column of plots (uHg, uMeHg, and TSS)
    if (str_detect(param, "^Unf|^TSS")) {
      p <- p + ylab(paste0("Net Load\n(", df$LoadUnits[1], ")")) 
    } else {
      p <- p + ylab(NULL)
    }

    # Only add x-axis labels to bottom plots (TSS, fMeHg, and pMeHg)
    if (param %in% c("Filter-passing MeHg", "Particulate MeHg", "TSS")) {
      p <- p + xlab("Total Inflow (cfs)")
    } else {
      p <- p + xlab(NULL)
    }
  
  return(p)
}

# Create scatterplots for each Analyte
net_loads_flow_plots <- net_loads_flow %>% 
  group_nest(Analyte) %>% 
  # Run regression analysis and create plots
  mutate(
    model = map(data, ~summary(lm(net_load ~ total_inflow, data = .x))),
    r2 = signif(map_dbl(model, ~glance(.x)$r.squared * 100), 3),
    p_value = signif(map_dbl(model, ~glance(.x)$p.value), 2),
    plot = pmap(
      list(data, Analyte, r2, p_value),
      .f = plot_net_load_flow
    )
  )

# Group scatterplots together into one figure
figure_group <- net_loads_flow_plots %>% 
  pull(plot) %>% 
  wrap_plots()

# Export figure
ggsave(
  paste0("Ch3_final_report_fig3-", fig_num, ".jpg"),
  plot = figure_group,
  dpi = 300,
  width = 9, 
  height = 5.75, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figure 3-17 -------------------------------------------------------------
# Same as Figure B-26 in Technical Appendix


# Figure 3-19 -------------------------------------------------------------
# Bar plots showing net loads of the upper reach and the entire Yolo Bypass
# Upper reach is between the inlets to the Stairsteps
# Entire Bypass is between the inlets and Below Liberty Island
# Facets for each analyte/parameter
# 2017 sampling events only

# Define figure number for easier updating
fig_num <- as.character(19)

# Bring in net load data
source("YB_Mass_Balance/Loads/Import_Net_Load_Data.R")

# Prepare net load data for figure
loads_net_clean <- loads_net %>% 
  # Filter load data to only include the upper and entire reaches,
  # 2017 data, and necessary parameters
  filter(
    Year == 2017,
    str_detect(Analyte, "^MeHg|^THg|OC$|^TSS"),
    Reach %in% c("Entire", "Upper")
  ) %>% 
  # Rename analytes
  rename_analytes() %>% 
  # Rename Reaches
  mutate(
    Reach = case_when(
      Reach == "Entire" ~ "Inlets to Below Liberty Island", 
      Reach == "Upper" ~ "Inlets to Stairsteps"
    ),
    # Convert Reach to a factor to apply plot order
    Reach = factor(Reach, levels = c("Inlets to Stairsteps", "Inlets to Below Liberty Island"))
  ) %>% 
  # Convert SamplingEvent and Analyte variables to factor to apply plot order
  conv_fact_analytes() %>% 
  conv_fact_samplingevent() %>% 
  # Round net loads to proper number of significant figures
  mutate(net_load = signif(net_load, digits))

# Create Figure
figure <- loads_net_clean %>% 
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

# Export Figure
ggsave(
  paste0("Ch3_final_report_fig3-", fig_num, ".jpg"), 
  plot = figure,
  dpi = 300,
  width = 6.3,
  height = 6.3,
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])

