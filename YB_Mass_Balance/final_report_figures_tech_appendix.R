# Yolo Bypass Mass Balance Study
# Purpose: Figures for Final Report - Technical Appendix
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(readxl)
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
        str_detect(Analyte, "OC$|SS$") ~ paste0(Analyte, " (", LoadUnits, ")"),
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
    "TOC (1,000 kg/day)",
    "DOC (1,000 kg/day)",
    "POC (1,000 kg/day)",
    "TSS (1,000 kg/day)",
    "VSS (1,000 kg/day)"
  )
  
  df <- df %>% mutate(Analyte = factor(Analyte, levels = analytes_order))
  
  return(df)
}

# Create a function to format ggplot titles as column titles to be used with patchwork
format_col_titles <- function(h_just = 0.5, title_size = 14, b_margin = 15) {
  theme(
    plot.title = element_text(
      hjust = h_just,  #Default to center column titles
      size = title_size,  #Make column titles slightly larger
      face = "bold",  #Make column titles bold
      margin = margin(b = b_margin)  #Add more margin space below column titles
    )
  )  
}

obj_keep <- c("obj_keep", "rename_analytes", "conv_fact_analytes", "format_col_titles")


# Figure B-4 --------------------------------------------------------------
# Two barplots arranged vertically that show the inlet flows to the Yolo Bypass 
# The top figure shows the inlet flows for the 5 inlet sources 
# The bottom figure shows the percentage of total inflow for each inlet
# All sampling events

# Define figure number for easier updating
fig_num <- as.character(4)

# Bring in daily flow data for the inlets for sampling events only
source("YB_Mass_Balance/Flows/Import_Inlet_Flow_Data_SE.R")

# Prepare data for plotting
se_flows_clean <- flows_inlet_se %>% 
  # convert variables to factors to apply plot order
  conv_fact_inlet_names() %>% 
  conv_fact_samplingevent() %>% 
  select(SamplingEvent, StationName, Flow)

# Create Figure
  # Top Figure
  figure_top <- se_flows_clean %>% 
    # Round flow values to 4 significant figures before plotting
    mutate(Flow = signif(Flow, 4)) %>% 
    ggplot(aes(x = SamplingEvent, y = Flow, fill = StationName)) +
    geom_col() +
    xlab(NULL) +
    add_inlet_color_pal("fill", legend_title = "Inlet") +
    theme_owhg() +
    theme(axis.text.x = element_blank()) +
    scale_y_continuous(
      name = "Daily Average Flow (cfs)",
      labels = comma_format(),
      expand = expansion(mult = 0.02)
    )

  # Bottom Figure
  figure_bottom <- se_flows_clean %>%
    ggplot(aes(x = SamplingEvent, y = Flow, fill = StationName)) +
    geom_col(position = "fill") +
    xlab(NULL) +
    add_inlet_color_pal("fill", legend_title = "Inlet") +
    theme_owhg(x_axis_v = TRUE) +
    scale_y_continuous(
      name = "Percentage of Total Inflow",
      labels = percent_format(),
      expand = expansion(mult = 0.02)
    )

  # Combine top and bottom figures
  figure <- figure_top / figure_bottom + plot_layout(guides = "collect")

# Export Figure
ggsave(
  paste0("final_report_fig_b-", fig_num, ".jpg"),
  plot = figure,
  dpi = 300,
  width = 6.5, 
  height = 7.5, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figures B-5 through B-7 -------------------------------------------------
# Line plots showing daily average flow in cfs for each inlet
# Line plot showing daily average stage in feet for Lisbon
# Red markers indicate the sampling events
# B-5 is for 2014 flood, B-6 is for 2016 flood, B-7 is for 2017 flood

# Define figure numbers for easier updating
fig_num_2014 <- as.character(5)
fig_num_2016 <- as.character(6)
fig_num_2017 <- as.character(7)

# Bring in daily flow data for the inlets
source("YB_Mass_Balance/Flows/Import_Inlet_Flow_Data_all.R")

# Import Lisbon stage data
  # Data is on SharePoint site for the Open Water Final Report
  # Define path on SharePoint site for data
  sharepoint_path <- normalizePath(
    file.path(
      Sys.getenv("USERPROFILE"),
      "California Department of Water Resources/DWR Documents - Open Water Final Report - Documents/Technical Appendices/Technical Appendix-B_Inlet-Outlet/Data/Raw"
    )
  )
  
  lis_col_names <- c("Datetime", "Date", "Stage")

  lis14 <- read_excel(
    path = paste0(sharepoint_path, "/2014_YB_Flood_Flows.xlsx"),
    range = "Lisbon!A2:C2977",
    col_names = lis_col_names
  )

  lis16 <- read_excel(
    path = paste0(sharepoint_path, "/2016_YB_Flood_Flows.xlsx"),
    range = "Lisbon!A2:C2977",
    col_names = lis_col_names
  )
  
  lis17 <- read_excel(
    path = paste0(sharepoint_path, "/2017_YB_Flood_Flows.xlsx"),
    range = "Lisbon!A3004:C15195",
    col_names = lis_col_names
  )

# Prepare data for plotting
all_flows_clean <- flows_inlet_all %>% 
  filter(Date >= "2014-12-10") %>% 
  filter(!(Date >= "2014-12-31" & Date < "2016-03-08")) %>% 
  filter(Date < "2017-05-05") %>% 
  select(-LocType)

lis_stage_clean <- bind_rows(lis14, lis16, lis17) %>% 
  mutate(
    Datetime = force_tz(Datetime, tzone = "Etc/GMT+8"),
    Date = as_date(Date),
    Year = year(Date)
  ) %>% 
  filter(Date >= "2014-12-10") %>% 
  filter(!(Date > "2014-12-31" & Date < "2016-03-08"))

# Create a df of sampling events to mark these on the hydrographs of the inlets
se_dates <- tibble(
  Date = as_date(
    c("2014-12-22",
      "2016-03-15",
      "2017-01-11",
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

# Join se_dates df to all_flow_clean to pull out data for just the sampling events
se_flows <- inner_join(all_flows_clean, se_dates) %>% 
  filter(Flow != 0)

# Create a df of sampling events to mark these on the Lisbon hydrograph
se_datetimes <- tibble(
  Datetime = as_datetime(
    c("2014-12-22 12:00:00",
      "2016-03-15 12:00:00",
      "2017-01-11 12:00:00",
      "2017-01-24 12:00:00",
      "2017-01-31 12:00:00",
      "2017-02-14 12:00:00",
      "2017-03-01 12:00:00",
      "2017-03-15 12:00:00",
      "2017-03-28 12:00:00",
      "2017-04-11 12:00:00",
      "2017-04-25 12:00:00"
    ), 
    tz = "Etc/GMT+8"
  )
)

# Join se_datetimes df to lis_stage_clean to pull out data for just the sampling events
se_lis_stage <- inner_join(lis_stage_clean, se_datetimes)

# Create plot functions for hydrographs
  # Inlets
  plot_hydro_inlet <- function(df, df_point, PlotTitle, num_breaks) {
    p <- 
      ggplot(
        data = df,
        aes(
          x = Date,
          y = Flow
        )
      ) +
      geom_line() +
      geom_point(
        data = df_point, 
        color = "red",
        size = 2
      ) +
      ggtitle(PlotTitle) +
      scale_x_date(
        name = NULL,
        breaks = breaks_pretty(num_breaks),
        labels = label_date_short(),
        expand = expansion(mult = 0)  #Remove extra space on ends of x-axis
      ) +
      scale_y_continuous(
        name = "Daily Average Flow (cfs)",
        labels = label_comma(),
        limits = c(0, NA)  #Force y-axis limit to start at zero
      ) +
      theme_owhg()
    
    return(p)
  }
  
  # Lisbon
  plot_hydro_lis <- function(df, df_point, num_breaks) {
    p <- 
      ggplot(
        data = df,
        aes(
          x = Datetime,
          y = Stage
        )
      ) +
      geom_line() +
      geom_point(
        data = df_point, 
        color = "red",
        size = 2
      ) +
      ggtitle("Toe Drain at Lisbon Weir") +
      ylab("Stage (ft)") +
      scale_x_datetime(
        name = NULL,
        breaks = breaks_pretty(num_breaks),
        labels = label_date_short(),
        expand = expansion(mult = 0)  #Remove extra space on ends of x-axis
      ) +
      theme_owhg()
    
    return(p)
  }

# Create nested df's
  # Inlets
  all_flows_clean_n <- all_flows_clean %>% 
    group_nest(Year, StationName)
  
  se_flows_n <- se_flows %>% 
    group_nest(Year, StationName) %>% 
    rename(data_se = data)
  
  # Lisbon
  lis_stage_clean_n <- lis_stage_clean %>% 
    group_nest(Year)
  
  se_lis_stage_n <- se_lis_stage %>% 
    group_nest(Year) %>% 
    rename(data_se = data)
  
# Join nested df's and run functions to create hydrographs
  # Inlets
  hydrograph_inlets <- left_join(all_flows_clean_n, se_flows_n) %>% 
    mutate(
      plot = pmap(
        list(data, data_se, StationName), 
        .f = plot_hydro_inlet, 
        num_breaks = 10
      )
    )
  
  # Lisbon
  hydrograph_lis <- left_join(lis_stage_clean_n, se_lis_stage_n) %>% 
    mutate(
      plot = map2(
        data, 
        data_se, 
        .f = plot_hydro_lis, 
        num_breaks = 10
      ),
      StationName = "Lisbon"
    )
  
# Combine nested df's with hydrographs together
hydrograph_all <- bind_rows(hydrograph_inlets, hydrograph_lis)
  
# Group hydrographs together by year
hydrograph_group <- hydrograph_all %>%   
  select(-c(data, data_se)) %>% 
  # apply custom order
  mutate(
    StationName = factor(
      StationName, 
      levels = c(
        "Lisbon",
        "Fremont Weir",
        "KLRC",
        "CCSB",
        "Putah Creek",
        "Sacramento Weir"
      )
    )
  ) %>% 
  arrange(StationName) %>% 
  group_nest(Year) %>% 
  mutate(
    group_plots = map(
      data, 
      .f = ~pull(.x, plot) %>% 
        wrap_plots(ncol = 2)
    )
  )

# Export Figures
  # 2014
  ggsave(
    paste0("final_report_fig_b-", fig_num_2014, ".jpg"),
    plot = hydrograph_group$group_plots[[1]],
    dpi = 300,
    width = 7, 
    height = 6.5, 
    units = "in"
  )
  
  # 2016
  ggsave(
    paste0("final_report_fig_b-", fig_num_2016, ".jpg"),
    plot = hydrograph_group$group_plots[[2]],
    dpi = 300,
    width = 7, 
    height = 8.75, 
    units = "in"
  )
  
  # 2017
  ggsave(
    paste0("final_report_fig_b-", fig_num_2017, ".jpg"),
    plot = hydrograph_group$group_plots[[3]],
    dpi = 300,
    width = 7, 
    height = 8.75, 
    units = "in"
  )

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figure B-8 --------------------------------------------------------------
# Six area plots showing inlet flows to the Yolo Bypass during the 2014, 2016, and 2017 flood events
# The top row of three figures shows the flows for the 5 inlet sources for the 3 flood years
# The bottom row of three figures shows the percentage of total inflow for each inlet for the 3 flood years

# Define figure number for easier updating
fig_num <- as.character(8)

# Bring in daily flow data for the inlets
source("YB_Mass_Balance/Flows/Import_Inlet_Flow_Data_all.R")

# Prepare data for plotting
all_flows_clean <- flows_inlet_all %>% 
  filter(Date >= "2014-12-10") %>% 
  filter(!(Date >= "2014-12-31" & Date < "2016-03-08")) %>% 
  filter(Date < "2017-05-05") %>% 
  select(-LocType) %>%
  conv_fact_inlet_names()

# Create plot function for area plots of inflow data
plot_area_inlet <- function(df, PlotTitle, PlotType) {
  # Create base plot
  p <- 
    ggplot(
      data = df,
      aes(
        x = Date,
        y = Flow,
        fill = StationName
      )
    ) +
    scale_x_date(
      name = NULL,
      breaks = breaks_pretty(10),
      labels = label_date_short(),
      expand = expansion(mult = 0)  #Remove extra space on ends of x-axis
    ) +
    theme_owhg() +
    format_col_titles(b_margin = 10) +
    add_inlet_color_pal("fill", legend_title = "Inlet")
  
  # Apply different formatting based upon PlotType
  if (PlotType == "stacked") {
    p <- p +
      geom_area() +
      ggtitle(PlotTitle) +  #Only create plot titles for the top row of plots
      scale_y_continuous(
        labels = label_comma(),
        expand = expansion(mult = 0.02)  #Reduce extra space on ends of y-axis
      )
  } else if (PlotType == "filled") {
    p <- p +
      geom_area(position = "fill") +
      scale_y_continuous(
        labels = label_percent(),
        expand = expansion(mult = 0)  #Remove extra space on ends of y-axis
      )
  }
  
  # Only keep y-axis label for first column of plots (2014)
  if (PlotTitle == 2014) {
    # Different labels based on PlotType
    if (PlotType == "stacked") {
      p <- p + ylab("Daily Average Flow (cfs)")
    } else if (PlotType == "filled") {
      p <- p + ylab("Percent of total Inlet flow")
    }
  } else {
    p <- p + ylab(NULL)
  }
  
  # Remove legends from all plots except for the 2017 plots- for patchwork to collect guides correctly
  if (PlotTitle != 2017) {
    p <- p + guides(fill = "none")
  }
  
  return(p)
}

# Create area plots for each year
inlet_area_plots <- all_flows_clean %>% 
  group_nest(Year) %>% 
  mutate(
    plot_stack = map2(
      data, 
      Year, 
      .f = plot_area_inlet, 
      PlotType = "stacked"
    ),
    plot_fill = map2(
      data,
      Year,
      .f = plot_area_inlet,
      PlotType = "filled"
    )
  ) %>% 
  # rearrange df for more efficient grouping of plots
  select(-data) %>% 
  pivot_longer(
    cols = -Year,
    names_to = "type",
    values_to = "plot"
  ) %>% 
  arrange(desc(type), Year)

# Group area plots together
figure <- inlet_area_plots %>% 
  pull(plot) %>% 
  wrap_plots() + plot_layout(guides = "collect")

# Export Figure
ggsave(
  paste0("final_report_fig_b-", fig_num, ".jpg"),
  plot = figure,
  dpi = 300,
  width = 9.5, 
  height = 6.25, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figure B-9 --------------------------------------------------------------
# Six scatterplots showing MeHg concentrations as a function of tributary inflow
# The top row of three figures shows the plots for each MeHg fraction for the average of the three
  # Fremont Weir stations
# The bottom row of three figures shows the plots for each MeHg fraction for the average of 
  # the two CCSB Overflow Weir stations
# 2017 sampling events only

# Define figure number for easier updating
fig_num <- as.character(9)

# Bring in concentration data
source("YB_Mass_Balance/Concentrations/Import_Conc_Data.R")

# Prepare data for plotting
flow_fw_ccsb <- daily_flow_data_all %>% 
  # Prepare daily flow data to be joined with the concentration data
  filter(str_detect(StationName, "^CCSB- O|^Frem")) %>% 
  mutate(
    StationName = if_else(
      str_detect(StationName, "^CCSB"),
      "CCSB Overflow Weir",
      "Fremont Weir"
    )
  ) %>% 
  select(Date, StationName, Flow)

mehg_conc_flow_fw_ccsb <- all_conc %>% 
  # Only keep MeHg data for CCSB and Fremont Weir for 2017
  filter(
    str_detect(StationName, "^CCSB O|^Frem"),
    str_detect(Analyte, "^MeHg"),
    year(SampleDate) == 2017
  ) %>% 
  select(StationName, SampleDate, Analyte, Conc) %>% 
  # Average concentrations for CCSB and Fremont Weir stations
  mutate(
    StationName = if_else(
      str_detect(StationName, "^CCSB"),
      "CCSB Overflow Weir",
      "Fremont Weir"
    )
  ) %>% 
  group_by(StationName, SampleDate, Analyte) %>% 
  summarize(avg_conc = mean(Conc)) %>% 
  ungroup() %>% 
  # Join flow data
  left_join(flow_fw_ccsb, by = c("SampleDate" = "Date", "StationName")) %>% 
  # Change analyte names and apply plotting order
  mutate(
    Analyte = case_when(
      Analyte == "MeHg- filtered" ~ "MeHg- filter-passing",
      Analyte == "MeHg- total" ~ "MeHg- unfiltered",
      TRUE ~ Analyte
    ),
    Analyte = factor(Analyte, levels = c("MeHg- unfiltered", "MeHg- filter-passing", "MeHg- particulate"))
  ) %>% 
  arrange(Analyte, SampleDate, StationName)

# Create plot function for the Conc vs Flow scatterplots
plot_conc_flow <- function(df, station, param, rsq, pval) {
  # Create base plot
  p <- 
    ggplot(
      data = df,
      aes(
        x = Flow,
        y = avg_conc
      )
    ) +
    geom_point() +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE
    ) +
    labs(subtitle = paste0("R Squared = ", rsq, "%\np-value = ", pval)) +
    scale_x_continuous(labels = label_comma()) +
    theme_owhg() +
    format_col_titles()
  
  # Format plots differently based upon station
  if (station == "Fremont Weir") {
    p <- p +
      ggtitle(param) +  #Only add title of param to top row of plots
      xlab(NULL)
  } else {
    p <- p + xlab("Daily Average Inflow (cfs)")  #Only add x-axis labels to bottom row of plots
  }
  
  # Only keep y-axis label for first column of plots (MeHg - unfiltered)
  if (param == "MeHg- unfiltered") {
    p <- p + ylab(paste0(station, "\nConcentration (ng/L)"))
  } else {
    p <- p + ylab(NULL)
  }
  
  return(p)
}

# Create scatterplots for each Station-Analyte combination
fw_ccsb_scattplots <- mehg_conc_flow_fw_ccsb %>% 
  group_nest(StationName, Analyte) %>% 
  # Run regression analysis and create plots
  mutate(
    model = map(data, ~summary(lm(avg_conc ~ Flow, data = .x))),
    r2 = signif(map_dbl(model, ~glance(.x)$r.squared * 100), 3),
    p_value = signif(map_dbl(model, ~glance(.x)$p.value), 2),
    plot = pmap(
      list(data, StationName, Analyte, r2, p_value),
      .f = plot_conc_flow
    )
  ) %>% 
  # Rearrange dataframe for proper order of plots
  arrange(desc(StationName), Analyte)

# Group scatterplots together
figure <- fw_ccsb_scattplots %>% 
  pull(plot) %>% 
  wrap_plots()

# Export Figure
ggsave(
  paste0("final_report_fig_b-", fig_num, ".jpg"),
  plot = figure,
  dpi = 300,
  width = 9.5, 
  height = 5.75, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figure B-10 -------------------------------------------------------------
# Boxplots showing total inlet, outlet at the Stairsteps, and Below Liberty Island loads
# Facets for each analyte/parameter
# Just sampling events collected in 2017
# Add points to boxplots with jitter, make point representing first event in 2017 a different color (red)

# Define figure number for easier updating
fig_num <- as.character(10)

# Bring in total load data
source("YB_Mass_Balance/Loads/Import_Total_Load_Data.R")

# Prepare data for plotting
loads_total_clean <- loads_total %>% 
  # Filter and rename analytes
  rename_analytes() %>% 
  filter(
    !is.na(Analyte),
    Year == 2017
  ) %>% 
  # Apply plotting order
  conv_fact_analytes() %>% 
  mutate(
    LocType = if_else(
      LocType == "Below Liberty",
      "BLI",
      LocType
    ),
    LocType = factor(LocType, levels = c("Inlet", "Outlet", "BLI"))
  ) %>% 
  # Round total loads to proper number of significant figures
  mutate(total_load = signif(total_load, digits)) %>% 
  # Create new variable to indicate first sampling event in 2017
  mutate(
    se_type = if_else(
      SamplingEvent == "Jan 11-12, 2017",
      "First sampling event",
      "All other sampling events"
    ),
    se_type = factor(se_type, levels = c("First sampling event", "All other sampling events"))
  ) %>% 
  select(LocType, Analyte, se_type, total_load)

# Create Figure
figure <- loads_total_clean %>% 
  ggplot(aes(x = LocType, y = total_load)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    width = 0.25,
    aes(color = se_type)
  ) +
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
  add_gen_color_pal(num_colors = 2, "color") +
  theme_owhg(x_axis_v = TRUE) +
  theme(
    legend.margin = margin(0, 0, 0, 0),
    legend.position = c(0.82, 0.12)
  )

# Export Figure
ggsave(
  paste0("final_report_fig_b-", fig_num, ".jpg"),
  plot = figure,
  dpi = 300,
  width = 7, 
  height = 8.25, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figures B-11 and B-12 -------------------------------------------------------------
# Barplots showing total inlet, outlet at the Stairsteps, and Below Liberty Island loads
# Facets for each analyte/parameter
# All sampling events
# B-11 is for MeHg and THg, B-12 is for organic carbon and suspended solids

# Define figure numbers for easier updating
fig_num_hg <- as.character(11)
fig_num_other <- as.character(12)

# Bring in total load data
source("YB_Mass_Balance/Loads/Import_Total_Load_Data.R")

# Prepare data for plotting
loads_total_clean <- loads_total %>% 
  # Filter and rename analytes
  rename_analytes() %>% 
  filter(!is.na(Analyte)) %>% 
  # Apply plotting order
  conv_fact_analytes() %>% 
  conv_fact_samplingevent() %>% 
  mutate(
    LocType = case_when(
      LocType == "Outlet" ~ "Outlet at Stairsteps",
      LocType == "Below Liberty" ~ "Below Liberty Island",
      TRUE ~ LocType
    ),
    LocType = factor(LocType, levels = c("Inlet", "Outlet at Stairsteps", "Below Liberty Island"))
  ) %>% 
  # Round total loads to proper number of significant figures
  mutate(total_load = signif(total_load, digits))

# Create plot functions for barplots
plot_total_loads <- function(df) {
  p <- 
    ggplot(
      data = df,
      aes(
        x = SamplingEvent, 
        y = total_load, 
        fill = LocType
      )
    ) +
    geom_col(position = position_dodge2(preserve = "single")) +
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
    add_gen_color_pal(num_colors = 3, "fill") +
    theme_owhg(x_axis_v = TRUE) +
    theme(legend.position = "bottom")
  
  return(p)
}

# Create a list with the data for the two figures and run function to create barplots of total loads
loads_total_plots <- 
  list(
    "hg" = filter(loads_total_clean, str_detect(Analyte, "Hg")),
    "other" = filter(loads_total_clean, !str_detect(Analyte, "Hg"))
  ) %>% 
  map(.f = plot_total_loads)

# Export Figure of THg and MeHg data
ggsave(
  paste0("final_report_fig_b-", fig_num_hg, ".jpg"),
  plot = loads_total_plots$hg,
  dpi = 300,
  width = 9.5, 
  height = 6.25, 
  units = "in"
)

# Export Figure of organic carbon and suspended solids data
ggsave(
  paste0("final_report_fig_b-", fig_num_other, ".jpg"),
  plot = loads_total_plots$other,
  dpi = 300,
  width = 9.5, 
  height = 6.25, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figure B-13 -------------------------------------------------------------
# Barplots showing loads for each individual inlet stacked on top of each other for each event
# Facets for each analyte/parameter
# All sampling events

# Define figure number for easier updating
fig_num <- as.character(13)

# Bring in inlet load data
source("YB_Mass_Balance/Loads/Import_Inlet_Load_Data.R")

# Prepare data for plotting
loads_inlet_clean <- loads_inlet %>% 
  # Remove added zeros for events when the weirs weren't spilling
  anti_join(zero_loads) %>% 
  # Filter and rename analytes
  rename_analytes() %>% 
  filter(!is.na(Analyte)) %>% 
  # Apply plotting order
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
    legend.position = c(0.84, 0)
  ) +
  guides(fill = guide_legend(keyheight = 0.95))

# Export Figure
ggsave(
  paste0("final_report_fig_b-", fig_num, ".jpg"),
  plot = figure,
  dpi = 300,
  width = 7, 
  height = 8.75, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figure B-14 -------------------------------------------------------------
# Barplots of average loads of each inlet with error bars indicating their standard deviations
# Facets for each analyte/parameter
# Means and standard deviations only include sampling events collected in 2017

# Define figure number for easier updating
fig_num <- as.character(14)

# Bring in inlet load data
source("YB_Mass_Balance/Loads/Import_Inlet_Load_Data.R")

# Prepare data for plotting
loads_inlet_clean <- loads_inlet %>% 
  # Filter and rename analytes
  rename_analytes() %>% 
  filter(
    !is.na(Analyte),
    Year == 2017
  ) %>% 
  # Apply plotting order
  conv_fact_analytes() %>% 
  conv_fact_inlet_names() %>% 
  # Calculate averages and standard deviations of loads for each inlet
  group_by(StationName, Analyte) %>% 
  summarize(
    sign_digits = min(digits, na.rm = TRUE),
    Mean = signif(mean(Load), sign_digits),
    StDev = signif(sd(Load), sign_digits)
  ) %>% 
  ungroup()

# Create Figure
figure <- loads_inlet_clean %>% 
  ggplot(aes(x = StationName, y = Mean)) +
  geom_col(aes(fill = StationName)) +
  geom_errorbar(
    aes(
      ymin = Mean - StDev, 
      ymax = Mean + StDev
    ),
    width = 0.25
  ) +
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
  guides(fill = "none")

# Export Figure
ggsave(
  paste0("final_report_fig_b-", fig_num, ".jpg"),
  plot = figure,
  dpi = 300,
  width = 7, 
  height = 8.25, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figure B-15 -------------------------------------------------------------
# Filled barplots showing the percentage of the total inlet load for each individual inlet
# Facets for each analyte/parameter
# All sampling events

# Define figure number for easier updating
fig_num <- as.character(15)

# Bring in inlet load data
source("YB_Mass_Balance/Loads/Import_Inlet_Load_Data.R")

# Prepare data for plotting
loads_inlet_clean <- loads_inlet %>%
  # Remove added zeros for events when the weirs weren't spilling
  anti_join(zero_loads) %>% 
  # Filter and rename analytes
  mutate(
    Analyte = case_when(
      str_detect(Analyte, "OC$|SS$") ~ Analyte,
      Analyte == "MeHg- filtered" ~ "fMeHg",
      Analyte == "MeHg- particulate" ~ "pMeHg",
      Analyte == "MeHg- total" ~ "uMeHg",
      Analyte == "THg- filtered" ~ "fHg",
      Analyte == "THg- particulate" ~ "pHg",
      Analyte == "THg- total" ~ "uHg"
    )
  ) %>% 
  filter(!is.na(Analyte)) %>% 
  # Apply plotting order
  conv_fact_samplingevent() %>% 
  conv_fact_inlet_names() %>% 
  mutate(
    Analyte = factor(
      Analyte,
      levels = c(
        "uHg",
        "fHg",
        "pHg",
        "uMeHg",
        "fMeHg",
        "pMeHg",
        "TOC",
        "DOC",
        "POC",
        "TSS",
        "VSS"
      )
    )
  ) %>% 
  select(SamplingEvent, StationName, Analyte, Load)

# Create Figure
figure <- loads_inlet_clean %>% 
  ggplot(aes(x = SamplingEvent, y = Load, fill = StationName)) +
  geom_col(position = "fill") +
  facet_wrap(
    vars(Analyte),
    ncol = 3
  ) +
  scale_y_continuous(
    name = "Percentage of Total Inlet Load",
    labels = percent_format()
  ) +
  xlab(NULL) +
  add_inlet_color_pal("fill") +
  theme_owhg(x_axis_v = TRUE) +
  theme(
    legend.margin = margin(0, 0, 0, 0),
    legend.position = c(0.84, 0)
  ) +
  guides(fill = guide_legend(keyheight = 0.95))

# Export Figure
ggsave(
  paste0("final_report_fig_b-", fig_num, ".jpg"),
  plot = figure,
  dpi = 300,
  width = 7, 
  height = 8.75, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figure B-16 -------------------------------------------------------------
# Barplots showing the percent of the total Hg that was in the MeHg fraction
# Facetted horizontally by each inlet source and vertically by Hg fraction
# All sampling events

# Define figure number for easier updating
fig_num <- as.character(16)

# Bring in inlet load data
source("YB_Mass_Balance/Loads/Import_Inlet_Load_Data.R")

# Prepare data for plotting
loads_inlet_perc_mehg <- loads_inlet %>% 
  # Remove added zeros for events when the weirs weren't spilling
  anti_join(zero_loads) %>% 
  # Calculate MeHg/THg ratios
  filter(str_detect(Analyte, "Hg")) %>%
  separate(Analyte, into = c("Analyte", "Fraction")) %>% 
  pivot_wider(
    id_cols = c(SamplingEvent, StationName, Fraction),
    names_from = Analyte, 
    values_from = Load
  ) %>% 
  mutate(per_mehg = MeHg/THg) %>% 
  select(-c(MeHg, THg)) %>% 
  # Change analyte names and apply plotting order
  mutate(
    Fraction = case_when(
      Fraction == "total" ~ "Unfiltered",
      Fraction == "filtered" ~ "Filter-passing",
      TRUE ~ str_to_title(Fraction)
    ),
    Fraction = factor(Fraction, levels = c("Unfiltered", "Filter-passing", "Particulate"))
  ) %>% 
  conv_fact_samplingevent() %>% 
  conv_fact_inlet_names() 

# Create Figure
figure <- loads_inlet_perc_mehg %>% 
  ggplot(aes(x = SamplingEvent, y = per_mehg)) +
  geom_col() +
  facet_grid(
    rows = vars(StationName),
    cols = vars(Fraction)
  ) +
  scale_y_continuous(
    name = "Percent MeHg of the total Hg",
    labels = percent_format()
  ) +
  xlab(NULL) +
  theme_owhg(x_axis_v = TRUE)

# Export Figure
ggsave(
  paste0("final_report_fig_b-", fig_num, ".jpg"),
  plot = figure,
  dpi = 300,
  width = 7, 
  height = 8.75, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figure B-17 -------------------------------------------------------------
# Filled barplots showing the partitioning of Hg and MeHg into filter-passing and particulate forms
# Facetted horizontally by each inlet source and vertically by parameter (Hg and MeHg)
# All sampling events

# Define figure number for easier updating
fig_num <- as.character(17)

# Bring in inlet load data
source("YB_Mass_Balance/Loads/Import_Inlet_Load_Data.R")

# Prepare data for plotting
loads_inlet_perc_frac <- loads_inlet %>%
  # Remove added zeros for events when the weirs weren't spilling
  anti_join(zero_loads) %>%
  # Restructure Hg and MeHg data to be used with ggplot
  filter(str_detect(Analyte, "Hg")) %>% 
  separate(Analyte, into = c("Analyte", "Fraction")) %>% 
  filter(Fraction != "total") %>% 
  mutate(
    Analyte = if_else(Analyte == "THg", "Mercury", "Methylmercury"),
    Fraction = if_else(Fraction == "filtered", "Filter-passing", str_to_title(Fraction))
  ) %>% 
  # Apply plotting order
  conv_fact_samplingevent() %>% 
  conv_fact_inlet_names() %>%
  select(SamplingEvent, StationName, Analyte, Fraction, Load)

# Create Figure
figure <- loads_inlet_perc_frac %>% 
  ggplot(aes(x = SamplingEvent, y = Load, fill = Fraction)) +
  geom_col(
    color = "gray30",
    position = "fill"
  ) +
  facet_grid(
    rows = vars(StationName),
    cols = vars(Analyte)
  ) +
  scale_y_continuous(
    name = "Percentage of each Fraction",
    labels = percent_format()
  ) +
  xlab(NULL) +
  add_gen_color_pal(2, "fill") +
  theme_owhg(x_axis_v = TRUE)

# Export Figure
ggsave(
  paste0("final_report_fig_b-", fig_num, ".jpg"),
  plot = figure,
  dpi = 300,
  width = 7, 
  height = 8.75, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figures B-18 and B-19  -------------------------------------------------------------
# Barplots showing net internal loads for the Upper and Liberty Island reaches and the entire Yolo Bypass
# Facets for each analyte/parameter
# All sampling events
# B-18 is for MeHg and THg, B-19 is for organic carbon and suspended solids

# Define figure numbers for easier updating
fig_num_hg <- as.character(18)
fig_num_other <- as.character(19)

# Bring in net load data
source("YB_Mass_Balance/Loads/Import_Net_Load_Data.R")

# Prepare data for plotting
loads_net_clean <- loads_net %>% 
  # Filter and rename analytes
  rename_analytes() %>% 
  filter(!is.na(Analyte)) %>% 
  # Apply plotting order
  conv_fact_analytes() %>% 
  conv_fact_samplingevent() %>% 
  mutate(
    Reach = case_when(
      Reach == "Entire" ~ "Entire Yolo Bypass",
      Reach == "Liberty" ~ "Liberty Island Reach",
      Reach == "Upper" ~ "Upper Reach"
    ),
    Reach = factor(Reach, levels = c("Upper Reach", "Liberty Island Reach", "Entire Yolo Bypass"))
  ) %>% 
  # Round net loads to proper number of significant figures
  mutate(net_load = signif(net_load, digits)) %>% 
  select(Reach, SamplingEvent, Analyte, net_load)

# Create plot functions for barplots
plot_net_loads <- function(df) {
  p <- 
    ggplot(
      data = df,
      aes(
        x = SamplingEvent, 
        y = net_load, 
        fill = Reach
      )
    ) +
    geom_col(position = position_dodge2(preserve = "single")) +
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
    add_gen_color_pal(num_colors = 3, "fill") +
    theme_owhg(x_axis_v = TRUE) +
    theme(legend.position = "bottom")
  
  return(p)
}

# Create a list with the data for the two figures and run function to create barplots of net loads
loads_net_plots <- 
  list(
    "hg" = filter(loads_net_clean, str_detect(Analyte, "Hg")),
    "other" = filter(loads_net_clean, !str_detect(Analyte, "Hg"))
  ) %>% 
  map(.f = plot_net_loads)

# Export Figure of THg and MeHg data
ggsave(
  paste0("final_report_fig_b-", fig_num_hg, ".jpg"),
  plot = loads_net_plots$hg,
  dpi = 300,
  width = 9.5, 
  height = 6.25, 
  units = "in"
)

# Export Figure of organic carbon and suspended solids data
ggsave(
  paste0("final_report_fig_b-", fig_num_other, ".jpg"),
  plot = loads_net_plots$other,
  dpi = 300,
  width = 9.5, 
  height = 6.25, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figure B-20 -------------------------------------------------------------
# Barplot showing net fMeHg and pMeHg loads for the Upper reach dodged next to each other
# 2017 sampling events only

# Define figure number for easier updating
fig_num <- as.character(20)

# Bring in net load data
source("YB_Mass_Balance/Loads/Import_Net_Load_Data.R")

# Prepare data for plotting
mehg_net_loads <- loads_net %>% 
  filter(
    str_detect(Analyte, "^MeHg- f|^MeHg- p"),
    Year == 2017,
    Reach == "Upper"
  ) %>% 
  # Apply plotting order
  conv_fact_samplingevent() %>% 
  # Rename Analytes and round net loads to proper number of significant figures
  mutate(
    Analyte = if_else(str_detect(Analyte, "filt"), "Filter-passing", "Particulate"),
    net_load = signif(net_load, digits)
  ) %>% 
  select(SamplingEvent, Analyte, net_load)

# Create Figure
figure <- mehg_net_loads %>% 
  ggplot(aes(x = SamplingEvent, y = net_load, fill = Analyte)) +
  geom_col(position = "dodge") +
  ylab("Net Load (g/day)") +
  xlab(NULL) +
  add_gen_color_pal(num_colors = 2, "fill") +
  theme_owhg(x_axis_v = TRUE)

# Export Figure
ggsave(
  paste0("final_report_fig_b-", fig_num, ".jpg"),
  plot = figure,
  dpi = 300,
  width = 5.5, 
  height = 3.5, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figure B-21 -------------------------------------------------------------
# Barplots showing MeHg concentrations on solids for the three Stairstep locations
# Facet for each Station
# 2017 sampling events only

# Define figure number for easier updating
fig_num <- as.character(21)

# Prepare MeHg concentration on solids data for plotting
mehg_conc_solids_out <- comb_param_calc %>% 
  filter(
    year(SampleDate) == 2017,
    Parameter == "MeHg Concentration on Solids",
    str_detect(StationName, "^Lib|^Shag|Toe.+[n]$")
  ) %>% 
  # Add sampling event variable
  add_samplingevent() %>% 
  # Apply plotting order
  conv_fact_samplingevent() %>% 
  mutate(
    StationName = factor(
      StationName,
      levels = c(
        "Shag Slough below Stairsteps",
        "Liberty Cut below Stairsteps",
        "Toe Drain at 1/2 Lisbon"
      )
    )
  ) %>% 
  select(SamplingEvent, StationName, Value)

# Create Figure
figure <- mehg_conc_solids_out %>% 
  ggplot(aes(x = SamplingEvent, y = Value)) +
  geom_col() +
  facet_wrap(vars(StationName)) +
  ylab("Concentration (ng/g)") +
  xlab(NULL) +
  theme_owhg(x_axis_v = TRUE)

# Export Figure
ggsave(
  paste0("final_report_fig_b-", fig_num, ".jpg"),
  plot = figure,
  dpi = 300,
  width = 6.5, 
  height = 4, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figure B-22 -------------------------------------------------------------
# Barplot showing net VSS loads for the Upper reach
# 2017 sampling events only
# More zoomed in plot of the VSS panel of Figure B-19

# Define figure number for easier updating
fig_num <- as.character(22)

# Bring in net load data
source("YB_Mass_Balance/Loads/Import_Net_Load_Data.R")

# Prepare data for plotting
vss_net_loads <- loads_net %>% 
  filter(
    Analyte == "VSS",
    Year == 2017,
    Reach == "Upper"
  ) %>% 
  # Apply plotting order
  conv_fact_samplingevent() %>% 
  # Round net loads to proper number of significant figures
  mutate(net_load = signif(net_load, digits)) %>% 
  select(SamplingEvent, net_load)

# Create Figure
figure <- vss_net_loads %>% 
  ggplot(aes(x = SamplingEvent, y = net_load)) +
  geom_col() +
  scale_y_continuous(
    name = "Net Load (1,000 kg/day)",
    labels = label_comma()
  ) +
  xlab(NULL) +
  theme_owhg(x_axis_v = TRUE)

# Export Figure
ggsave(
  paste0("final_report_fig_b-", fig_num, ".jpg"),
  plot = figure,
  dpi = 300,
  width = 3.75, 
  height = 3.5, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figure B-23 through B-25 --------------------------------------------------------------
# Each figure has multiple scatterplots showing net loads as a function of total Bypass inflow
# The top row of three figures shows the plots for the unfiltered fraction
# The middle row of three figures shows the plots for the filter-passing fraction
# The bottom row of three figures shows the plots for the particulate fraction
# The columns are ordered from left to right by Upper reach, Liberty Island reach, and entire Bypass
# Figure B-23 is for MeHg 
# Figure B-24 is for Hg 
# Figure B-23 is for TSS (only has three panels)
# 2017 sampling events only

# Define figure numbers for easier updating
fig_num_mehg <- as.character(23)
fig_num_hg <- as.character(24)
fig_num_tss <- as.character(25)

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
    Year == 2017
  ) %>%
  # Join flow data
  left_join(total_inflows) %>% 
  # Apply plot order
  mutate(
    Analyte = factor(
      Analyte,
      levels = c(
        "MeHg- total", 
        "MeHg- filtered", 
        "MeHg- particulate",
        "THg- total", 
        "THg- filtered", 
        "THg- particulate",
        "TSS"
      )
    ),
    Reach = factor(
      Reach, 
      levels = c(
        "Upper", 
        "Liberty",
        "Entire"
      )
    ),
    # Rename Analytes
    Analyte = recode(
      Analyte,
      "MeHg- total" = "Unfiltered MeHg", 
      "MeHg- filtered" = "Filter-passing MeHg", 
      "MeHg- particulate" = "Particulate MeHg",
      "THg- total" = "Unfiltered Hg", 
      "THg- filtered" = "Filter-passing Hg", 
      "THg- particulate" = "Particulate Hg",
      TSS = "TSS"
    ),
    # Rename Reaches
    Reach = recode(
      Reach,
      Upper = "Upper Reach",
      Liberty = "Liberty Island Reach",
      Entire = "Entire Yolo Bypass"
    )
  ) %>% 
  select(-c(Year, SamplingEvent, digits))

# Create plot function for the Net Load vs Flow scatterplots
plot_net_load_flow <- function(df, reach, param, rsq, pval) {
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
    labs(subtitle = paste0("R Squared = ", rsq, "%\np-value = ", pval)) +
    scale_x_continuous(labels = label_comma()) +
    scale_y_continuous(labels = label_comma()) +
    theme_owhg()
  
  # Format plots differently based upon param
  if (param == "TSS") {
    # add title and axis labels to TSS plots
    p <- p +
      ggtitle(reach) +  
      xlab("Total Inflow (cfs)")+
      ylab(paste0("Net Load (", df$LoadUnits[1], ")")) +
      format_col_titles(h_just = 0, b_margin = 11/2)
  } else if (str_detect(param, "^Unf")) {
    # Add titles but no x-axis labels to plots of the unfiltered fraction
    p <- p +
      ggtitle(reach) +
      format_col_titles() +
      xlab(NULL)
  } else if (str_detect(param, "^Part")) {
    # Add x-axis labels to plots of the particulate fraction
    p <- p + xlab("Total Inflow (cfs)")
  } else {
    # Remove x-axis labels from the filter-passing fraction
    p <- p + xlab(NULL)
  }
  
  # Only keep y-axis label for first column of plots (Upper Reach), except for TSS plots
  if (param != "TSS") {
    if (reach == "Upper Reach") {
      p <- p + ylab(paste0(param, "\nNet Load (", df$LoadUnits[1], ")"))
    } else {
      p <- p + ylab(NULL)
    }
  }
  
  return(p)
}

# Create scatterplots for each Reach-Analyte combination
net_loads_flow_plots <- net_loads_flow %>% 
  group_nest(Reach, Analyte) %>% 
  # Run regression analysis and create plots
  mutate(
    model = map(data, ~summary(lm(net_load ~ total_inflow, data = .x))),
    r2 = signif(map_dbl(model, ~glance(.x)$r.squared * 100), 3),
    p_value = signif(map_dbl(model, ~glance(.x)$p.value), 2),
    plot = pmap(
      list(data, Reach, Analyte, r2, p_value),
      .f = plot_net_load_flow
    )
  ) %>% 
  # Rearrange dataframe for proper order of plots
  arrange(Analyte, Reach)

# Group scatterplots together
figures_group <- 
  list(
    "mehg" = filter(net_loads_flow_plots, str_detect(Analyte, "MeHg$")),
    "hg" = filter(net_loads_flow_plots, str_detect(Analyte, " Hg$")),
    "tss" = filter(net_loads_flow_plots, Analyte == "TSS")
  ) %>% 
  map(~pull(.x, plot)) %>% 
  map_at(c("mehg", "hg"), wrap_plots) %>% 
  map_at("tss", ~wrap_plots(.x, ncol = 2))

# Export figures
  # MeHg
  ggsave(
    paste0("final_report_fig_b-", fig_num_mehg, ".jpg"),
    plot = figures_group$mehg,
    dpi = 300,
    width = 9.5, 
    height = 6.25, 
    units = "in"
  )
  
  # Hg
  ggsave(
    paste0("final_report_fig_b-", fig_num_hg, ".jpg"),
    plot = figures_group$hg,
    dpi = 300,
    width = 9.5, 
    height = 6.25, 
    units = "in"
  )
  
  # TSS
  ggsave(
    paste0("final_report_fig_b-", fig_num_tss, ".jpg"),
    plot = figures_group$tss,
    dpi = 300,
    width = 6.5, 
    height = 6, 
    units = "in"
  )

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])


# Figure B-26 -------------------------------------------------------------
# Scatterplots comparing data collected in 2006 by Foe et al. with the data collected from 2014-2017
# Plots show MeHg loads as a function of total Bypass inflow
# Each year has a different color
# Three facets: Inlet loads, Outlet loads at the Stairsteps, and net loads between the two

# Define figure number for easier updating
fig_num <- as.character(26)

# Bring in total and net load data
source("YB_Mass_Balance/Loads/Import_Total_Load_Data.R")
source("YB_Mass_Balance/Loads/Import_Net_Load_Data.R")

# Prepare flow data to join with load data
total_inflows <- daily_flow_data_se %>% 
  # Only include inlet flows
  filter(LocType == "Inlet") %>% 
  # Group by and sum flow data
  group_by(SamplingEvent) %>% 
  summarize(total_inflow = sum(Flow)) %>% 
  ungroup()

# Prepare total load data to join with net load data
loads_total_mod <- loads_total %>%
  filter(LocType != "Below Liberty") %>% 
  pivot_wider(
    id_cols = -digits,
    names_from = LocType,
    values_from = total_load
  )

# Join net and total MeHg loads and flow data
mehg_loads_flow <- loads_net %>% 
  filter(Reach == "Upper") %>%
  select(-c(Reach, digits)) %>% 
  left_join(loads_total_mod) %>% 
  filter(Analyte == "MeHg- total") %>%
  left_join(total_inflows) %>% 
  select(Year, net_load:total_inflow) %>% 
  rename(
    Net_Load = net_load,
    Inlet_Load = Inlet,
    Outlet_Load = Outlet,
    Total_Inflow = total_inflow
  )

# Add load and flow data from 2006 and restructure for plotting
mehg_loads_flow_all <- loads_flow_cf %>% 
  select(Year, Inlet_Load:Total_Inflow) %>% 
  bind_rows(mehg_loads_flow) %>% 
  pivot_longer(
    cols = Inlet_Load:Net_Load,
    names_to = "Load_Type",
    values_to = "Load"
  ) %>% 
  mutate(
    Year = as.character(Year),
    Load_Type = factor(Load_Type, levels = c("Inlet_Load", "Outlet_Load", "Net_Load"))
  )

# Create Figure
figure <- mehg_loads_flow_all %>% 
  ggplot(aes(x = Total_Inflow, y = Load, color = Year)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE
  ) +
  facet_wrap(
    vars(Load_Type),
    ncol = 2,
    scales = "free_y",
    labeller = labeller(
      Load_Type = c(
        Inlet_Load = "Inlet Load",
        Outlet_Load = "Outlet Load",
        Net_Load = "Net Internal Production"
      )
    )
  ) +
  scale_x_continuous(
    name = "Total Inflow (cfs)",
    labels = label_comma()
  ) +
  ylab("MeHg Load (g/day)") +
  add_gen_color_pal(
    num_colors = 4,
    aes_type = "color",
    legend_title = "Year"
  ) +
  theme_owhg() +
  theme(
    legend.margin = margin(0, 0, 0, 0),
    legend.position = c(0.72, 0.22)
  )

# Export Figure
ggsave(
  paste0("final_report_fig_b-", fig_num, ".jpg"),
  plot = figure,
  dpi = 300,
  width = 6.5, 
  height = 6.5, 
  units = "in"
)

# Clean up
rm(list= ls()[!(ls() %in% obj_keep)])

