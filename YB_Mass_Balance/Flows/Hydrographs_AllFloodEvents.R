# Yolo Bypass Inlet-Outlet Study
# Create hydrographs for all flood events- 2014, 2016, and 2017

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(patchwork)


# Set System Timezone as "Etc/GMT+8" to make it consistent with all df's 
Sys.setenv(TZ = "Etc/GMT+8")
Sys.time()
Sys.timezone()

# 1. Import Flow Data ----------------------------------------------------

# 2014 Flood Event
klrc14 <- read_excel("Flows/2014_YB_Flood_Flows.xlsx", range = "KLRC!A1:C2977")

ccsb14 <- read_excel("Flows/2014_YB_Flood_Flows.xlsx", range = "CCSB!A1:E2977")
  
putah14 <- read_excel("Flows/2014_YB_Flood_Flows.xlsx", range = "Putah Ck!L1:M31")

lisb14 <- read_excel("Flows/2014_YB_Flood_Flows.xlsx", range = "Lisbon!A1:C2977")

# 2016 Flood Event
fremont16 <- read_excel("Flows/2016_YB_Flood_Flows.xlsx", range = "Fremont Weir!A1:C2977")

klrc16 <- read_excel("Flows/2016_YB_Flood_Flows.xlsx", range = "KLRC!A1:C2977")

ccsb16 <- read_excel("Flows/2016_YB_Flood_Flows.xlsx", range = "CCSB!A1:E2977")

putah16 <- read_excel("Flows/2016_YB_Flood_Flows.xlsx", range = "Putah Ck!L1:M32")

lisb16 <- read_excel("Flows/2016_YB_Flood_Flows.xlsx", range = "Lisbon!A1:C2977")

# 2017 Flood Event
fremont17 <- read_excel("Flows/2017_YB_Flood_Flows.xlsx", range = "Fremont Weir!A1:D12097")

klrc17 <- read_excel("Flows/2017_YB_Flood_Flows.xlsx", range = "KLRC!A1:C12001")

ccsb17 <- read_excel("Flows/2017_YB_Flood_Flows.xlsx", range = "CCSB!A1:E11905")

putah17 <- read_excel("Flows/2017_YB_Flood_Flows.xlsx", range = "Putah Ck!M1:N125")

sacweir17 <- read_excel("Flows/2017_YB_Flood_Flows.xlsx", range = "Sacramento Weir!A1:B125")

lisb17 <- read_excel("Flows/2017_YB_Flood_Flows.xlsx", range = "Lisbon!A1:C12193")


# 2. Combine and Clean Data for plots -------------------------------------

# Fremont Weir
fremont_c <- bind_rows(fremont16, fremont17) %>% 
  select(-c(Date, "Stage (ft)")) %>% 
  rename(
    datetime = "Date/Time (PST)",
    flow = "Flow (cfs)"
  ) %>% 
  mutate(station = "Fremont Weir")

# KLRC
klrc_c <- bind_rows(klrc14, klrc16, klrc17) %>% 
  select(-Date) %>% 
  rename(
    datetime = "Date/Time (PST)",
    flow = "Flow (cfs)"
  ) %>% 
  mutate(station = "KLRC")

# CCSB
ccsb_c <- bind_rows(ccsb14, ccsb16, ccsb17) %>% 
  select(-c(Date, "CCSB Overflow Weir Qual")) %>% 
  rename(
    datetime = "Date/Time (PST)",
    ow_flow = "CCSB Overflow Weir Flow (cfs)",
    lfc_flow = "CCSB low-flow channel Flow (cfs)"
  ) %>% 
  # remove cases where both Overflow Weir and LFC are NA values
  filter(!(is.na(ow_flow) & is.na(lfc_flow))) %>% 
  # replace remaining NA values with numeric zero for proper addition
  replace_na(list(ow_flow = 0, lfc_flow = 0)) %>% 
  mutate(
    # calculate sum of Overflow Weir and LFC
    flow = ow_flow + lfc_flow,
    station = "CCSB- total outflow"
  ) %>% 
  select(-c(ow_flow, lfc_flow))

# Combine data for Fremont Weir, KLRC, and CCSB and calculate daily averages
comb_flow_avg <- bind_rows(fremont_c, klrc_c, ccsb_c) %>%
  filter(!is.na(flow)) %>% 
  mutate(date = date(datetime)) %>% 
  group_by(station, date) %>% 
  summarize(daily_flow = mean(flow)) %>% 
  ungroup()

# Putah Creek
putah_c <- bind_rows(putah14, putah16, putah17) %>% 
  rename(daily_flow = "Putah Ck Daily Average Flow (cfs)") %>% 
  mutate(
    date = as_date(Date),
    station = "Putah Creek"
  ) %>% 
  select(-Date)

# Sacramento Weir
sacweir17_c <- sacweir17 %>% 
  rename(daily_flow = "Daily Average Flow (cfs)") %>% 
  mutate(
    date = as_date(Date),
    station = "Sacramento Weir"
  ) %>% 
  select(-Date)

# Combine all daily average flow data for the input sites
all_flow_avg <- bind_rows(comb_flow_avg, putah_c, sacweir17_c) %>% 
  mutate(
    daily_flow = round(daily_flow, 2),
    year = year(date)
  ) %>% 
  filter(date >= "2014-12-10") %>% 
  filter(!(date >= "2014-12-31" & date < "2016-03-08")) %>% 
  filter(date < "2017-05-05")

# Combine all Lisbon Stage data
lisb_c <- bind_rows(lisb14, lisb16, lisb17) %>% 
  rename(
    datetime = "Date/Time (PST)",
    stage = "Stage (ft)"
  ) %>% 
  select(-Date) %>% 
  mutate(
    datetime = force_tz(datetime, tzone = "Etc/GMT+8"),
    year = year(datetime),
    station = "Lisbon"
  ) %>% 
  filter(datetime >= "2014-12-10 00:00:00") %>% 
  filter(!(datetime >= "2016-03-01 00:00:00" & datetime < "2016-03-08 00:00:00"))


# 3. Create Hydrographs ---------------------------------------------------

# Create a df of sampling events to mark these on the hydrographs of the inlets
se_dates <- tibble(
  date = as_date(
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

# Join se_dates df to all_flow_avg to pull out data for just the sampling events
se_flows <- inner_join(all_flow_avg, se_dates) %>% 
  filter(daily_flow != 0)

# Create a df of sampling events to mark these on the Lisbon hydrograph
se_datetimes <- tibble(
  datetime = as_datetime(
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

# Join se_dates df to lisb_c to pull out data for just the sampling events
se_lisb <- inner_join(lisb_c, se_datetimes)

# Create plot functions for hydrographs
  # Inlets
  plot_hydro_inlet <- function(df, df_point, PlotTitle, num_breaks) {
    p <- 
      ggplot(
        data = df,
        aes(
          x = date,
          y = daily_flow
        )
      ) +
      geom_line() +
      geom_point(
        data = df_point, 
        color = "red",
        size = 2
      ) +
      labs(
        title = PlotTitle,
        x = NULL,
        y = "Daily Average Flow (cfs)"
      ) +
      scale_x_date(
        breaks = breaks_pretty(num_breaks),
        labels = label_date_short(),
        expand = c(0.01, 0.01)
      ) +
      scale_y_continuous(
        labels = label_comma(),
        limits = c(0, NA)
      ) +
      theme(text = element_text(size = 10))
    
    return(p)
  }
  
  # Lisbon
  plot_hydro_lisb <- function(df, df_point, num_breaks) {
    p <- 
      ggplot(
        data = df,
        aes(
          x = datetime,
          y = stage
        )
      ) +
      geom_line() +
      geom_point(
        data = df_point, 
        color = "red",
        size = 2
      ) +
      labs(
        title = "Toe Drain at Lisbon Weir",
        x = NULL,
        y = "Stage (ft)"
      ) +
      scale_x_datetime(
        breaks = breaks_pretty(num_breaks),
        labels = label_date_short(),
        expand = c(0.01, 0.01)
      ) +
      theme(text = element_text(size = 10))
    
    return(p)
  }

# Create nested df's
  # Inlets
  all_flow_avg_n <- all_flow_avg %>% 
    group_nest(year, station)
  
  se_flows_n <- se_flows %>% 
    group_nest(year, station) %>% 
    rename(data_se = data)

  # Lisbon
  lisb_c_n <- lisb_c %>% 
    group_nest(year, station)
  
  se_lisb_n <- se_lisb %>% 
    group_nest(year, station) %>% 
    rename(data_se = data)

# Join nested df's and run functions to create hydrographs
  # Inlets
  hydrograph_inlets <- left_join(all_flow_avg_n, se_flows_n) %>% 
    mutate(plots = pmap(
      list(data, data_se, station), 
      .f = plot_hydro_inlet, 
      num_breaks = 10
    ))
  
  # Lisbon
  hydrograph_lisb <- left_join(lisb_c_n, se_lisb_n) %>% 
    mutate(plots = map2(
      data, 
      data_se, 
      .f = plot_hydro_lisb, 
      num_breaks = 10
    ))

# Combine nested df's with hydrographs together
hydrograph_all <- bind_rows(hydrograph_inlets, hydrograph_lisb)
  
# Create function to group hydrographs together
group_hydro <- function(df, group) {
  # Group plots together
  p <- df %>% pull(plots) %>% wrap_plots(ncol = 2)
  
  # Add Plot Title
  p <- p +  
    plot_annotation(
      title = paste(group, "Flood Event"),
      theme = theme(plot.title = element_text(size = 14)),
      caption = "Red points indicate sampling events"
    )
  
  return(p)
}
  
# Group hydrographs together by year
hydrograph_group <- hydrograph_all %>%   
  select(-c(data, data_se)) %>% 
  # apply custom order
  mutate(station = factor(
    station, 
    levels = c(
      "Lisbon",
      "Fremont Weir",
      "KLRC",
      "CCSB- total outflow",
      "Putah Creek",
      "Sacramento Weir"
    )
  )) %>% 
  arrange(station) %>% 
  group_nest(year) %>% 
  mutate(group_plots = map2(
    data, 
    year, 
    .f = group_hydro
  ))

# Export hydrographs as .jpg files for import into final report
hydrograph_group_list <- hydrograph_group %>% pull(group_plots)

  # 2014 plot
  ggsave(
    "Plots_Final_Report/hydrographs_2014.jpg", 
    plot = hydrograph_group_list[[1]], 
    width = 7.5, 
    height = 7, 
    units = "in"
  )

  # 2016 plot
  ggsave(
    "Plots_Final_Report/hydrographs_2016.jpg", 
    plot = hydrograph_group_list[[2]], 
    width = 7.5, 
    height = 9.5, 
    units = "in"
  )
  
  # 2017 plot
  ggsave(
    "Plots_Final_Report/hydrographs_2017.jpg",
    plot = hydrograph_group_list[[3]],
    width = 7.5,
    height = 9.5,
    units = "in"
  )
  

# 4. Create Area Plots ----------------------------------------------------

# Define custom color palette based on Viridis
inlet_colors <- c(
  "KLRC" = "#FDE333",
  "CCSB- total outflow" = "#53CC67",
  "Putah Creek" = "#009B95",
  "Sacramento Weir" = "#00588B",
  "Fremont Weir" = "#4B0055"
) 

# Create plot function for area plots
plot_area <- function(df, PlotTitle, PlotType) {
  p <- 
    ggplot(
      data = df,
      aes(
        x = date,
        y = daily_flow,
        fill = station
      )
    ) +
    scale_x_date(
      breaks = breaks_pretty(10),
      labels = label_date_short(),
      expand = c(0.01, 0.01)
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 10),
      axis.ticks = element_line(
        color = "grey40", 
        linetype = 1
      )
    ) +
    scale_fill_manual(
      name = "Inlet",
      values = inlet_colors,
      drop = FALSE
    )
  
  if (PlotType == "stacked") {
    p <- p +
      geom_area() +
      labs(
        title = paste(PlotTitle, "Inlet Flows"),
        x = NULL,
        y = "Daily Average Flow (cfs)"
      ) +
      scale_y_continuous(
        labels = label_comma(),
        expand = c(0.02, 0.02)
      )
    
  } else if (PlotType == "filled") {
    p <- p +
      geom_area(position = "fill") +
      labs(
        title = paste(PlotTitle, "Flow Percentages"),
        x = NULL,
        y = "Percent of total Inlet flow"
      ) +
      scale_y_continuous(
        labels = label_percent(),
        expand = c(0.02, 0.02)
      )
  }
  
  return(p)
}

# Create area plots for each year
area_plots <- all_flow_avg %>% 
  # apply custom order for stations
  mutate(station = factor(
    station,
    levels = c(
      "KLRC",
      "CCSB- total outflow",
      "Putah Creek",
      "Sacramento Weir",
      "Fremont Weir"
    )
  )) %>% 
  group_nest(year) %>% 
  mutate(
    plot_stack = map2(
      data, 
      year, 
      .f = plot_area, 
      PlotType = "stacked"
    ),
    plot_fill = map2(
      data,
      year,
      .f = plot_area,
      PlotType = "filled"
    )
  ) %>% 
  # rearrange df for more efficient grouping of plots
  select(-data) %>% 
  pivot_longer(
    cols = -year,
    names_to = "type",
    values_to = "plot"
  ) %>% 
  arrange(desc(type), year)
 
# Group area plots together
area_plots_group <- area_plots %>% 
  pull(plot) %>% 
  wrap_plots() + plot_layout(guides = "collect")

# Export area plots as .jpg files for import into final report
ggsave(
  "Plots_Final_Report/area_plots_inflow.jpg", 
  plot = area_plots_group,
  width = 10, 
  height = 7, 
  units = "in"
)

