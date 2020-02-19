# Yolo Bypass Inlet-Outlet Study
# Create plots of the loads

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(rlang)
library(broom)
library(patchwork)


# 1. Import Load data and setup for plotting ---------------------------------

# Import data
Loads <- read_csv("Loads/All_YB_Loads-R.csv") %>% 
  select(-c(Conc:Flow))

# Sum the CCSB Loads
CCSB_Loads <- Loads %>% 
  filter(str_detect(StationName, "^CCSB")) %>% 
  group_by(SamplingEvent, Year, Analyte, LocType, LoadUnits) %>% 
  summarize(TotalLoad = sum(Load)) %>% 
  ungroup() %>% 
  rename(Load = TotalLoad) %>% 
  add_column(StationName = "CCSB")

# Add back the summed CCSB loads
Loads_plot <- Loads %>% 
  filter(!str_detect(StationName, "^CCSB")) %>%
  bind_rows(CCSB_Loads)

# Calculate TSS-VSS for plots
TSSminusVSS <- Loads_plot %>% 
  filter(Analyte %in% c("TSS", "VSS")) %>% 
  spread(Analyte, Load) %>% 
  mutate(TSS_VSS = TSS - VSS) %>% 
  select(-c(TSS, VSS)) %>% 
  gather(Analyte, Load, TSS_VSS) %>% 
  mutate(Analyte = "TSS - VSS")

# Add TSS-VSS calculation back to Loads_plot df
Loads_plot <- bind_rows(Loads_plot, TSSminusVSS)

# Make some modifications to the Loads_plot df for plotting
Loads_plot <- Loads_plot %>% 
  mutate(
    # Shorten some of the StationNames
    StationName = case_when(
      StationName == "Knights Landing Ridge Cut" ~ "KLRC",
      StationName == "Putah Creek at Mace Blvd" ~ "Putah Ck",
      StationName == "Sac River above the Sacramento Weir" ~ "Sac Weir",
      StationName == "Toe Drain at 1/2 Lisbon" ~ "Toe Drain",
      StationName == "Liberty Cut below Stairsteps" ~ "Liberty Cut",
      StationName == "Shag Slough below Stairsteps" ~ "Shag Slough",
      TRUE ~ StationName
    ),
    # Create Analyte groups for more efficient plots
    AnalyteGroup = case_when(
      str_detect(Analyte, "^THg") ~ "THg",
      str_detect(Analyte, "^MeHg") ~ "MeHg",
      Analyte %in% c("TOC", "DOC", "POC") ~ "Organic Carbon",
      Analyte %in% c("TSS", "VSS", "TSS - VSS") ~ "Suspended Sediment",
      Analyte == "Chloride- filtered" ~ "Chloride",
      Analyte %in% c("Iron- filtered", "Manganese- filtered") ~ "Filtered Metals"
    ),
    AnalyteGroup2 = if_else(
      AnalyteGroup %in% c("Suspended Sediment", "Chloride"),
      "Suspended Sediment and Chloride",
      AnalyteGroup
    )
  )  
  
# Setup plotting order
SampEvents <- sort(unique(Loads_plot$SamplingEvent))
SampEventsOrder <- SampEvents[c(3,9,5:7,4,8,10:11,1:2)]

InletOrder <- c(
  "KLRC", 
  "CCSB", 
  "Putah Ck", 
  "Sac Weir", 
  "Fremont Weir"
)

OutletOrder <- c(
  "Toe Drain",
  "Little Holland",
  "Liberty Cut",
  "Main Liberty",
  "Shag Slough"
)

AllStationsOrder <- c(InletOrder, OutletOrder)

LocTypeOrder <- c(
  "Inlet",
  "Outlet",
  "Below Liberty"
)

Loads_plot <- Loads_plot %>% 
  mutate(
    SamplingEvent = factor(SamplingEvent, levels = SampEventsOrder),
    LocType = factor(LocType, levels = LocTypeOrder),
    AnalyteGroup = factor(
      AnalyteGroup, 
      levels = c(
        "THg",
        "MeHg",
        "Organic Carbon",
        "Suspended Sediment",
        "Chloride",
        "Filtered Metals"
      )
    ),
    AnalyteGroup2 = factor(
      AnalyteGroup2,
      levels = c(
        "THg",
        "MeHg",
        "Organic Carbon",
        "Suspended Sediment and Chloride",
        "Filtered Metals"
      )
    )
  )

# Calculate % MeHg for inlet and outlet plots
PerMeHg <- Loads_plot %>% 
  filter(Analyte %in% c("MeHg- total", "THg- total")) %>% 
  select(-c(LoadUnits:AnalyteGroup2)) %>% 
  spread(Analyte, Load) %>% 
  rename(
    MeHg = "MeHg- total",
    THg = "THg- total"
  ) %>% 
  mutate(Per_MeHg = MeHg/THg) %>% 
  select(-c(THg, MeHg)) 

# Calculate the percentage of each fraction for THg and MeHg
FracTHg <- Loads_plot %>% 
  filter(AnalyteGroup == "THg") %>% 
  select(-c(LoadUnits, AnalyteGroup2)) %>% 
  spread(Analyte, Load) %>% 
  rename(
    Filt = "THg- filtered",
    Part = "THg- particulate",
    Total = "THg- total"
  ) %>% 
  mutate(
    Filtered = Filt/Total,
    Particulate = Part/Total
  ) %>% 
  select(-c(Filt:Total)) %>%
  gather(Fraction, Percent, Filtered:Particulate) 
  
FracMeHg <- Loads_plot %>% 
  filter(AnalyteGroup == "MeHg") %>% 
  select(-c(LoadUnits, AnalyteGroup2)) %>% 
  spread(Analyte, Load) %>% 
  rename(
    Filt = "MeHg- filtered",
    Part = "MeHg- particulate",
    Total = "MeHg- total"
  ) %>% 
  mutate(
    Filtered = Filt/Total,
    Particulate = Part/Total
  ) %>% 
  select(-c(Filt:Total)) %>%
  gather(Fraction, Percent, Filtered:Particulate)

# Bind the two Frac df's together
FracHg <- bind_rows(FracTHg, FracMeHg)

# Create vectors for the custom labels
AnalyteGroup_Label <- c(
  "THg" = "THg (g/day)",          
  "MeHg" = "MeHg (g/day)",          
  "Organic Carbon" = "Organic Carbon (1,000 kg/day)",   
  "Suspended Sediment" = "Suspended Sediment (1,000 kg/day)",   
  "Chloride" = "Chloride (1,000 kg/day)",   
  "Filtered Metals" = "Filtered Metals (kg/day)"        
)

# Recreate default ggplot2 colors for n groups
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


# 2. Create plots of the Loads -----------------------------------------------

# Create functions for Basic Load plots- column fill color by Group, option to dodge columns
  # 1 facet grouping
  LoadPlotsByGroup1 <- function(df, FreeY, PlotTitle, y_name, FillGroup, Dodged) {
    y_name.quo <- enquo(y_name)
    FillGroup.quo <- enquo(FillGroup)
    
    p <- 
      ggplot(
        data = df,
        aes(
          x = SamplingEvent, 
          y = !!y_name.quo, 
          fill = !!FillGroup.quo
        )
      ) +
      labs(
        title = PlotTitle,
        x = "Sampling Event",
        y = paste0("Loads (", df$LoadUnits[1], ")")
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  #x-axis labels at 90 degrees
    
    if (Dodged == FALSE) {
      p <- p + geom_col()
    } else {
      p <- p + geom_col(position = "dodge")
    }
    
    if (FreeY == FALSE) {
      p <- p + 
        facet_grid(rows = vars(Analyte))
    } else {
      p <- p + 
        facet_grid(
          rows = vars(Analyte),
          scales = "free_y"
        )
    }
    
    return(p)
  }
  
  # 2 facet groupings
  LoadPlotsByGroup2 <- function(df, PlotTitle, y_name, FillGroup, Dodged) {
    y_name.quo <- enquo(y_name)
    FillGroup.quo <- enquo(FillGroup)
    
    p <- 
      ggplot(
        data = df,
        aes(
          x = SamplingEvent, 
          y = !!y_name.quo, 
          fill = !!FillGroup.quo
        )
      ) +
      facet_wrap(
        vars(AnalyteGroup, Analyte),
        ncol = 3, 
        scales = "free_y",
        labeller = labeller(AnalyteGroup = AnalyteGroup_Label)
      ) +
      labs(
        title = PlotTitle,
        x = "Sampling Event",
        y = "Loads"
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  #x-axis labels at 90 degrees
    
    if (Dodged == FALSE) {
      p <- p + geom_col()
    } else {
      p <- p + geom_col(position = "dodge")
    }
    
    return(p)
  }

# Create function for Load plots- stacked by Analyte fraction
LoadPlotAnaFrac <- function(df, PlotTitle, SubTitle, FreeY, y_name, FacetGroup) {
  y_name.quo <- enquo(y_name)
  FacetGroup.quo <- enquo(FacetGroup)
  
  p <- 
    ggplot(
      data = df,
      aes(
        x = SamplingEvent, 
        y = !!y_name.quo, 
        fill = Analyte
      )
    ) +
    geom_col(color = "gray30") +
    labs(
      title = PlotTitle,
      subtitle = SubTitle,
      x = "Sampling Event",
      y = paste0("Loads (", df$LoadUnits[1], ")")
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  #x-axis labels at 90 degrees
  
  if (FreeY == FALSE) {
    p <- p + 
      facet_grid(rows = vars(!!FacetGroup.quo))
  } else {
    p <- p + 
      facet_grid(
        rows = vars(!!FacetGroup.quo),
        scales = "free_y"
      )
  }
  
  return(p)
}

# Create function for Load plots- stacked by Percentage of Analyte fraction
LoadPlotPerAnaFrac <- function(df, PlotTitle, FacetGroup) {
  FacetGroup.quo <- enquo(FacetGroup)
  
  p <- 
    ggplot(
      data = df,
      aes(
        x = SamplingEvent, 
        y = Percent, 
        fill = Fraction
      )
    ) +
    geom_col(
      color = "gray30",
      position = "fill"
      ) +
    facet_grid(rows = vars(!!FacetGroup.quo)) +
    labs(
      title = PlotTitle,
      x = "Sampling Event",
      y = "Fraction Percentage"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + #x-axis labels at 90 degrees
    scale_y_continuous(labels = percent_format())
  
  return(p)
}

# Create a nested df and create plots based on groupings in AnalyteGroup2
Loads_plot_AG2 <- Loads_plot %>% 
  filter(Analyte != "TSS - VSS") %>% 
  filter(
    LocType == "Inlet" |
    (LocType == "Outlet" & Year == 2017)
  ) %>% 
  mutate(StationName = factor(StationName, levels = AllStationsOrder)) %>% 
  group_nest(LocType, AnalyteGroup2) %>% 
  mutate(
    freeY = if_else(
      AnalyteGroup2 == "MeHg",
      FALSE,
      TRUE
    ),
    title = paste0(LocType, " Loads - ", AnalyteGroup2)
  ) %>% 
  mutate(
    plots = pmap(
      list(data, freeY, title), 
      LoadPlotsByGroup1,
      y_name = Load,
      FillGroup = StationName,
      Dodged = FALSE
    )
  )

# Create a nested df and create plots stacked by Analyte Fraction
Loads_plot_stack <- Loads_plot %>% 
  filter(
    LocType == "Inlet" |
    (LocType == "Outlet" & Year == 2017)
  ) %>% 
  filter(!Analyte %in% c("MeHg- total", "THg- total", "TOC", "TSS")) %>% 
  filter(!AnalyteGroup %in% c("Chloride", "Filtered Metals")) %>% 
  mutate(StationName = factor(StationName, levels = AllStationsOrder)) %>% 
  group_nest(LocType, AnalyteGroup) %>% 
  mutate(
    title = case_when(
      LocType == "Inlet" & AnalyteGroup != "Suspended Sediment" ~ "Inlet Loads - Filtered and Particulate Fractions",
      LocType == "Inlet" & AnalyteGroup == "Suspended Sediment" ~ "Inlet Loads - TSS and VSS Fractions",
      LocType == "Outlet" & AnalyteGroup != "Suspended Sediment" ~ "Outlet Loads - Filtered and Particulate Fractions",
      LocType == "Outlet" & AnalyteGroup == "Suspended Sediment" ~ "Outlet Loads - TSS and VSS Fractions"
    )
  ) %>% 
  mutate(
    plots = pmap(
      list(data, title, AnalyteGroup),
      LoadPlotAnaFrac,
      FreeY = TRUE,
      y_name = Load,
      FacetGroup = StationName
    )
  )

# Create a nested df and create plots stacked by Percentage of Analyte Fraction
FracHg_plot <- FracHg %>% 
  filter(
    LocType == "Inlet" |
    (LocType == "Outlet" & Year == 2017)
  ) %>% 
  mutate(StationName = factor(StationName, levels = AllStationsOrder)) %>% 
  group_nest(LocType, AnalyteGroup) %>% 
  mutate(title = paste0(LocType, " Loads - Percentage of each ", AnalyteGroup, " fraction")) %>% 
  mutate(
    plots = map2(
      data,
      title,
      LoadPlotPerAnaFrac,
      FacetGroup = StationName
    )
  )

# Create a vector of the smaller flood events
SampEvents_small <- c(
  "Dec 22-23, 2014",
  "Jan 31-Feb 1, 2017",
  "Mar 15-16, 2017",
  "Mar 28-29, 2017",
  "Apr 11-12, 2017",
  "Apr 25-26, 2017"
)

# 2.1 Inlet Loads -----------------------------------------------------------

# Create a subset of just Inlet Loads for important parameters
InletLoads <- Loads_plot %>% 
  filter(
    LocType == "Inlet",
    AnalyteGroup %in% c(
      "THg", 
      "MeHg", 
      "Organic Carbon", 
      "Suspended Sediment"
    )
  ) %>% 
  mutate(StationName = factor(StationName, levels = InletOrder))

# Subset the InletLoads df even further
  # Just 2017 events
  InletLoads2017 <- InletLoads %>% filter(Year == 2017)
  
  # Smaller events
  InletLoads_small <- InletLoads %>% 
    filter(SamplingEvent %in% SampEvents_small)
  
  # Larger events
  InletLoads_large <- InletLoads %>% 
    filter(!SamplingEvent %in% SampEvents_small)
  
# Make a list of these df's to pass to LoadPlotsByGroup2 function
InletLoads_list <- list(
  dfs = list(InletLoads, InletLoads2017, InletLoads_small, InletLoads_large),
  titles = c(
    "Inlet Loads - All Flood Events", 
    "Inlet Loads - 2017 Flood Events", 
    "Inlet Loads - Smaller Flood Events",
    "Inlet Loads - Larger Flood Events"
  )
)

# Print Inlet Load plots to a pdf file
pdf(file = "Loads/Inlet_Loads.pdf", w = 13, h = 8.5)
  # Inlet load plots- stacked by Station, all on one page
  map2(
    InletLoads_list$dfs,
    InletLoads_list$titles,
    LoadPlotsByGroup2, 
    y_name = Load,
    FillGroup = StationName, 
    Dodged = FALSE
  ) 
  
  # Inlet load plots- stacked by Station, each AnalyteGroup2 on a different page
  inPlots <- Loads_plot_AG2$plots[1:5]
  for (p in inPlots) {
    print(p)
  }
  
  # Inlet load plots- stacked and filled to show proportion of each input to the overall load (all parameters)
    # All Parameters on one page
    InletLoads %>% 
      ggplot(
        aes(
          x = SamplingEvent, 
          y = Load, 
          fill = StationName
        )
      ) +
      geom_col(position = "fill") +
      facet_wrap(
        vars(AnalyteGroup, Analyte),
        ncol = 3, 
        scales = "free_y",
        labeller = labeller(AnalyteGroup = AnalyteGroup_Label)
      ) +
      labs(
        title = "Proportion of each Input to the overall Inlet Load",
        x = "Sampling Event",
        y = "Proportion of Total Inlet Load"
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
    
    # Each AnalyteGroup2 on a different page
    Loads_plot %>% 
      filter(LocType == "Inlet", Analyte != "TSS - VSS") %>% 
      mutate(StationName = factor(StationName, levels = InletOrder)) %>% 
      group_by(AnalyteGroup2) %>% do(plot = {
        p <- ggplot(
          data = .,
          aes(
            x = SamplingEvent, 
            y = Load, 
            fill = StationName
          )
        ) +
        geom_col(position = "fill") +
        facet_grid(
          rows = vars(Analyte),
          scales = "free_y"
        ) +
        labs(
          title = "Proportion of each Input to the overall Inlet Load",
          subtitle = .$AnalyteGroup2[1],
          x = "Sampling Event",
          y = "Proportion of Total Inlet Load"
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
          
      print(p)
    })
  
  # Inlet load plots- stacked by Analyte Fraction
  inPlots <- Loads_plot_stack$plots[1:4]
  for (p in inPlots) {
    print(p)
  }
  
  # Percentages of Analyte Fractions
  inPlots <- FracHg_plot$plots[1:2]
  for (p in inPlots) {
    print(p)
  } 
  
  # Percent MeHg
  PerMeHg %>% 
    filter(LocType == "Inlet") %>% 
    mutate(StationName = factor(StationName, levels = InletOrder)) %>% 
    ggplot(
      aes(
        x = SamplingEvent, 
        y = Per_MeHg
      )
    ) +
    geom_col() +
    facet_grid(rows = vars(StationName)) +
    labs(
      title = "Inlet Loads - Percent MeHg",
      x = "Sampling Event",
      y = "Percent MeHg"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  #x-axis labels at 90 degrees
    scale_y_continuous(labels = percent_format())
  
  # Boxplots of Input loads for each Analyte
  Loads_plot %>% 
    filter(LocType == "Inlet") %>% 
    mutate(
      Year = as.character(Year),
      StationName = factor(StationName, levels = InletOrder)
    ) %>% 
    group_by(Analyte) %>% 
    do(plot = {
      print(.$Analyte[1])
      p <- 
        ggplot(
          data = .,
          aes(
            x = StationName, 
            y = Load
          )
        ) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(
          width = 0.35,
          aes(color = Year)
        ) +
        labs(
          title = paste0("Inlet Loads - Boxplots of ", .$Analyte[1]),
          subtitle = "All Sampling Events",
          x = "Station",
          y = paste0("Loads (", .$LoadUnits[1], ")")
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x-axis labels at 90 degrees
      
      print(p)
      
    })
    
dev.off()


# 2.2 Outlet Loads- Just 2017 Sampling Events -------------------------------

# Create a subset of just Outlet Loads for the 2017 sampling events and the important parameters
OutletLoads <- Loads_plot %>% 
  filter(
    Year == 2017,
    LocType == "Outlet",
    AnalyteGroup %in% c(
      "THg", 
      "MeHg", 
      "Organic Carbon", 
      "Suspended Sediment"
    )
  ) %>% 
  mutate(StationName = factor(StationName, levels = OutletOrder))

# Subset the InletLoads df even further
  # Smaller events
  OutletLoads_small <- OutletLoads %>% 
    filter(SamplingEvent %in% SampEvents_small) %>% 
    filter(SamplingEvent != "Apr 11-12, 2017")
  
  # Larger events
  OutletLoads_large <- OutletLoads %>% 
    filter(!SamplingEvent %in% SampEvents_small)

# Make a list of these df's to pass to LoadPlotsByGroup2 function
OutletLoads_list <- list(
  dfs = list(OutletLoads, OutletLoads_small, OutletLoads_large),
  titles = c(
    "Outlet Loads - 2017 Flood Events", 
    "Outlet Loads - Smaller Flood Events",
    "Outlet Loads - Larger Flood Events"
  )
)

# Print Inlet Load plots to a pdf file
pdf(file = "Loads/Outlet_Loads.pdf", w = 13, h = 8.5)
  # Inlet load plots- stacked by Station, all on one page
  map2(
    OutletLoads_list$dfs,
    OutletLoads_list$titles,
    LoadPlotsByGroup2, 
    y_name = Load,
    FillGroup = StationName, 
    Dodged = FALSE
  ) 
  
  # Inlet load plots- stacked by Station, each AnalyteGroup2 on a different page
  outPlots <- Loads_plot_AG2$plots[6:10]
  for (p in outPlots) {
    print(p)
  }
  
  # Inlet load plots- stacked by Analyte Fraction
  outPlots <- Loads_plot_stack$plots[5:8]
  for (p in outPlots) {
    print(p)
  }
  
  # Percentages of Analyte Fractions
  outPlots <- FracHg_plot$plots[3:4]
  for (p in outPlots) {
    print(p)
  } 
  
  # Percent MeHg
  PerMeHg %>% 
    filter(
      Year == 2017,
      LocType == "Outlet"
    ) %>% 
    mutate(StationName = factor(StationName, levels = OutletOrder)) %>% 
    ggplot(
      aes(
        x = SamplingEvent, 
        y = Per_MeHg
      )
    ) +
    geom_col() +
    facet_grid(rows = vars(StationName)) +
    labs(
      title = "Outlet Loads - Percent MeHg",
      x = "Sampling Event",
      y = "Percent MeHg"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  #x-axis labels at 90 degrees
    scale_y_continuous(labels = percent_format())
  
dev.off()


# 2.3 Load Summaries --------------------------------------------------------

# Create a loads summary df which has the sums of the Inlet, Outlet, and Below Liberty loads
LoadsSummary_plot <- Loads_plot %>% 
  group_by(SamplingEvent, Year, Analyte, AnalyteGroup, AnalyteGroup2, LoadUnits, LocType) %>% 
  summarize(TotalLoad = sum(Load)) %>% 
  ungroup()

# Make some calculations on the Loads Summary
  # Percent MeHg
  PerMeHg_Summ <- LoadsSummary_plot %>% 
    filter(Analyte %in% c("MeHg- total", "THg- total")) %>% 
    select(-c(AnalyteGroup:LoadUnits)) %>% 
    spread(Analyte, TotalLoad) %>% 
    rename(
      MeHg = "MeHg- total",
      THg = "THg- total"
    ) %>% 
    mutate(Per_MeHg = MeHg/THg) %>% 
    select(-c(THg, MeHg))
  
  # Percentage of each fraction for THg
  FracTHg_Summ <- LoadsSummary_plot %>% 
    filter(AnalyteGroup == "THg") %>% 
    select(-c(LoadUnits, AnalyteGroup2)) %>%
    spread(Analyte, TotalLoad) %>% 
    rename(
      Filt = "THg- filtered",
      Part = "THg- particulate",
      Total = "THg- total"
    ) %>% 
    mutate(
      Filtered = Filt/Total,
      Particulate = Part/Total
    ) %>% 
    select(-c(Filt:Total)) %>%
    gather(Fraction, Percent, Filtered:Particulate) 
  
  # Percentage of each fraction for MeHg
  FracMeHg_Summ <- LoadsSummary_plot %>% 
    filter(AnalyteGroup == "MeHg") %>% 
    select(-c(LoadUnits, AnalyteGroup2)) %>%
    spread(Analyte, TotalLoad) %>% 
    rename(
      Filt = "MeHg- filtered",
      Part = "MeHg- particulate",
      Total = "MeHg- total"
    ) %>% 
    mutate(
      Filtered = Filt/Total,
      Particulate = Part/Total
    ) %>% 
    select(-c(Filt:Total)) %>%
    gather(Fraction, Percent, Filtered:Particulate) 
  
  # Bind the two Frac df's together
  FracHg_Summ <- bind_rows(FracTHg_Summ, FracMeHg_Summ)

# Create a subset of the loads summary df to include most important parameters
LoadsSummary_plot_imp <- LoadsSummary_plot %>%  
  filter(
    AnalyteGroup %in% c(
      "THg", 
      "MeHg", 
      "Organic Carbon", 
      "Suspended Sediment"
    )
  )

# Subset the LoadsSummary_plot_imp df even further
  # Just 2017 events
  LoadsSummary_plot_imp2017 <- LoadsSummary_plot_imp %>% filter(Year == 2017)
  
  # Smaller events
  LoadsSummary_plot_imp_small <- LoadsSummary_plot_imp %>% 
    filter(SamplingEvent %in% SampEvents_small)
  
  # Larger events
  LoadsSummary_plot_imp_large <- LoadsSummary_plot_imp %>%
    filter(!SamplingEvent %in% SampEvents_small)
  
# Subset loads summary df for the other parameters
LoadsSummary_plot_other <- LoadsSummary_plot %>% 
  filter(AnalyteGroup %in% c("Chloride", "Filtered Metals"))

# Make a list of these df's to pass to LoadPlotsByGroup2 function
LoadsSummary_list <- list(
  dfs = list(
    LoadsSummary_plot_imp,
    LoadsSummary_plot_imp2017,
    LoadsSummary_plot_imp_small,
    LoadsSummary_plot_imp_large,
    LoadsSummary_plot_other
  ),
  titles = c(
    "Total Input, Output, and Below Liberty Island Loads - All Flood Events", 
    "Total Input, Output, and Below Liberty Island Loads - 2017 Flood Events",
    "Total Input, Output, and Below Liberty Island Loads - Smaller Flood Events",
    "Total Input, Output, and Below Liberty Island Loads - Larger Flood Events",
    "Total Input, Output, and Below Liberty Island Loads - All Flood Events"
  )
)
  
# Create a nested df and create plots based on groupings in AnalyteGroup2
LoadsSummary_plot_AG2 <- LoadsSummary_plot %>% 
  filter(Analyte != "TSS - VSS") %>% 
  group_nest(AnalyteGroup2) %>% 
  mutate(
    freeY = if_else(
      AnalyteGroup2 == "MeHg",
      FALSE,
      TRUE
    ),
    title = paste0("Total Input, Output, and Below Liberty Island Loads - ", AnalyteGroup2)
  ) %>% 
  mutate(
    plots = pmap(
      list(data, freeY, title), 
      LoadPlotsByGroup1,
      y_name = TotalLoad,
      FillGroup = LocType,
      Dodged = TRUE
    )
  )

# Create a nested df and create plots stacked by Analyte Fraction
LoadsSummary_plot_stack <- LoadsSummary_plot_imp %>% 
  filter(!Analyte %in% c("MeHg- total", "THg- total", "TOC", "TSS")) %>% 
  group_nest(AnalyteGroup) %>% 
  mutate(
    title = if_else(
      AnalyteGroup != "Suspended Sediment",
      "Total Input, Output, and Below Liberty Island Loads - Filtered and Particulate Fractions",
      "Total Input, Output, and Below Liberty Island Loads - TSS and VSS Fractions"
    )
  ) %>% 
  mutate(
    plots = pmap(
      list(data, title, AnalyteGroup),
      LoadPlotAnaFrac,
      FreeY = FALSE,
      y_name = TotalLoad,
      FacetGroup = LocType
    )
  )

# Create a nested df and create plots stacked by Percentage of Analyte Fraction
FracHg_Summ_plot <- FracHg_Summ %>% 
  group_nest(AnalyteGroup) %>% 
  mutate(title = paste0("Total Input, Output, and Below Liberty Island Loads - Percentage of each ", AnalyteGroup, " fraction")) %>% 
  mutate(
    plots = map2(
      data,
      title,
      LoadPlotPerAnaFrac,
      FacetGroup = LocType
    )
  )

# Create plots for Total Loads (Inlet, Outlet, Below Liberty) vs. Total Input Flow
  # Import Flow data and modify it to include the total input flows for each sampling event
  FlowData <- read_excel("Flows/DailyAvgFlows_All_and_SE.xlsx", sheet = "Just Sampling Events") %>% 
    # Only keep inlet flows
    filter(LocType == "Inlet") %>% 
    # Sum flows for each sampling event
    group_by(SamplingEvent, Year) %>% 
    summarize(TotalInputFlow = sum(Flow)) %>% 
    ungroup() %>% 
    # add year to SamplingEvent to match NetLoads df
    mutate(SamplingEvent = paste0(SamplingEvent, ", ", Year))
  
  # Join FlowData df to NetLoads df
  LoadsSummary_wFlow <- left_join(LoadsSummary_plot, FlowData)
  
  # Create a function for the Total Load vs Input Flow scatterplots- for just MeHg and THg plots
  LoadsSummary.Flow_Plot <- function(df, Param) {
    p <- 
      ggplot(
        data = df,
        aes(
          x = TotalInputFlow,
          y = TotalLoad,
          color = LocType
        )
      ) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) + 
      labs(
        title = "Total Loads vs. Input Flows Scatterplots",
        subtitle = Param,
        x = "Daily Average Input Flow (cfs)",
        y = "Total Load (g/day)"
      ) +
      facet_grid(
        rows = vars(Analyte),
        scales = "free_y"
      )
    
    return(p)
  } 

# Print Summary Load plots to a pdf file
pdf(file = "Loads/Loads_Summary.pdf", w = 13, h = 8.5)
  # Summary load plots- Dodged by LocType
  map2(
    LoadsSummary_list$dfs,
    LoadsSummary_list$titles,
    LoadPlotsByGroup2, 
    y_name = TotalLoad,
    FillGroup = LocType, 
    Dodged = TRUE
  )
  
  # Summary load plots- Dodged by LocType, each AnalyteGroup2 on a different page
  for (p in LoadsSummary_plot_AG2$plots) {
    print(p)
  }
  
  # Summary load plots- stacked by Analyte Fraction
  for (p in LoadsSummary_plot_stack$plots) {
    print(p)
  }
  
  # Percentages of Analyte Fractions
  for (p in FracHg_Summ_plot$plots) {
    print(p)
  } 
  
  # Percent MeHg
  PerMeHg_Summ %>% 
    ggplot(
      aes(
        x = SamplingEvent, 
        y = Per_MeHg
      )
    ) +
    geom_col() +
    facet_grid(rows = vars(LocType)) +
    labs(
      title = "Total Input, Output, and Below Liberty Island Loads - Percent MeHg",
      x = "Sampling Event",
      y = "Percent MeHg"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  #x-axis labels at 90 degrees
    scale_y_continuous(labels = percent_format())
  
  # Total Load vs. Total Input Flow scatterplots
    # All on one page
    LoadsSummary_wFlow %>% 
      filter(
        AnalyteGroup %in% c("MeHg", "THg") | 
        Analyte == "TSS"
      ) %>% 
      ggplot(
        aes(
          x = TotalInputFlow,
          y = TotalLoad,
          color = LocType
        )
      ) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) + 
      labs(
        title = "Total Loads vs. Input Flows Scatterplots",
        x = "Daily Average Input Flow (cfs)",
        y = "Total Load"
      ) + 
      facet_wrap(
        vars(AnalyteGroup, Analyte),
        ncol = 3,
        scales = "free_y",
        labeller = labeller(AnalyteGroup = AnalyteGroup_Label)
      )
    
    # Just MeHg, THg
    LoadsSummary_wFlow %>% 
      filter(AnalyteGroup %in% c("MeHg", "THg")) %>%
      split(.$AnalyteGroup, drop = TRUE) %>% 
      imap(LoadsSummary.Flow_Plot)
    
    # Just TSS
    LoadsSummary_wFlow %>% 
      filter(Analyte == "TSS") %>% 
      ggplot(
        aes(
          x = TotalInputFlow,
          y = TotalLoad,
          color = LocType
        )
      ) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) + 
      labs(
        title = "Total Loads vs. Input Flows Scatterplot",
        subtitle = "TSS",
        x = "Daily Average Input Flow (cfs)",
        y = "Total Load (1,000 kg/day)"
      )
    
dev.off()


# 2.4 Net Loads -------------------------------------------------------------

# Calculate Net Loads
NetLoads_plot <- LoadsSummary_plot %>% 
  spread(LocType, TotalLoad) %>% 
  rename(BL = "Below Liberty") %>% 
  mutate(
    "Stairsteps - Inlet" = Outlet - Inlet,
    "Below Liberty - Stairsteps" = if_else(
      !is.na(BL),
      BL - Outlet,
      NULL
    ),
    "Below Liberty - Inlet" = if_else(
      !is.na(BL),
      BL - Inlet,
      NULL
    )
  ) %>% 
  select(-c(Inlet:BL)) %>% 
  gather(Segment, NetLoad, "Stairsteps - Inlet":"Below Liberty - Inlet") %>% 
  filter(!is.na(NetLoad)) %>% 
  mutate(
    Segment = factor(
      Segment, 
      levels = c(
        "Stairsteps - Inlet", 
        "Below Liberty - Stairsteps",
        "Below Liberty - Inlet"
      )
    )
  )

# Create a function for faceted plots of the net loads for each analyte
NetLoad.byAna_Plot <- function(df, stitle) {
  p <- 
    ggplot(
      data = df,
      aes(
        x = SamplingEvent, 
        y = NetLoad 
      )
    ) +
    geom_col() +
    facet_wrap(
      vars(AnalyteGroup, Analyte),
      ncol = 3, 
      scales = "free_y",
      labeller = labeller(AnalyteGroup = AnalyteGroup_Label)
    ) +
    labs(
      title = "Net Loads - All Flood Events",
      subtitle = paste0("Segment: ", stitle),
      x = "Sampling Event",
      y = "Loads"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  #x-axis labels at 90 degrees

  return(p)
}

# Create a subset of the NetLoads df to compare net loads based on the output location (Stairsteps vs. Below Liberty)
NetLoads_plot_imp_comp <- NetLoads_plot %>% 
  filter(
    AnalyteGroup %in% c(
      "THg", 
      "MeHg", 
      "Organic Carbon", 
      "Suspended Sediment"
    ),
    Year == 2017,
    SamplingEvent != "Apr 11-12, 2017",
    Segment != "Below Liberty - Stairsteps"
  )

  # Smaller events
  NetLoads_plot_imp_comp_s <- NetLoads_plot_imp_comp %>% 
    filter(SamplingEvent %in% SampEvents_small) 
  
  # Larger events
  NetLoads_plot_imp_comp_l <- NetLoads_plot_imp_comp %>% 
    filter(!SamplingEvent %in% SampEvents_small)

# Make a list of these df's to pass to LoadPlotsByGroup2 function
NetLoads_list <- list(
  dfs = list(
    NetLoads_plot_imp_comp,
    NetLoads_plot_imp_comp_s,
    NetLoads_plot_imp_comp_l
  ),
  titles = c(
    "Comparison of Net Loads based on the Output location (Stairsteps vs. Below Liberty)",
    "Comparison of Net Loads based on the Output location (Stairsteps vs. Below Liberty) - Smaller Events",
    "Comparison of Net Loads based on the Output location (Stairsteps vs. Below Liberty) - Larger Events"
  )
)

# Create a nested df and create net load comparison plots based on groupings in AnalyteGroup2
NetLoads_plot_AG2 <- NetLoads_plot %>% 
  filter(Analyte != "TSS - VSS") %>%
  filter(
    Year == 2017,
    SamplingEvent != "Apr 11-12, 2017",
    Segment != "Below Liberty - Stairsteps"
  ) %>% 
  group_nest(AnalyteGroup2) %>% 
  mutate(
    freeY = if_else(
      AnalyteGroup2 == "MeHg",
      FALSE,
      TRUE
    ),
    title = paste0("Comparison of Net Loads based on the Output location - ", AnalyteGroup2),
    plots = pmap(
      list(data, freeY, title), 
      LoadPlotsByGroup1,
      y_name = NetLoad,
      FillGroup = Segment,
      Dodged = TRUE
    )
  )

# Create a nested df and create net load plots dodged by segment based on groupings in AnalyteGroup2
NetLoads_plot_AG2_bySeg <- NetLoads_plot %>% 
  filter(Analyte != "TSS - VSS") %>%
  filter(
    Year == 2017,
    SamplingEvent != "Apr 11-12, 2017"
  ) %>% 
  group_nest(AnalyteGroup2) %>% 
  mutate(
    freeY = if_else(
      AnalyteGroup2 == "MeHg",
      FALSE,
      TRUE
    ),
    title = paste0("Net Loads for each Segment - ", AnalyteGroup2),
    plots = pmap(
      list(data, freeY, title), 
      LoadPlotsByGroup1,
      y_name = NetLoad,
      FillGroup = Segment,
      Dodged = TRUE
    )
  )

# Create plots for Net Load vs. Total Input Flow
  # Join FlowData df to NetLoads df
  NetLoads_wFlow <- left_join(NetLoads_plot, FlowData)

  # Create a function for the Net Load vs Input Flow scatterplots
  NetLoad.Flow_Plot <- function(df, Param, Rsq, pval) {
    p <- 
      ggplot(
        data = df,
        aes(
          x = TotalInputFlow,
          y = NetLoad
        )
      ) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) + 
      labs(
        title = Param,
        subtitle = paste0("R Squared = ", Rsq, "%\np-value = ", pval),
        x = "Daily Average Input Flow (cfs)",
        y = paste0("Net Load (", df$LoadUnits[1], ")")
      )
    
    return(p)
  }  
  
  # Create a function for grouping Net Load vs. Input Flow scatterplots using custom patchwork panels
  Group.NetLoadFlow.plots <- function(df, Group, PlotSubT) {
    p <- df$plot[[1]] + df$plot[[2]] + df$plot[[3]] + df$plot[[4]] + df$plot[[5]] +
      df$plot[[6]] + df$plot[[7]] + df$plot[[8]] + df$plot[[9]] + df$plot[[10]] +
      plot_layout(ncol = 4)
    
    # Add Plot Title
    if (!is.na(PlotSubT)) {
      p <- p +
        plot_annotation(
          title = paste0("Net Loads vs. Input Flows Scatterplots\n", Group),
          subtitle = PlotSubT
        )
      
    } else {
      p <- p +
        plot_annotation(title = paste0("Net Loads vs. Input Flows Scatterplots\n", Group))
    
    }
    
    return(p)
  }
  
  # Create Nested dataframes to make a series of Net Load vs Input Flow scatterplots
  NetLoads_wFlow_plots <- NetLoads_wFlow %>% 
    filter(
      AnalyteGroup %in% c("MeHg", "THg", "Organic Carbon") | 
      Analyte == "TSS"
    ) %>% 
    list(
      # All 3 segments
      AllSE = .,
      # Just 2017 sampling events for the Stairsteps - Inlet segment
      Just2017 = filter(., Segment == "Stairsteps - Inlet", Year == 2017)
    ) %>% 
    map(~group_nest(.x, Segment, Analyte)) %>% 
    map(
      ~mutate(
        .x,
        model = map(data, ~ summary(lm(NetLoad ~ TotalInputFlow, data = .x))),
        Rsquared = signif(map_dbl(model, ~glance(.x)$r.squared*100), 3),
        p_value = signif(map_dbl(model, ~glance(.x)$p.value), 2),
        plot = pmap(
          list(data, Analyte, Rsquared, p_value),
          NetLoad.Flow_Plot
        )
      )
    ) %>% 
    bind_rows(.id = "Analysis") %>% 
    select(Analysis, Segment, Analyte, plot)
    
  # Setup plotting order
  NetLoadFlow <- sort(unique(NetLoads_wFlow_plots$Analyte))
  NetLoadFlow.Order <- NetLoadFlow[c(2:4,6:8,1,5,9,10)]
  
  NetLoads_wFlow_plots <- NetLoads_wFlow_plots %>% 
    mutate(Analyte = factor(Analyte, levels = NetLoadFlow.Order)) %>% 
    arrange(Analysis, Segment, Analyte)
  
  # Nest df's one level further and apply plot grouping function
  NetLoads_wFlow_plots <- NetLoads_wFlow_plots %>% 
    group_nest(Analysis, Segment) %>% 
    mutate(
      SubTitle = case_when(
        Analysis == "Just2017" ~ "Just 2017 Sampling Events",
        Analysis == "AllSE" & Segment == "Stairsteps - Inlet" ~ "All Sampling Events",
        TRUE ~ NA_character_
      ), 
      plot.group = pmap(
        list(data, Segment, SubTitle),
        Group.NetLoadFlow.plots
      )
    )
  
  # Reorder df to print off plots in proper order
  NetLoads_wFlow_plots <- NetLoads_wFlow_plots[c(1,4,3,2),]
      
# Create plots for Net Load vs. Time Inundated 
  # Create a function for the Net Load vs Time Inundated scatterplots
  NetLoad.TFlood_Plot <- function(df, Param, Rsq, pval) {
    p <- 
      ggplot(
        data = df,
        aes(
          x = WeeksFlood,
          y = NetLoad
        )
      ) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        title = Param,
        subtitle = paste0("R Squared = ", Rsq, "%\np-value = ", pval),
        x = "Weeks since start of 2017 flood",
        y = paste0("Net Load (", df$LoadUnits[1], ")")
      )
    
    return(p)
  } 

  # Create a function for grouping Net Load vs. Time Inundated scatterplots using custom patchwork panels
  Group.NetLoadTFlood.plots <- function(df, Group) {
    p <- df$plot[[1]] + df$plot[[2]] + df$plot[[3]] + 
      df$plot[[4]] + df$plot[[5]] + df$plot[[6]] +
      plot_layout(ncol = 3) +
      plot_annotation(
        title = paste0("Net Loads vs. Time Inundated Scatterplots\n", Group), 
        subtitle = "2017 Flood Event"
      )
      
    return(p)
  }
  
  # Create Nested dataframe to make a series of Net Load vs Time Inundated scatterplots
  NetLoads_wTFlood_plots <- NetLoads_plot %>% 
    filter(
      AnalyteGroup %in% c("MeHg", "Organic Carbon"),
      Year == 2017
    ) %>%
    mutate(
      SampleDate = case_when(
        SamplingEvent == "Jan 11-12, 2017" ~ as_date("2017-01-12"),
        SamplingEvent == "Jan 24-25, 2017" ~ as_date("2017-01-25"),
        SamplingEvent == "Jan 31-Feb 1, 2017" ~ as_date("2017-02-01"),
        SamplingEvent == "Feb 14-15, 2017" ~ as_date("2017-02-15"),
        SamplingEvent == "Mar 1-2, 2017" ~ as_date("2017-03-02"),
        SamplingEvent == "Mar 15-16, 2017" ~ as_date("2017-03-16"),  
        SamplingEvent == "Mar 28-29, 2017" ~ as_date("2017-03-29"),
        SamplingEvent == "Apr 11-12, 2017" ~ as_date("2017-04-12"),
        SamplingEvent == "Apr 25-26, 2017" ~ as_date("2017-04-26")
      ),
      WeeksFlood = (as.numeric(SampleDate - as_date("2017-01-09"))/7)
    ) %>% 
    select(
      Segment,
      Analyte,
      NetLoad,
      LoadUnits,
      WeeksFlood
    ) %>% 
    group_nest(Segment, Analyte) %>% 
    mutate(
      model = map(data, ~ summary(lm(NetLoad ~ WeeksFlood, data = .x))),
      Rsquared = signif(map_dbl(model, ~glance(.x)$r.squared*100), 3),
      p_value = signif(map_dbl(model, ~glance(.x)$p.value), 2),
      plot = pmap(
        list(data, Analyte, Rsquared, p_value),
        NetLoad.TFlood_Plot
      )
    ) %>% 
    select(Segment, Analyte, plot)
  
  # Setup plotting order
  NetLoadTFlood <- sort(unique(NetLoads_wTFlood_plots$Analyte))
  NetLoadTFlood.Order <- NetLoadTFlood[c(2:4,1,5,6)]
  
  NetLoads_wTFlood_plots <- NetLoads_wTFlood_plots %>% 
    mutate(Analyte = factor(Analyte, levels = NetLoadTFlood.Order)) %>% 
    arrange(Segment, Analyte)
  
  # Nest df's one level further and apply plot grouping function
  NetLoads_wTFlood_plots <- NetLoads_wTFlood_plots %>% 
    group_nest(Segment) %>% 
    mutate(
      plot.group = map2(
        data,
        Segment,
        Group.NetLoadTFlood.plots
      )
    )
  
  # Reorder df to print off plots in proper order
  NetLoads_wTFlood_plots <- NetLoads_wTFlood_plots[c(1,3,2),]
    
# Compare our net tMeHg loads vs Total Input Flow plots to Chris Foe's plot
  # Bring in Chris Foe's net load and flow data for the 2006 flood event
  yb2006 <- read_excel("Loads/YB_NetMeHgLoad_and_Flow_2006.xlsx") %>% 
    mutate(
      SampleDate = as_date(SampleDate),
      Study = "Chris Foe (2006)"
    ) %>% 
    select(-SampleDate)
  
  # Modify NetLoads_wFlow df so it can be bound to yb2006 df
  NetLoads_wFlow_tMeHg <- NetLoads_wFlow %>% 
    select(-c(SamplingEvent, AnalyteGroup, AnalyteGroup2)) %>% 
    filter(
      Year == 2017,
      Analyte == "MeHg- total",
      Segment != "Below Liberty - Stairsteps"
    ) %>% 
    mutate(Study = "MME Group (2017)") %>% 
    split(.$Segment, drop = TRUE) %>% 
    map(~ bind_rows(.x, yb2006))
  
  # Create a function for net tMeHg loads vs Total Input Flow scatterplots to compare two studies
  NetLoad.Flow_StComp_Plot <- function(df, stitle) {
    p <- 
      ggplot(
        data = df,
        aes(
          x = TotalInputFlow,
          y = NetLoad,
          color = Study
        )
      ) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) + 
      labs(
        title = "Net MeHg (total) Loads vs. Input Flows Scatterplots -\nComparison of MME data with Chris Foe's data",
        subtitle = paste0("Net Load for MME data = ", stitle, "\nJust 2017 Sampling Events"),
        x = "Daily Average Input Flow (cfs)",
        y = "Net Load (g/day)"
      )
    
    return(p)
  }

# Print Net Load plots to a pdf file
pdf(file = "Loads/Net_Loads.pdf", w = 13, h = 8.5)
  # Net load plots- All Flood Events, one page for each Segment
  NetLoads_plot %>% 
    split(.$Segment) %>% 
    imap(NetLoad.byAna_Plot)
  
  # Net load comparison plots- Dodged by Segment (Stairsteps - Inlet vs. Below Liberty - Inlet)
  map2(
    NetLoads_list$dfs,
    NetLoads_list$titles,
    LoadPlotsByGroup2, 
    y_name = NetLoad,
    FillGroup = Segment, 
    Dodged = TRUE
  )
  
  # Net load comparison plots- Dodged by Segment, each AnalyteGroup2 on a different page
  for (p in NetLoads_plot_AG2$plots) {
    print(p)
  }
  
  # Net load plots- Dodged by Segment (all three)
  NetLoads_plot %>% 
    filter(
      AnalyteGroup %in% c(
        "THg", 
        "MeHg", 
        "Organic Carbon", 
        "Suspended Sediment"
      ),
      Year == 2017,
      SamplingEvent != "Apr 11-12, 2017"
    ) %>% 
    LoadPlotsByGroup2(
      PlotTitle = "Net Loads for each Segment", 
      y_name = NetLoad,
      FillGroup = Segment,
      Dodged = TRUE
    )
  
  # Net load plots- Dodged by Segment (all three), each AnalyteGroup2 on a different page
  for (p in NetLoads_plot_AG2_bySeg$plots) {
    print(p)
  }

  # Net Load vs. Total Input Flow scatterplots
  for (p in NetLoads_wFlow_plots$plot.group) {
    print(p)
  }
  
  # Net Load vs. Time Inundated scatterplots
  for (p in NetLoads_wTFlood_plots$plot.group) {
    print(p)
  }
  
  # Comparison of MME data with Chris Foe's data
  imap(NetLoads_wFlow_tMeHg, NetLoad.Flow_StComp_Plot)
    
dev.off()


# 3. Calculate a few summary statistics of the loads -------------------------

# Bring in Summary Stats script
source("../../General_R_Code/Summary_Stats_1or2_Groups.R")

# Inlet Loads 
InletLoads2 <- Loads_plot %>% 
  filter(LocType == "Inlet") %>% 
  filter(
    AnalyteGroup %in% c("MeHg", "THg") |
    Analyte == "TSS"
  ) %>% 
  select(-c(Year, LocType, AnalyteGroup:AnalyteGroup2))

# Summary Statistics for the Inlet Loads
SummStat(InletLoads2, Load, StationName, Analyte) %>%
  write_excel_csv("Inlet_Summary.csv")

TotalInletLoads <- LoadsSummary_plot %>%
  filter(LocType == "Inlet") %>% 
  filter(
    AnalyteGroup %in% c("MeHg", "THg") |
    Analyte == "TSS"
  ) %>% 
  select(-c(Year, AnalyteGroup:LocType))

InletLoads_Per <- InletLoads2 %>% 
  left_join(TotalInletLoads) %>% 
  mutate(PerLoad = Load/TotalLoad)

InletLoads_Per %>%
  select(-c(Load:TotalLoad)) %>%
  spread(StationName, PerLoad) %>%
  write_excel_csv("InletLoads_per.csv", na = "0")

# Summary Statistics for the Inlet Load Percentages
SummStat(InletLoads_Per, PerLoad, StationName, Analyte) %>%
  write_excel_csv("Inlet_Summary_Per.csv")

# Summary Statistics for the Inlet Loads Fraction Percentages of Hg
FracHg %>%
  filter(LocType == "Inlet") %>%
  unite(AnalyteGroup, Fraction, col = "Analyte", sep = "- ") %>% 
  SummStat(Percent, StationName, Analyte) %>% 
  separate(Analyte, into = c("AnalyteGroup", "Fraction"), sep = "- ") %>% 
  write_excel_csv("Inlet_Summary_PerFrac.csv")
  
# Summary Statistics for the Net Loads
NetLoads_plot %>% 
  SummStat(NetLoad, Segment, Analyte) %>% 
  write_excel_csv("NetLoads_Summary.csv")


