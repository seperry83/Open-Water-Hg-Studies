# Yolo Bypass Inlet-Outlet Study
# EDA plots of the Field Measurements collected in 2017 during the Yolo Bypass flood event

library(tidyverse)
library(readxl)
library(lubridate)

# Bring in data
FieldM <- read_excel(path = "FieldMeasurements/FieldMeasurements.xlsx") %>% 
  # Create date column with just date
  mutate(date=as_date(Date)) %>% 
  # Pivot df to long format
  pivot_longer(
    cols = 'Water Temperature':Turbidity,
    names_to = "coc",
    values_to = "Result"
  ) %>% 
  # Add some new variables
  mutate(
    RL = 1,
    nd = as.numeric(grepl('<', Result)), 
    conc = if_else(
      nd == 1, 
      as.numeric(RL), 
      as.numeric(Result)
    ),
    units = case_when(
      str_detect(coc, "^W") ~ "Degrees C",
      str_detect(coc, "^S") ~ "uS/cm",
      str_detect(coc, "^D") ~ "mg/L",
      str_detect(coc, "T") ~ "NTU"
    ),
    Year = year(date)
  ) %>% 
  select(-c(DateTime, Date)) %>% 
  filter(!is.na(conc))

# Bring in Key for short Station Names
StationNameKey <- read_excel(path = 'FieldMeasurements/StationNameKey_forPlots.xlsx')

# Perform left join to add short Station Names to Field Measurement data
FieldM <- left_join(FieldM, StationNameKey)

# Setup plotting order
Sta.short <- sort(unique(FieldM$locid))
Sta.short.Order <- Sta.short[c(5:7,14,2:4,8,12,18,16,17,15,11,9,13,1,10)]

FieldM <- FieldM %>% 
  mutate(locid = factor(locid, levels = Sta.short.Order))

# Load TS plot script
source("C:/Users/dboswort/OneDrive - California Department of Water Resources/Stats/R_Software/Basic_Stats_Course/explore_tsplot_grid.R")  

# Create Time Series plots by Analyte for just 2017 sampling events
pdf(file = "FieldMeasurements/2017_FieldMeas_TS.pdf", w=11, h=8.5)
FieldM %>%
  filter(Year == 2017) %>% 
  group_by(coc) %>% 
  do(plot=explore_tsplot_grid(.,header.clip = .$coc[1],showlim = F))
dev.off()

# Create Boxplots by Analyte for all sampling events
pdf(file = "FieldMeasurements/FieldMeas_Boxplots.pdf", w=11, h=8.5)
FieldM %>% 
  mutate(Year = as.character(Year)) %>%
  group_by(coc) %>% 
  do(plot = {
    print(.$coc[1])
    p <- 
      ggplot(
        data = .,
        aes(
          x = locid, 
          y = conc
        )
      ) + 
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(  # Add jitter points
        width=0.25,
        aes(color = Year)
      ) +  
      labs(
        title = paste0('Boxplot for ',.$coc[1]),
        subtitle = "All Sampling Events",
        x = 'Station',
        y = paste0('Concentration (',.$units[1],')')
        ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  #vertical x-axis labels
    
    print(p)
  })
dev.off()

# Create Histograms for just 2017 sampling events
pdf(file = "FieldMeasurements/2017_FieldMeas_HistPlots.pdf", w=11, h=8.5)
FieldM %>% 
  filter(Year == 2017) %>% 
  group_by(coc) %>% 
  do(plot = {
    print(.$coc[1])
    p <- 
      ggplot(
        data = .,
        aes(x = conc)
      ) + 
      geom_histogram(bins = 5) +
      facet_wrap(vars(locid)) +
      labs(
        title = paste0('Histograms for ', .$coc[1]),
        subtitle = "Just 2017 Sampling Events",
        x = paste0('Concentration (', .$units[1], ')'),
        y = 'Count'
      )
    
    print(p)
  })
dev.off()


