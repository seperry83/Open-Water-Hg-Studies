# Vegetation Sensecence Studies
# Purpose: Figures for Final Report
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(readxl)
library(openwaterhg)

# Datasets are on SharePoint site for the Open Water Final Report
# Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR Documents - Open Water Final Report - Documents/Technical Appendices/Vegetation Senscence/Veg Sens. data and graphs"
  )
)  

# Figure 3 ----------------------------------------------------------------
# 2015 Pilot Study
# Bar plot of means of 4 groups with error bars indicating their standard deviations
# Means are of the mass in ng of filtered MeHg in the overlying water after 12 days of incubation

# Import Data
vss_pilot2015_orig <- read_excel(path = paste0(sharepoint_path, "/VegSens_PilotStudies_Data.xlsx"), sheet = "Dec2015")

# Prepare data for figure
vss_pilot2015_clean <- vss_pilot2015_orig %>% 
  # Just analyze Day 12 results due to issues with repeated measurements
  filter(Day == 12) %>% 
  # Create a variable CombineGroup that is a combination of Treatment and Aeration variables
  mutate(
    combine_group = case_when(
      Treatment == "Vegetated" & Aeration == "Yes" ~ "Veg with Air",
      Treatment == "Vegetated" & Aeration == "No" ~ "Veg without Air",
      Treatment == "Sediment Only" & Aeration == "Yes" ~ "Sed with Air",
      Treatment == "Sediment Only" & Aeration == "No" ~ "Sed without Air"
    )
  ) %>% 
  # Calculate means and standard deviations
  group_by(combine_group) %>% 
  summarize(
    avg = mean(dMeHg),
    stdev = sd(dMeHg)
  )

# Create df to add text labels for statistical significance to the figure
vss_pilot2015_text <- vss_pilot2015_clean %>% 
  select(-c(avg, stdev)) %>% 
  mutate(
    label = case_when(
      combine_group == "Sed with Air" ~ "A",
      combine_group == "Sed without Air" ~ "A",
      combine_group == "Veg with Air" ~ "B",
      combine_group == "Veg without Air" ~ "C"
    ),
    y_pos = case_when(
      combine_group == "Sed with Air" ~ 1.5,
      combine_group == "Sed without Air" ~ 1.5,
      combine_group == "Veg with Air" ~ 6,
      combine_group == "Veg without Air" ~ 21.5
    )
  )

# Create figure
vss_pilot2015_fig <- vss_pilot2015_clean %>% 
  ggplot(aes(x = combine_group, y = avg)) +
  geom_col(fill = "#00BE7D") +
  geom_errorbar(
    aes(
      ymin = avg - stdev, 
      ymax = avg + stdev, 
      width = 0.25
    )
  ) +
  labs(
    caption = "Letters indicate statistical significance from Tukey-HSD analysis.\nStatistics were run on values that were transformed to natural log.",
    x = NULL,
    y = "fMeHg (ng/12 days) +/- SD"
  ) +
  theme_light() +
  theme(plot.caption = element_text(size = 8)) +
  geom_text(
    data = vss_pilot2015_text,
    aes(
      x = combine_group,
      y = y_pos,
      label = label
    ),
    size = 3
  )
  
# Export figure
ggsave(
  "VSS_final_report_fig3.jpg", 
  plot = vss_pilot2015_fig,
  dpi = 300,
  width = 4.5, 
  height = 3.5, 
  units = "in"
)

