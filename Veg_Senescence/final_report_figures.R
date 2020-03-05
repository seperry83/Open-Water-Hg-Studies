# Vegetation Sensecence Studies
# Purpose: Figures for Final Report
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)
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
# Means are the mass in ng of filtered MeHg in the overlying water after 12 days of incubation

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
vss_pilot2015_fig3 <- vss_pilot2015_clean %>% 
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
  plot = vss_pilot2015_fig3,
  dpi = 300,
  width = 4.5, 
  height = 3.5, 
  units = "in"
)


# Figure 8 ----------------------------------------------------------------
# 2017 Vegetation Senescence Study
# Bar plot of means of 3 groups with error bars indicating their standard deviations
# Means are the concentration in ng/L/day of filtered MeHg in the overlying water
# Facets for each Week of sample collection

# Import Data
vss_2017_orig <- read_excel(path = paste0(sharepoint_path, "/VegSens_Dec2017_Conc_Data.xlsx"), sheet = "Normal Water Data- R")

# Prepare data for figure
vss_2017_clean <- vss_2017_orig %>% 
  # Extract date from dttm variable
  mutate(SampleDate = as_date(SampleDate)) %>%
  # Remove Ice Chest Blank and Devegetated Treatments  
  filter(!str_detect(StationName, "Blank|Devegetated")) %>% 
  # Only keep MeHg- filtered data and Before Water Change samples
  filter(Analyte == "MeHg- filtered", SampleTiming == "Before water change") %>% 
  # Create a new variable Conc, which is a numeric version of Result with the MDL and RL for the ND values
  add_num_result() %>% 
  # Divide Conc by 4 to convert to ng/L/day
  mutate(Conc = signif(Conc/4, 2)) %>% 
  # Create Treatment and Week variables
  mutate(
    Treatment = str_sub(StationName, end = -3),
    Week = case_when(
      SampleDate == "2017-12-06" ~ "Week 1",
      SampleDate == "2017-12-13" ~ "Week 2",
      SampleDate == "2017-12-20" ~ "Week 3",
      SampleDate == "2018-01-03" ~ "Week 5"
    )
  ) %>% 
  # Calculate means and standard deviations
  group_by(Treatment, Week) %>% 
  summarize(
    avg = signif(mean(Conc), 2),
    stdev = signif(sd(Conc), 2)
  ) %>% 
  ungroup()

# Create figure
vss_2017_fig8 <- vss_2017_clean %>% 
  ggplot(aes(x = Treatment, y = avg, fill = Treatment)) +
  geom_col() +
  geom_errorbar(
    aes(
      ymin = avg - stdev, 
      ymax = avg + stdev, 
      width = 0.25
    )
  ) +
  labs(
    x = NULL,
    y = "fMeHg (ng/L/day) +/- SD"
  ) +
  facet_wrap(vars(Week)) +
  add_gen_color_pal(3, "fill") +
  theme_light() +
  theme(strip.text = element_text(color = "black")) +
  guides(fill = "none")

# Export figure
ggsave(
  "VSS_final_report_fig8.jpg", 
  plot = vss_2017_fig8,
  dpi = 300,
  width = 5, 
  height = 4, 
  units = "in"
)
