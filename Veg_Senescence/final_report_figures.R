# Vegetation Sensecence Studies
# Purpose: Figures for Final Report
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(RColorBrewer)
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
# Bar plot of means of each treatment with error bars indicating their standard deviations
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

# Create figure 3
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
    x = NULL,
    y = "fMeHg (ng) +/- SD"
  ) +
  theme_owhg() +
  geom_text(
    data = vss_pilot2015_text,
    aes(
      x = combine_group,
      y = y_pos,
      label = label
    ),
    size = 3
  )
  
# Export figure 3
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
# Bar plot of means of each treatment with error bars indicating their standard deviations
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

# Create figure 8
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
  theme_owhg() +
  guides(fill = "none")

# Export figure 8
ggsave(
  "VSS_final_report_fig8.jpg", 
  plot = vss_2017_fig8,
  dpi = 300,
  width = 5, 
  height = 4, 
  units = "in"
)


# Figure 10 ---------------------------------------------------------------
# 2019 Vegetation Senescence Study
# Bar plot of means of each treatment with error bars indicating their standard deviations
# Means are the concentration in ng/L/day of filtered MeHg in the overlying water
# Facets for each Week of sample collection

# Import Data
vss_2019_orig <- read_excel(path = paste0(sharepoint_path, "/VegSens_Feb2019_Conc_Data.xlsx"), sheet = "Normal Water Data- R")

# Prepare data for figure
vss_2019_clean <- vss_2019_orig %>% 
  # Extract date from dttm variable
  mutate(SampleDate = as_date(SampleDate)) %>%
  # Remove Ice Chest Blanks and only keep MeHg- filtered data  
  filter(
    !str_detect(StationName, "Blank"),
    Analyte == "MeHg- filtered"
  ) %>% 
  # Create a new variable Conc, which is a numeric version of Result with the MDL and RL for the ND values
  add_num_result() %>% 
  # Divide Conc by 4 to convert to ng/L/day
  mutate(Conc = signif(Conc/4, 2)) %>% 
  # Create Treatment and Week variables
  mutate(
    Treatment = str_sub(StationName, end = -3),
    Week = case_when(
      SampleDate == "2019-02-19" ~ "Week 2",
      SampleDate == "2019-03-05" ~ "Week 4"
    )
  ) %>%
  # Calculate means and standard deviations
  group_by(Treatment, Week) %>% 
  summarize(
    avg = signif(mean(Conc), 2),
    stdev = signif(sd(Conc), 2)
  ) %>% 
  ungroup()

# Create figure 10
vss_2019_fig10 <- vss_2019_clean %>% 
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
  theme_owhg() +
  guides(fill = "none")

# Export figure 10
ggsave(
  "VSS_final_report_fig10.jpg", 
  plot = vss_2019_fig10,
  dpi = 300,
  width = 5, 
  height = 2.5, 
  units = "in"
)


# Figures 12 and 13 -------------------------------------------------------
# 2018 Vegetation Senescence Lab Study
# Bar plot of means of each treatment with error bars indicating their standard deviations
# Means are the concentration in ng/L/day of filtered MeHg in the overlying water
# Figure 12 is for Week 4 results, and Figure 13 is for Week 8

# Import Data
vss_2018_orig <- read_excel(path = paste0(sharepoint_path, "/VegSensLab_Oct2018_Conc_Data.xlsx"), sheet = "Normal Water Data- R")

# Create named vector to use in shortening StationName
sn_repl <- c("Vegetation" = "Veg", "Manure" = "Man", "Sediment" = "Sed", "High" = "H", "Medium" = "M", "Low" = "L")

# Prepare data for figures
vss_2018_clean <- vss_2018_orig %>% 
  # Extract date from dttm variable
  mutate(SampleDate = as_date(SampleDate)) %>%
  # Create Week variable and shorten StationName
  mutate(
    Week = case_when(
      SampleDate == "2018-11-13" ~ "Week 4",
      SampleDate == "2018-12-11" ~ "Week 8",
      TRUE ~ "other"
    ),
    StationName = str_replace_all(StationName, sn_repl)
  ) %>% 
  # Only keep MeHg- filtered and Weeks 4 and 8 data
  filter(
    Analyte == "MeHg- filtered",
    Week != "other"
  ) %>% 
  # Create a new variable Conc, which is a numeric version of Result with the MDL and RL for the ND values
  add_num_result() %>% 
  # Divide Conc by 4 to convert to ng/L/day
  mutate(Conc = signif(Conc/4, 2)) %>% 
  # Create Treatment and Group variables
  mutate(
    Treatment = str_sub(StationName, end = -3),
    Treatment = case_when(
      LandMgmtType == "Disked" & VegLevel == "Low" ~ "Veg L, Disked",
      LandMgmtType == "Disked" & VegLevel == "Medium" ~ "Veg M, Disked",
      LandMgmtType == "Disked" & VegLevel == "High" ~ "Veg H, Disked",
      LandMgmtType == "Grazed" & VegLevel == "Low" ~ "Veg L, Grazed",
      LandMgmtType == "Grazed" & VegLevel == "Medium" ~ "Veg M, Grazed",
      LandMgmtType == "Grazed" & VegLevel == "High" ~ "Veg H, Grazed",
      LandMgmtType == "Ungrazed" & VegLevel == "Low" ~ "Veg L, Ungrazed",
      LandMgmtType == "Ungrazed" & VegLevel == "Medium" ~ "Veg M, Ungrazed",
      LandMgmtType == "Ungrazed" & VegLevel == "High" ~ "Veg H, Ungrazed",
      TRUE ~ Treatment
    ),
    Group = case_when(
      str_detect(Treatment, "^Control") ~ "Control",
      str_detect(Treatment, "^Sieved") ~ "Sed Only",
      str_detect(Treatment, "^Man") ~ "Manure, No Veg",
      str_detect(Treatment, "^Veg L") ~ "Veg L",
      str_detect(Treatment, "^Veg M") ~ "Veg M",
      str_detect(Treatment, "^Veg H") ~ "Veg H"
    )
  ) %>% 
  # Calculate means and standard deviations
  group_by(Group, Treatment, Week) %>% 
  summarize(
    avg = signif(mean(Conc), 2),
    stdev = signif(sd(Conc), 2)
  ) %>% 
  ungroup()

# Define custom order for figures
  # Group order
  group_order <- c(
    "Control", 
    "Sed Only", 
    "Manure, No Veg", 
    "Veg L", 
    "Veg M", 
    "Veg H"
  )
  
  # Treatment order
  treatment_order <- c(
    "Control Water", 
    "Sieved Sed Only", 
    "Man L, Sieved Sed", 
    "Man M, Sieved Sed", 
    "Man H, Sieved Sed", 
    "Veg L, No Sed", 
    "Veg L, Disked", 
    "Veg L, Grazed", 
    "Veg L, Ungrazed",
    "Veg M, No Sed", 
    "Veg M, Disked", 
    "Veg M, Grazed", 
    "Veg M, Ungrazed",
    "Veg H, No Sed", 
    "Veg H, Disked", 
    "Veg H, Grazed", 
    "Veg H, Ungrazed"
  )
  
  # Apply factor order to vss_2018_clean
  vss_2018_clean <- vss_2018_clean %>% 
    mutate(
      Group = factor(Group, group_order),
      Treatment = factor(Treatment, treatment_order)
    )

# Define custom colors for each Group in the figures
group_colors <- c(
  "Control" = "gray50", 
  "Sed Only" = "tan4", 
  "Manure, No Veg" = "darkorange", 
  "Veg L" = brewer.pal(9,"Greens")[4],
  "Veg M" = brewer.pal(9,"Greens")[6],
  "Veg H" = brewer.pal(9,"Greens")[8]
)

# Create function for figures 12 and 13
barplot_vss_2018 <- function(df) {
  p <- 
    ggplot(
      data = df,
      aes(
        x = Treatment, 
        y = avg, 
        fill = Group
      )
    ) +
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
    theme_owhg(x_axis_v = TRUE) +
    theme(legend.position = c(0.18, 0.7)) +
    scale_fill_manual(
      name = NULL,
      values = group_colors
    )
  
  return(p)
}

# Create figures 12 and 13
vss_2018_fig12_13 <- vss_2018_clean %>% 
  group_nest(Week) %>% 
  mutate(figures = map(data, .f = barplot_vss_2018))

# Export figure 12
ggsave(
  "VSS_final_report_fig12.jpg", 
  vss_2018_fig12_13$figures[[1]],
  dpi = 300,
  width = 6, 
  height = 5, 
  units = "in"
)

# Export figure 13
ggsave(
  "VSS_final_report_fig13.jpg", 
  vss_2018_fig12_13$figures[[2]],
  dpi = 300,
  width = 6, 
  height = 5, 
  units = "in"
)


