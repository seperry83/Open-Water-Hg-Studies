# Yolo Bypass Inlet-Outlet Study
# Purpose: A script that calculates the net loads for each reach (Upper, Liberty, and the 
# entire Bypass) and formats it to be used in further calculations. 
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(openwaterhg)

# Calculate total loads for each LocType
loads_total <- loads_calc %>% 
  # Group and sum load data
  group_by(SamplingEvent, Year, LocType, Analyte, LoadUnits) %>% 
  summarize(
    total_load = sum(Load),
    digits = min(digits)
  ) %>% 
  ungroup()

# Pull out digits variable to be joined after calculating net loads
# Nest by LocType
s_digits_nest_lt <- loads_total %>% 
  select(SamplingEvent, LocType, Analyte, digits) %>% 
  group_nest(LocType) %>% 
  arrange(LocType)

# Make a nested df and summarize the minimum number of digits for each reach, sampling event,
# analyte combination
s_digits_nest_r <- 
  tibble(
    Reach = c("Upper", "Liberty", "Entire"),
    df = list(
      bind_rows(s_digits_nest_lt$data[[2]], s_digits_nest_lt$data[[3]]),
      bind_rows(s_digits_nest_lt$data[[1]], s_digits_nest_lt$data[[3]]),
      bind_rows(s_digits_nest_lt$data[[1]], s_digits_nest_lt$data[[2]])
    )
  ) %>% 
  mutate(
    df_digits = map(df, ~group_by(.x, SamplingEvent, Analyte) %>% 
      summarize(digits = min(digits)) %>% 
      ungroup()
    )
  ) %>% 
  select(-df)

# Calcualate net loads for each Reach
loads_net <- loads_total %>% 
  select(-digits) %>% 
  pivot_wider(names_from = LocType, values_from = total_load) %>% 
  rename(below_liberty = "Below Liberty") %>% 
  # Calculate net loads for each reach
  mutate(
    Upper = Outlet - Inlet,
    Liberty = below_liberty - Outlet,
    Entire = below_liberty - Inlet
  ) %>% 
  select(-c(Inlet:below_liberty)) %>% 
  pivot_longer(
    cols = Upper:Entire,
    names_to = "Reach",
    values_to = "net_load"
  ) %>% 
  filter(!is.na(net_load)) %>% 
  # Add minimum number of digits
  group_nest(Reach) %>% 
  left_join(s_digits_nest_r) %>% 
  mutate(df_final = map2(data, df_digits, .f = left_join)) %>% 
  select(Reach, df_final) %>% 
  unnest(df_final)

# Clean up
rm(loads_total, s_digits_nest_lt, s_digits_nest_r)

