# Yolo Bypass Inlet-Outlet Study
# Purpose: A script that calculates the net loads for each reach (Upper, Liberty, and the 
# entire Bypass) and formats it to be used in further calculations. 
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(openwaterhg)

# Calculate total loads for each LocType
temp_loads_total <- loads_calc %>% 
  # Group and sum load data
  group_by(SamplingEvent, Year, LocType, Analyte, LoadUnits) %>% 
  summarize(
    total_load = sum(Load),
    digits = min(digits)
  ) %>% 
  ungroup()

# Pull out digits variable to be joined after calculating net loads
# Nest by LocType
temp_s_digits_nest_lt <- temp_loads_total %>% 
  select(SamplingEvent, LocType, Analyte, digits) %>% 
  group_nest(LocType) %>% 
  arrange(LocType)

# Make a nested df and summarize the minimum number of digits for each reach, sampling event,
# analyte combination
temp_s_digits_nest_r <- 
  tibble(
    Reach = c("Upper", "Liberty", "Entire"),
    df = list(
      bind_rows(temp_s_digits_nest_lt$data[[2]], temp_s_digits_nest_lt$data[[3]]),
      bind_rows(temp_s_digits_nest_lt$data[[1]], temp_s_digits_nest_lt$data[[3]]),
      bind_rows(temp_s_digits_nest_lt$data[[1]], temp_s_digits_nest_lt$data[[2]])
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
loads_net <- temp_loads_total %>% 
  select(-digits) %>% 
  pivot_wider(names_from = LocType, values_from = total_load) %>% 
  rename(below_liberty = "Below Liberty") %>% 
  # Calculate net loads for each reach
  mutate(
    Upper = Outlet - Inlet,
    Liberty = below_liberty - Outlet,
    Entire = below_liberty - Inlet
  ) %>% 
  select(-c(Inlet, Outlet, below_liberty)) %>% 
  pivot_longer(
    cols = Upper:Entire,
    names_to = "Reach",
    values_to = "net_load"
  ) %>% 
  filter(!is.na(net_load)) %>% 
  # Add minimum number of digits
  group_nest(Reach) %>% 
  left_join(temp_s_digits_nest_r) %>% 
  mutate(df_final = map2(data, df_digits, .f = left_join)) %>% 
  select(Reach, df_final) %>% 
  unnest(df_final)

# Clean up
rm(temp_loads_total, temp_s_digits_nest_lt, temp_s_digits_nest_r)

