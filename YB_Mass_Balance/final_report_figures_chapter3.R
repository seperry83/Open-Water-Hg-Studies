# Yolo Bypass Mass Balance Study
# Purpose: Figures for Final Report - Chapter 3
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(scales)
library(openwaterhg)


# Figure 3-5 --------------------------------------------------------------
# Line plot showing daily average flow in cfs for the Fremont Weir and the CCSB for the 2017 flood event
# Red markers indicate the sampling events
# Hydrographs arranged horizontally

daily_flow_data_all


# Figure 3-6 --------------------------------------------------------------
# Filled bar plot showing percentage of total inflow for each inlet for the 2017 sampling events

daily_flow_data_se


# Figure 3-8 --------------------------------------------------------------
# Filled bar plot showing percentage of total input load for each inlet for the 2017 sampling events

loads_calc


# Figure 3-10 -------------------------------------------------------------
# Filled bar plot showing percentage of filtered and particulate MeHg fractions for the 2017 sampling events
# Facets for each inlet

loads_calc


# Figure 3-12 -------------------------------------------------------------
# Bar plots showing export loads at the Stairsteps for the 2017 sampling events
# Facets for each analyte/parameter

loads_calc


# Figure 3-13 -------------------------------------------------------------
# Bar plots showing net loads of the upper and lower reaches for the 2017 sampling events
# Upper reach is between the inlets to the Stairsteps
# Lower reach is between the Stairsteps and Below Liberty Island
# Facets for each analyte/parameter

loads_calc
