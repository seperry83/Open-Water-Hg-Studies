# Yolo Bypass Inlet-Outlet Study

# Summarize Field QA samples including Field Blanks, Filter Blanks, and Field Duplicates

library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)


# Field and Filter Blanks -------------------------------------------------

# Import Data
blanks <- read_excel("../../../Data/Lab_Final/YB_Inlet-Outlet_Conc_Data.xlsx", sheet = "Field and Filter Blanks") %>% 
  clean_names(case = "upper_camel") %>% 
  select(SampleCode:Units) %>% 
  mutate(
    SampleDate = as_date(SampleDate), 
    CollectionTimePst = hms::as_hms(CollectionTimePst)
  ) %>% 
  # remove a few samples that we don't want included in the summary
  filter((SampleDate >= "2014-12-22" & SampleDate <= "2016-03-17") | SampleDate >= "2017-01-11") %>% 
  filter(!(StationName == "Field Blank" & Analyte =="DOC"))

# Modify blanks df to count number of detected blanks
blanks <- blanks %>% 
  mutate(
    ResDet = if_else(
      str_detect(Result, "<"),
      "ND",
      "D"
    )
  )

# Count all samples
blanks.c.all <- blanks %>% 
  count(StationName, Analyte, Units) %>% 
  rename(N.all = n)

# Count # of detected blanks
blanks.c.det <- blanks %>% 
  filter(ResDet == "D") %>% 
  count(StationName, Analyte) %>% 
  rename(N.det = n)

# Make a table of RL and MDL values
det.limits <- blanks %>% 
  count(Analyte, Rl, Mdl) %>% 
  select(-n) %>% 
  filter(!(Analyte == "UVA 254" & Rl == 0.001))

# Summarize min and max values for just detected blanks
det.summ <- blanks %>% 
  filter(ResDet == "D") %>%
  mutate(Result = as.numeric(Result)) %>% 
  group_by(StationName, Analyte) %>% 
  summarize(
    Det.Min = min(Result),
    Det.Max = max(Result),
    Det.Med = median(Result)
  ) %>% 
  ungroup()

# Combine all df's together to make a summary table for report
blanks.summ <- blanks.c.all %>% 
  left_join(det.limits) %>% 
  left_join(blanks.c.det) %>% 
  left_join(det.summ) %>% 
  replace_na(list(N.det = 0)) %>% 
  mutate(Per.Det = N.det/N.all) %>% 
  select(
    StationName,
    Analyte,
    Units,
    Rl,
    Mdl,
    N.all,
    N.det,
    Per.Det,
    Det.Min,
    Det.Max,
    Det.Med
  )
    
# Export blanks.summ df
blanks.summ %>% write_excel_csv("Blanks_Summ.csv", na = "")


# Field Duplicates --------------------------------------------------------

# Import Data
fdups <- read_excel("../../../Data/Lab_Final/YB_Inlet-Outlet_Conc_Data.xlsx", sheet = "Field Duplicates") %>% 
  clean_names(case = "upper_camel") %>% 
  select(
    StationName,
    SampleDate,
    Analyte,
    ResultParentSample,
    ResultFieldDup,
    Rpd,
    Units,
    Rl
  ) %>% 
  mutate(SampleDate = as_date(SampleDate)) %>% 
  # remove a few samples that we don't want included in the summary
  filter(!str_detect(StationName, "^YB")) %>% 
  filter(SampleDate != "2014-12-12")

# Count the total # of Field Dups for each Analyte - before removing < MDL or < RL values
fdups.c.all <- fdups %>% 
  count(Analyte) %>% 
  rename(N.fd = n)

# Clean up fdups df for further analysis
fdups <- fdups %>% 
  # don't include ND values in the analysis
  filter(!(str_detect(ResultParentSample, "<") | str_detect(ResultFieldDup, "<"))) %>% 
  # convert a few character variables to numeric
  mutate(
    Rpd = signif(as.numeric(Rpd), 3),
    ResultParentSample = signif(as.numeric(ResultParentSample), 3),
    ResultFieldDup = signif(as.numeric(ResultFieldDup), 3)
  )

# Summarize RPD values for each Analyte
rpd.summ <- fdups %>% 
  group_by(Analyte) %>% 
  summarize(
    Min.rpd = min(Rpd),
    Max.rpd = max(Rpd),
    Med.rpd = median(Rpd)
  ) %>% 
  ungroup()

# Count # of RPD values greater than 25% and one or both of the values is greater than 10x the RL
rpd.large.c <- fdups %>% 
  mutate(
    ValQual = if_else(
      ResultParentSample > 10*Rl | ResultFieldDup > 10*Rl,
      "greater",
      "less"
    ),
    Rpd.large = if_else(
      Rpd >= 0.25 & ValQual == "greater",
      "yes",
      "no"
    )
  ) %>% 
  filter(Rpd.large == "yes") %>% 
  count(Analyte) %>% 
  rename(N.rpd.large = n)

# Join df together to make a summary table for report
fdups.summ <- fdups.c.all %>%
  left_join(rpd.summ) %>% 
  left_join(rpd.large.c) %>% 
  replace_na(list(N.rpd.large = 0)) %>% 
  mutate(Per.rpd.large = N.rpd.large/N.fd)
  
# Export fdups.summ df
fdups.summ %>% write_excel_csv("FieldDups_Summ.csv")


