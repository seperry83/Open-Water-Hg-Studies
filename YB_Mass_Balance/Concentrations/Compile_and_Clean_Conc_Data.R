# Yolo Bypass Inlet-Outlet Study
# Purpose: Compile, combine, and clean all of the concentration data
# Author: Dave Bosworth

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(openwaterhg)


# Contract Lab Data -------------------------------------------------------

# Define paths for data files
mlml_path <- "M:/Data/Lab/MLML/Open_Water/YB_In-Out_Study"
pnnl_path <- "M:/Data/Lab/PNNL"
  
# Create character vectors of all data files
mlml_files <- dir(mlml_path, pattern = "\\.xls$", recursive = T, full.names = T)
pnnl_files <- dir(pnnl_path, pattern = "SWAMP.xlsx$", full.names = T)
  
# Remove some of the files from the vectors
mlml_files <- mlml_files[!str_detect(mlml_files, "2014")] #Remove 2014 data file since it's in the wrong format
pnnl_files <- pnnl_files[!str_detect(pnnl_files, "QA_Samples")] #Remove QA data file
  
# Combine all of the data
mlml_data_orig <- map_dfr(mlml_files, read_excel)
pnnl_data_orig <- map_dfr(pnnl_files, read_excel)
  
# Clean up the datasets
  # MLML
  mlml_data_clean <- mlml_data_orig %>%
    # Convert some of the variables to numeric
    mutate(
      Result = as.numeric(Result),
      MDL = as.numeric(MDL),
      RL = as.numeric(RL),
      ExpectedValue = as.numeric(ExpectedValue)
    ) %>% 
    # Only keep Grab data, Field Blanks, and Filter Blanks
    filter(SampleTypeCode %in% c("Grab", "FieldBlank", "FilterBlank", "EquipBlank")) %>%
    # Remove * Stations
    filter(StationName != "*") %>%
    # Remove a couple of MS, MSD samples with SampleTypeCode of "Grab"
    filter(!str_detect(LabSampleID, "ms")) %>% 
    # Round the Result variable
    mutate(Result = signif(Result, 3))
  
  # PNNL Data
  pnnl_data_clean <- pnnl_data_orig %>% 
    # Filter out QA data
    filter(StationCode != "LABQA", StationName != "LABQA") %>%
    filter(!str_detect(SampleTypeCode, "MS") | is.na(SampleTypeCode)) %>% 
    # Round the Result variable
    mutate(Result = if_else(Result < 1000, signif(Result, 3), round(Result)))
  
# Import one MLML dataset with different format
mlml2 <- read_excel(path = "M:/Data/Lab/MLML/Open_Water/YB_In-Out_Study/MeHg/2014-Dec22_23_YB_InOutStudy_MMHg_data_forR.xlsx")

# Combine MLML and PNNL Data
contract_data <- bind_rows(mlml_data_clean, pnnl_data_clean, mlml2)
  
# Clean up contract_data df
  # ONLY NECESSARY ONCE- JUST KEPT CODE FOR FUTURE REFERENCE
    # Create a list of unique StationNames
    #StationNames <- count(ContractData, StationName)
    # Export list to a .csv file
    #write_excel_csv(StationNames, "StationNameKey_ContractLabs.csv")
    
  #Import StationName Standarized Key
  std_station_cl <- read_csv("YB_Mass_Balance/Concentrations/StationNameKey_ContractLabs.csv")

  contract_data_clean <- contract_data %>%
    # Make a new variable "Analyte" that combines AnalyteName and FractionName
    mutate(
      Analyte = case_when(
        AnalyteName == "Boron"                                                        ~ "Boron- total",
        AnalyteName == "Methylmercury" & FractionName %in% c("Total", "raw")          ~ "MeHg- total",
        AnalyteName == "Methylmercury" & FractionName %in% c("Dissolved", "filtered") ~ "MeHg- filtered",
        AnalyteName == "Iron"                                                         ~ "Iron- filtered",
        AnalyteName == "Managnese"                                                    ~ "Manganese- filtered"
      )
    ) %>% 
    # Convert Result variable to character data type
    mutate(
      Result = if_else(ResQualCode == "ND", "< MDL", as.character(Result)),
      # Create a new variable ResQual where ND = 1 and Detect = 0
      ResQual = if_else(ResQualCode == "ND", 1, 0)
    ) %>% 
    # Clean up date and time formatting
    mutate(
      SampleDate = as_date(SampleDate), 
      AnalysisDate = as_date(AnalysisDate),
      CollectionTime = hms::as_hms(CollectionTime)
    ) %>% 
    # Standardize StationNames
    left_join(std_station_cl) %>%
    # Keep necessary variables
    select(
      StationCode,
      StationNameStd,
      SampleDate,
      CollectionTime,
      LabBatch,
      AnalysisDate,
      Analyte,
      UnitName,
      Result,
      ResQual,
      MDL,
      RL,
      LabResultComments
    ) %>% 
    # Rename some of the variables (New Name = Old Name)
    rename(
      SampleCode = StationCode,
      StationName = StationNameStd,
      Units = UnitName,
      LabComments = LabResultComments
    ) %>% 
    # Add "C" to the end of the SampleCodes to be consistent with Bryte reporting
    mutate(SampleCode = if_else(SampleDate > "2014-12-31", paste0(SampleCode, "C"), SampleCode))

# Change StationNames of two boron samples that were switched
contract_data_temp <- contract_data_clean %>% 
  filter(
    SampleCode %in% c("EH0317B0545C", "EH0317B0547C"),
    Analyte == "Boron- total"
  )

contract_data_switch <- contract_data_temp %>% 
  mutate(
    StationName = if_else(
      StationName == "Field Blank", 
      "Field Duplicate Sample",
      "Field Blank"
    ),
    MME_Comments = if_else(
      StationName == "Field Blank", 
      "Field Blank originally labeled as Field Duplicate on data sheet, most likely a blank sample after looking at data from sampling event",
      "Field Duplicate originally labeled as Field Blank on data sheet, most likely an ambient sample after looking at data from sampling event"
    )
  )

contract_data_clean <- 
  anti_join(contract_data_clean, contract_data_temp) %>% 
  bind_rows(contract_data_switch)

# Create a vector of analytes for the contract labs to be used later
contract_ana <- sort(unique(contract_data_clean$Analyte))
    
# Clean up
rm(list= ls()[!(ls() %in% c("contract_data_clean", "contract_ana"))])


# Bryte Lab Data ----------------------------------------------------------

# Import Bryte Lab dataset
bryte_data_orig <- read_excel(path = "M:/Data/Lab/Bryte_Lab/Open_Water/YB_Inlet-Outlet_Data_Bryte_all.xlsx")
 
# Clean up bryte_data_orig df
  # Clean up variable names
  names(bryte_data_orig) <- str_replace_all(names(bryte_data_orig), "[:space:]", "")
  
  #Import StationName Standarized Key
  std_station_b <- read_csv("YB_Mass_Balance/Concentrations/StationNameKey_Bryte.csv")
  
  # Import Analyte name Standarized Key
  std_analyte_b <- read_csv("YB_Mass_Balance/Concentrations/AnalyteKey_Bryte.csv")
  
  bryte_data_clean <- bryte_data_orig %>% 
    # Filter data
    filter(
      # remove data earlier than first sampling event on Dec 22-23, 2014
      CollectionDate >= "2014-12-22",
      # remove some extra Hg Analyses
      Method != "EPA 200.8 (Hg Total) [1]*"
    ) %>% 
    # Standardize Analyte Names
    left_join(std_analyte_b) %>% 
    # Standardize StationNames
    left_join(std_station_b) %>% 
    # Separate Collection Date into 2 variables- one for date, other for time
    mutate(
      SampleDate = as_date(CollectionDate), 
      CollectionTime = hms::as_hms(CollectionDate)
    ) %>%
    # Indicate Field Duplicates and Companion Grab Samples in the StationNameStd variable
    mutate(
      StationNameStd = case_when(
        Description == "Field Duplicate Sample" ~ "Field Duplicate Sample",
        Description == "Companion Grab Sample"  ~ "Companion Grab Sample",
        TRUE                                    ~ StationNameStd
      )
    ) %>%
    # Keep necessary variables
    select(
      StationNameStd,
      SampleCode,
      SampleDate,
      CollectionTime,
      AnalyteStd,
      Result,
      RptLimit,
      Units,
      Method,
      ParentSample,
      n_round
    ) %>%
    # Rename some variables (New Name = Old Name)
    rename(
      StationName = StationNameStd,
      Analyte = AnalyteStd,
      RL = RptLimit
    ) %>% 
    # Clean up Result variable
    mutate(
      # Create a new variable ResQual where ND = 1 and Detect = 0
      ResQual = if_else(str_detect(Result, "^<"), 1, 0),
      # Convert Result variable to numeric
      Result = if_else(ResQual == 1, RL, as.numeric(Result)),
      # Round Result variable to specified number of digits in n_round
      Result = round(Result, n_round),
      # Convert Result variable back to character
      Result = if_else(ResQual == 1, "< RL", as.character(Result))
    )
    
# Clean up 
rm(bryte_data_orig, std_analyte_b, std_station_b)
  

# All Data ----------------------------------------------------------------

# Combine Contract and Bryte Lab Data
all_data <- bind_rows(contract_data_clean, bryte_data_clean) %>% 
  # Create a new variable Conc, which is a numeric version of Result with the MDL and RL for the ND values
  add_num_result()


# Lab Replicates ----------------------------------------------------------

# Create a df of all Lab Replicates
lab_reps <- all_data %>% 
  count(SampleCode, Analyte) %>% 
  filter(n == 2) %>% 
  select(-n)
  
# Pull out a df of all Lab Replicates only including variables that are unique in Rep1 and Rep2
lab_reps_u <- inner_join(all_data, lab_reps) %>% 
  select(SampleCode, Analyte, Result, Conc)

# Pull out a df of all Lab Replicates only including variables that are identical in Rep1 and Rep2
lab_reps_i <- inner_join(all_data, lab_reps) %>% 
  select(-c(Result, LabComments, Conc)) %>% 
  # Average ResQual variable for each Replicate group
  group_by(SampleCode, Analyte) %>% 
  mutate(ResQual = mean(ResQual)) %>% 
  ungroup() %>% 
  # Remove duplicate rows
  distinct()
  
# Pull out a df of all data not including Lab Replicates
no_lab_reps <- anti_join(all_data, lab_reps)

# Create two different df to spread out unique variables
  # Result
  S_Result <- lab_reps_u %>% 
    select(-Conc) %>%
    group_by(SampleCode, Analyte) %>% 
    mutate(Rep = paste0("Result", row_number())) %>% 
    ungroup() %>% 
    pivot_wider(names_from = Rep, values_from = Result)
    
  # Conc
  S_Conc <- lab_reps_u %>% 
    select(-Result) %>%
    group_by(SampleCode, Analyte) %>% 
    mutate(Rep = paste0("Conc", row_number())) %>% 
    ungroup() %>% 
    pivot_wider(names_from = Rep, values_from = Conc)
  
# Join all of the df back together
lab_rep_data <-
  reduce(list(S_Conc, S_Result, lab_reps_i), left_join) %>% 
  # Calculate RPD values for each replicate pair and flag if necessary
  mutate(
    rpd = round(abs(Conc1 - Conc2)/((Conc1 + Conc2)/2), 3),
    flag = case_when(
      Analyte %in% contract_ana & rpd > 0.25 & (Conc1 > 10 * MDL | Conc2 > 10 * MDL) ~ "y",
      !Analyte %in% contract_ana & rpd > 0.25 & (Conc1 > 10 * RL | Conc2 > 10 * RL) ~ "y",
      TRUE ~ "n"
    )
  )

# Export lab_rep_data to .csv file- only needed once
# lab_rep_data %>%
#   select(
#     SampleCode,
#     StationName,
#     SampleDate,
#     CollectionTime,
#     Analyte,
#     LabBatch,
#     Result1,
#     Result2,
#     rpd,
#     ResQual,
#     RL,
#     MDL,
#     Units,
#     MME_Comments,
#     flag
#   ) %>%
#   write_excel_csv("LabReplicates.csv", na = "")
    
# Modify the lab_rep_data df
lab_rep_data_mod <- lab_rep_data %>% 
  # Average Concentration values and round
  mutate(
    Conc = (Conc1 + Conc2)/2,
    Conc = case_when(
      str_detect(Analyte, "^Boron|^MeHg") ~ round(Conc, 3),
      Analyte %in% c("Iron- filtered", "Manganese- filtered") ~ signif(Conc, 3),
      TRUE ~ round(Conc, n_round)
    ),
    # Add comment about using the average of lab replicates
    MME_Comments = if_else(
      is.na(MME_Comments),
      "Average of Lab Replicates",
      paste("Average of Lab Replicates", MME_Comments, sep = "; ")
    )
  ) %>% 
  # Delete a few variables
  select(
    -c(
      Conc1, 
      Conc2,
      Result1, 
      Result2,
      rpd,
      flag
    )
  ) %>% 
  # Add Result variable back to df
  mutate(
    Result = case_when(
      ResQual == 1 & Analyte %in% contract_ana ~ "< MDL",
      ResQual == 1 & !Analyte %in% contract_ana ~ "< RL",
      TRUE ~ as.character(Conc)
    )
  )
  
  # Bind the lab_rep_data_mod df back with the no_lab_reps df
  all_data1 <- bind_rows(no_lab_reps, lab_rep_data_mod)
  
  # Remove df that are no longer necessary
  rm(lab_rep_data_mod, lab_reps, lab_reps_i, lab_reps_u, no_lab_reps, S_Conc, S_Result)
    

# Blanks and QA Information -----------------------------------------------

# Create a .csv file that summarizes the Lab Methods used for each analyte- only needed once
# all_data1 %>% 
#   count(Analyte, Method) %>%
#   select(-n) %>% 
#   write_excel_csv("AnalyteMethods.csv", na = "")
  
# Export a .csv file for Lab QA batches- only needed once
# all_data1 %>% 
#   select(LabBatch, SampleCode, Analyte) %>%
#   write_excel_csv("Lab_QA_Batch.csv", na = "")

# Export a .csv file for Analysis dates for Lab QA batches- only needed once
# all_data1 %>% 
#   select(LabBatch, AnalysisDate) %>% 
#   distinct() %>% 
#   # removing Lab Batch: MPSL-DFG_20160427_W_MeHg with AnalysisDate of 4/28/2016, since all
#   # but one sample in this batch were analyzed on 4/27/2016
#   filter(!(LabBatch == "MPSL-DFG_20160427_W_MeHg" & AnalysisDate == "2016-04-28")) %>% 
#   write_excel_csv("Lab_QA_Batch_AnaDate.csv", na = "")

# Remove a few QA related variables
all_data1 <- all_data1 %>% 
  select(-c(LabBatch, AnalysisDate, Method))
  
# Pull out Blank Samples and save for QA validation
blank_samples <- all_data1 %>% 
  filter(str_detect(StationName, "Blank$"))

# Remove Blank samples from all_data df
all_data2 <- anti_join(all_data1, blank_samples)


# Field Duplicates --------------------------------------------------------

# Make a new df with the Field Duplicate Samples
field_dups <- filter(all_data2, StationName == "Field Duplicate Sample")
  
# Remove Field Duplicate Samples from all_data2 df
no_field_dups <- filter(all_data2, StationName != "Field Duplicate Sample")

# Create a df with all of the parent sample codes
parent_samples <- field_dups %>% 
  count(ParentSample) %>% 
  select(-n) %>% 
  filter(!is.na(ParentSample))
  
# Create a df with all Station-Date combos for the Field Duplicate pairs
fd_station_dates <- 
  inner_join(no_field_dups, parent_samples, by = c("SampleCode" = "ParentSample")) %>% 
  count(StationName, SampleDate) %>% 
  select(-n)
  
# Inner join fd_station_dates df to no_field_dups df to pull out all Field Duplicate pairs
field_dup_pairs <- inner_join(no_field_dups, fd_station_dates)

# Remove field_dup_pairs from no_field_dups df
no_field_dups <- anti_join(no_field_dups, field_dup_pairs, by = c("StationName", "SampleDate"))
  
# Bind field_dups and field_dup_pairs together
field_dups_all <- bind_rows(field_dups, field_dup_pairs)
  
# Clean up
rm(parent_samples, fd_station_dates, field_dups, field_dup_pairs)
  
# Remove some Field Duplicate Samples that were collected on same day but different location
# to be processed separately
  # Find these samples
  field_dups_same_day <- field_dups_all %>% 
    count(SampleDate, Analyte) %>% 
    filter(n > 2) %>% 
    select(-n)
    
  # Inner join with field_dups_all df to isolate these samples
  field_dups_same_day <- inner_join(field_dups_all, field_dups_same_day)
  
  # Anti join with field_dups_all df to remove these samples from the field_dups df
  field_dups_all <- anti_join(field_dups_all, field_dups_same_day)
  
  # Separate field dups from parent samples, then join together using suffixes
  field_dups_same_day_fd <- field_dups_same_day %>% 
    filter(StationName == "Field Duplicate Sample")
  
  field_dups_same_day_ps <- field_dups_same_day %>% 
    filter(StationName != "Field Duplicate Sample")
  
  field_dups_same_day_clean <- 
    left_join(
      field_dups_same_day_ps, 
      field_dups_same_day_fd, 
      by = c("SampleCode" = "ParentSample"),
      suffix = c("_PS", "_FD")
    ) %>% 
    select(
      SampleCode,
      SampleCode_FD,
      StationName_PS,
      SampleDate_PS,
      CollectionTime_PS,
      CollectionTime_FD,
      Analyte_PS,
      Result_PS,
      Result_FD,
      Conc_PS,
      Conc_FD,
      ResQual_PS,
      ResQual_FD,
      RL_PS,
      MDL_PS,
      Units_PS,
      LabComments_PS,
      LabComments_FD,
      MME_Comments_PS,
      MME_Comments_FD,
      n_round_PS
    ) %>% 
    rename(
      SampleCode_PS = SampleCode,
      StationName = StationName_PS,
      SampleDate = SampleDate_PS,
      Analyte = Analyte_PS,
      RL = RL_PS,
      MDL = MDL_PS,
      Units = Units_PS,
      n_round = n_round_PS
    )
  
  # Clean up
  rm(field_dups_same_day, field_dups_same_day_fd, field_dups_same_day_ps)

# All other field dups and parent samples:
# Separate field dups from parent samples, then join together using suffixes
field_dups_fd <- field_dups_all %>% 
  filter(StationName == "Field Duplicate Sample")

field_dups_ps <- field_dups_all %>% 
  filter(StationName != "Field Duplicate Sample")
    
field_dups_all <- 
  left_join(
    field_dups_ps,
    field_dups_fd,
    by = c("SampleDate", "Analyte"),
    suffix = c("_PS", "_FD")
  ) %>% 
  select(
    SampleCode_PS,
    SampleCode_FD,
    StationName_PS,
    SampleDate,
    CollectionTime_PS,
    CollectionTime_FD,
    Analyte,
    Result_PS,
    Result_FD,
    Conc_PS,
    Conc_FD,
    ResQual_PS,
    ResQual_FD,
    RL_PS,
    MDL_PS,
    Units_PS,
    LabComments_PS,
    LabComments_FD,
    MME_Comments_PS,
    MME_Comments_FD,
    n_round_PS
  ) %>% 
  rename(
    StationName = StationName_PS,
    RL = RL_PS,
    MDL = MDL_PS,
    Units = Units_PS,
    n_round = n_round_PS
  )

# Clean up 
rm(field_dups_fd, field_dups_ps)

# Bind all field dup df's back together
field_dup_data <- bind_rows(field_dups_all, field_dups_same_day_clean) %>% 
  # Calculate RPD values for each replicate pair and flag if necessary
  mutate(
    rpd = round(abs(Conc_PS - Conc_FD)/((Conc_PS + Conc_FD)/2), 3),
    flag = case_when(
      Analyte %in% contract_ana & rpd > 0.25 & (Conc_PS > 10 * MDL | Conc_FD > 10 * MDL) ~ "FV",
      (!Analyte %in% contract_ana & !Analyte %in% c("DOC", "TOC", "VSS")) & rpd > 0.25 & (Conc_PS > 10 * RL | Conc_FD > 10 * RL) ~ "FV",
      Analyte %in% c("DOC", "TOC", "VSS") & rpd > 0.3 & (Conc_PS > 10 * RL | Conc_FD > 10 * RL) ~ "FV",
      TRUE ~ "n"
    ),
    # Average ResQual values
    ResQual = (ResQual_PS + ResQual_FD)/2
  ) %>% 
  select(-c(ResQual_PS, ResQual_FD))

# Export field_dup_data to .csv file- only needed once
# field_dup_data %>%
#   select(
#     SampleCode_PS:Result_FD,
#     rpd,
#     ResQual,
#     RL:MME_Comments_FD,
#     flag
#   ) %>%
#   write_excel_csv("FieldDuplicates.csv", na = "")
  
# Modify the field_dup_data df
field_dup_data_mod <- field_dup_data %>% 
  # Average Concentration values and round
  mutate(
    Conc = (Conc_PS + Conc_FD)/2,
    Conc = case_when(
      str_detect(Analyte, "^Boron|^MeHg") ~ round(Conc, 3),
      Analyte %in% c("Iron- filtered", "Manganese- filtered") ~ signif(Conc, 3),
      TRUE ~ round(Conc, n_round)
    ),
    # Add comment about using the average of field duplicates
    MME_Comments = case_when(
      is.na(MME_Comments_PS) & is.na(MME_Comments_FD) ~ "Average of Field Duplicates",
      !is.na(MME_Comments_PS) | !is.na(MME_Comments_FD) ~ "Average of Field Duplicates and Lab Replicates"
    ),
    # Consolidate Lab Comments
    LabComments = case_when(
      !is.na(LabComments_PS) & !is.na(LabComments_FD) ~ paste0("Parent Sample: ", LabComments_PS, "; Field Dup: ", LabComments_FD),
      !is.na(LabComments_PS) & is.na(LabComments_FD) ~ paste0("Parent Sample: ", LabComments_PS),
      is.na(LabComments_PS) & !is.na(LabComments_FD) ~ paste0("Field Dup: ", LabComments_FD)
    )
  ) %>% 
  # Rename some variables
  rename(
    SampleCode = SampleCode_PS,
    CollectionTime = CollectionTime_PS
  ) %>% 
  # Delete a few variables
  select(
    -c(
      ends_with("_PS"),
      ends_with("_FD"),
      rpd,
      flag
    )
  ) %>% 
  # Add Result variable back to df
  mutate(
    Result = case_when(
      ResQual == 1 & Analyte %in% contract_ana ~ "< MDL",
      ResQual == 1 & !Analyte %in% contract_ana ~ "< RL",
      TRUE ~ as.character(Conc)
    )
  )

# Bind the field_dup_data_mod df back with the no_field_dups df
all_data3 <- bind_rows(no_field_dups, field_dup_data_mod) %>% 
  select(-ParentSample)

# Remove df that are no longer necessary
rm(field_dup_data_mod, field_dups_all, field_dups_same_day_clean, no_field_dups)

# START HERE
# remove all samples before Dec 22, 2014
# find missing field duplicates and blank samples
# add comment about using MDL value when calculating RPD

# Companion Grab Samples --------------------------------------------------

# Make a new df with the Companion Grab Samples
CompanionGrab <- filter(AllData, StationName == "Companion Grab Sample")

# Remove Companion Samples from AllData df
NoCompanionGrab <- filter(AllData, StationName != "Companion Grab Sample")

# Create a df with all of the parent sample codes
ParentSamples <- CompanionGrab %>% 
  count(ParentSample) %>% 
  select(-n) %>% 
  filter(!is.na(ParentSample))

# Create a df with all Station-Date combos for the Companion Grab pairs
CG_StationDates <- inner_join(NoCompanionGrab, ParentSamples, by = c("SampleCode" = "ParentSample")) %>% 
  count(StationName, SampleDate) %>% 
  select(-n)

# Inner join CG_StationDates df to NoCompanionGrab df to pull out all Companion Grab pairs
CompanionGrab2 <- inner_join(NoCompanionGrab, CG_StationDates)

# Remove one sample from CompanionGrab2 since it doesn't have a Companion Grab pair associated with it
CompanionGrab2 <- filter(CompanionGrab2, !(SampleDate == "2017-02-01" & Analyte == "Boron- total"))

# Remove CompanionGrab2 from NoCompanionGrab df
NoCompanionGrab <- anti_join(NoCompanionGrab, CompanionGrab2, by = c("StationName", "SampleDate", "Analyte"))

# Bind CompanionGrab and CompanionGrab2 together
CompanionGrab <- bind_rows(CompanionGrab, CompanionGrab2)

# Remove CompanionGrab2
rm(CompanionGrab2)

# Select only variables that are identical in CompanionGrab df
CompanionGrabI <- CompanionGrab %>% 
  select(StationName, SampleDate, Analyte, RL, MDL, Units) %>% 
  # Remove rows with StationName = "Companion Grab Sample"
  filter(StationName != "Companion Grab Sample")

# Select variables that are unique in CompanionGrab df
CompanionGrabU <- CompanionGrab %>% 
  select(-c(RL, MDL, Units, ParentSample, MME_CommentsFD, MME_CommentsPS)) %>% 
  # Create a Key variable from StationName variable
  mutate(
    Key = case_when(
      StationName == "Companion Grab Sample" ~ "CG",
      TRUE                                   ~ "PS"
    )
  ) %>% 
  # Remove StationName
  select(-StationName)

# Create different df to spread out unique variables based on Key variable
# ResQual
S_ResQual <- CompanionGrabU %>% 
  select(SampleDate, Analyte, Key, ResQual) %>%
  spread(Key, ResQual) %>% 
  rename(ResQualCG = CG,
         ResQualPS = PS)

# SampleCode
S_SampleCode <- CompanionGrabU %>% 
  select(SampleDate, Analyte, Key, SampleCode) %>%
  spread(Key, SampleCode) %>% 
  rename(SampleCodeCG = CG,
         SampleCodePS = PS)

# CollectionTime
S_CollectionTime <- CompanionGrabU %>% 
  select(SampleDate, Analyte, Key, CollectionTime) %>%
  spread(Key, CollectionTime) %>% 
  rename(CollectionTimeCG = CG,
         CollectionTimePS = PS)

# Conc
S_Conc <- CompanionGrabU %>% 
  select(SampleDate, Analyte, Key, Conc) %>%
  spread(Key, Conc) %>% 
  rename(ConcCG = CG,
         ConcPS = PS)

# MME_Comments
S_MME_Comments <- CompanionGrabU %>% 
  select(SampleDate, Analyte, Key, MME_Comments) %>%
  spread(Key, MME_Comments) %>% 
  rename(MME_CommentsCG = CG,
         MME_CommentsPS = PS)

# Result
S_Result <- CompanionGrabU %>% 
  select(SampleDate, Analyte, Key, Result) %>%
  spread(Key, Result) %>% 
  rename(ResultCG = CG,
         ResultPS = PS)

# LabComments
S_LabComments <- CompanionGrabU %>% 
  select(SampleDate, Analyte, Key, LabComments) %>%
  spread(Key, LabComments) %>% 
  rename(LabCommentsCG = CG,
         LabCommentsPS = PS)

# Join all of the spreaded df back together with CompanionGrabI
CompanionGrabData <-
  left_join(CompanionGrabI, S_ResQual) %>%
  left_join(S_SampleCode) %>%
  left_join(S_CollectionTime) %>% 
  left_join(S_Conc) %>% 
  left_join(S_MME_Comments) %>%
  left_join(S_Result) %>%
  left_join(S_LabComments)

# Export CompanionGrabData to .csv file for further analysis- only needed once
# CompanionGrabData %>% write_excel_csv("CompanionGrabSamples.csv")

# Modify the CompanionGrabData df
CompanionGrabData <- CompanionGrabData %>% 
  # Add a comment explaining that the CGS and Normal samples are switched
  mutate(MME_Comments = paste("This sample was entered as a CGS in FLIMS and was collected by hand", 
                              MME_CommentsCG, sep = "; ")) %>% 
  # Delete a few variables
  select(-c(ResQualPS, SampleCodePS, CollectionTimePS, ConcPS, 
            MME_CommentsCG, MME_CommentsPS, ResultPS, LabCommentsPS)) %>% 
  # Rename a few variables
  rename(ResQual = ResQualCG,
         SampleCode = SampleCodeCG,
         CollectionTime = CollectionTimeCG,
         Conc = ConcCG,
         Result = ResultCG,
         LabComments = LabCommentsCG)
  
# Bind the CompanionGrabData df back with the NoCompanionGrab df
AllData <- bind_rows(CompanionGrabData, NoCompanionGrab) %>% 
  # Delete ParentSample variable
  select(-ParentSample)

# Remove df that are no longer necessary
rm(CompanionGrab, NoCompanionGrab, ParentSamples, CG_StationDates,
   CompanionGrabI, CompanionGrabU, CompanionGrabData, S_CollectionTime,
   S_Conc, S_LabComments, S_MME_Comments, S_ResQual, S_Result, S_SampleCode)

# Export AllData to .csv file- only needed once
# AllData %>% write_excel_csv("NormalSamples.csv")

# The final copy of the Lab concentration data for the Yolo Bypass Inlet-Outlet Study is located here:
# M:\Data\Lab_Final\YB_Inlet-Outlet_Conc_Data.xlsx
  