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
    
# Clean up
rm(list= ls()[!(ls() == "contract_data_clean")])


# Bryte Lab Data ----------------------------------------------------------

# Import Bryte Lab dataset
bryte_data_orig <- read_excel(path = "M:/Data/Lab/Bryte_Lab/Open_Water/YB_Inlet-Outlet_Data_Bryte_all.xlsx")
 
# Clean up bryte_data_orig df
  #Import StationName Standarized Key
  std_station_b <- read_csv("YB_Mass_Balance/Concentrations/StationNameKey_Bryte.csv")
  
  # Import Analyte name Standarized Key
  std_analyte_b <- read_csv("YB_Mass_Balance/Concentrations/AnalyteKey_Bryte.csv")
  
  bryte_data_clean <- bryte_data_orig %>% 
    # Remove some extra Hg Analyses
    filter(Method != "EPA 200.8 (Hg Total) [1]*") %>%
    # Standardize Analyte Names
    left_join(std_analyte_b) %>% 
    # Standardize StationNames
    rename(StationName = "Station Name") %>% 
    left_join(std_station_b) %>% 
    # Separate Collection Date into 2 variables- one for date, other for time
    rename(CollectionDate = "Collection Date") %>% 
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
      "Sample Code",
      SampleDate,
      CollectionTime,
      AnalyteStd,
      Result,
      "Rpt Limit",
      Units,
      Method,
      "Parent Sample",
      n_round
    ) %>%
    # Rename some variables (New Name = Old Name)
    rename(
      StationName = StationNameStd,
      SampleCode = "Sample Code",
      Analyte = AnalyteStd,
      RL = "Rpt Limit",
      ParentSample = "Parent Sample"
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
# START HERE
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
lab_rep_data <- left_join(S_Conc, S_Result) %>% left_join(lab_reps_i) %>% 
  # Reorder columns
  select(
    SampleCode,
    StationName,
    SampleDate,
    CollectionTime,
    Analyte,
    Method,
    LabBatch,
    AnalysisDate,
    Conc1,
    Conc2,
    ResQual,
    RL,
    MDL,
    Units,
    ParentSample,
    Result1,
    Result2,
    n_round
  )
  
# Export lab_rep_data to .csv file for further analysis- only needed once
lab_rep_data %>% 
  select(-c(Method, AnalysisDate, ParentSample)) %>%
write_excel_csv("LabReplicates.csv")
    
# Modify the lab_rep_data df
lab_rep_data <- lab_rep_data %>% 
  mutate(Conc = (Conc1 + Conc2)/2,
         MME_Comments = paste0("Average of Lab Replicates: ", Result1, ", ", Result2)) %>% 
  # Delete a few variables
  select(-c(Conc1, Conc2, Result1, Result2)) %>% 
  # Round the Conc variable to 3 significant digits
  mutate(Conc = signif(Conc, 3)) %>% 
  # Add Result variable back to df
  mutate(Result = if_else(ResQual == 1, "< DL", as.character(Conc)))
  
  # Bind the lab_rep_data df back with the no_lab_reps df
  all_data <- bind_rows(lab_rep_data, no_lab_reps)
  
  # Remove df that are no longer necessary
  rm(lab_reps, lab_reps_i, lab_reps_u, lab_rep_data, no_lab_reps, S_Conc, S_Result)
    

# Blanks and QA Information -----------------------------------------------

# Create a .csv file that summarizes the Lab Methods used for each analyte- only needed once
# all_data %>% count(Analyte, Method) %>% 
  # write_excel_csv("AnalyteMethods.csv")
  
# Export a .csv file for QA tracking- only needed once
# all_data %>% select(SampleCode:AnalysisDate) %>% write_excel_csv("QA_tracking.csv")
  
# Bring in Sample Completeness data and check to see if we have all samples accounted for
IO_Samples <- read_excel(path = "Concentrations/Inlet-Outlet_SampleCompleteness.xlsx") %>% 
  mutate(SampleDate = as_date(SampleDate))
  
all_data %>% select(SampleCode, SampleDate, Analyte) %>% 
  setequal(IO_Samples)
  
# Remove a few QA related variables
all_data <- select(all_data, -c(Method:AnalysisDate))
 
# Filter and export Blank Samples- only needed once
# all_data %>% filter(StationName %in% c("Field and Filter Blank", "Field Blank", "Filter Blank")) %>% 
  # write_excel_csv("Blanks.csv")
  
# Remove Blank samples from all_data df
all_data <- filter(all_data, !StationName %in% c("Field and Filter Blank", "Field Blank", "Filter Blank"))


# Field Duplicates --------------------------------------------------------

# Make a new df with the Field Duplicate Samples
FieldDups <- filter(AllData, StationName == "Field Duplicate Sample")
  
# Remove Field Duplicate Samples from AllData df
NoFieldDups <- filter(AllData, StationName != "Field Duplicate Sample")

# Create a df with all of the parent sample codes
ParentSamples <- FieldDups %>% 
  count(ParentSample) %>% 
  select(-n) %>% 
  filter(!is.na(ParentSample))
  
# Create a df with all Station-Date combos for the Field Duplicate pairs
FD_StationDates <- inner_join(NoFieldDups, ParentSamples, by = c("SampleCode" = "ParentSample")) %>% 
  count(StationName, SampleDate) %>% 
  select(-n)
  
# Inner join FD_StationDates df to NoFieldDups df to pull out all Field Duplicate pairs
FieldDups2 <- inner_join(NoFieldDups, FD_StationDates)

# Remove FieldDups2 from NoFieldDups df
NoFieldDups <- anti_join(NoFieldDups, FieldDups2, by = c("StationName", "SampleDate"))
  
# Bind FieldDups and FieldDups2 together
FieldDups <- bind_rows(FieldDups, FieldDups2)
  
# Remove FieldDups2
rm(FieldDups2)
  
# Remove some Field Duplicate Samples that were collected on same day but different location
# to be processed separately
  # Find these samples
  FieldDupsSameDay <- FieldDups %>% 
    count(SampleDate, Analyte) %>% 
    filter(n > 2) %>% 
    select(-n)
    
  # Inner join with FieldDups df to isolate these samples and export to .csv file- only needed once
  # FieldDups %>% inner_join(FieldDupsSameDay) %>% 
    # write_excel_csv("FieldDupsUnusual.csv")
    
  # Anti join with FieldDups df to remove these samples from the FieldDups df
  FieldDups <- anti_join(FieldDups, FieldDupsSameDay)
  
# Select only variables that are identical in FieldDups df
FieldDupsI <- FieldDups %>% 
  select(StationName, SampleDate, Analyte, ResQual, RL, MDL, Units) %>% 
  # Average ResQual variable for each Replicate group
  group_by(SampleDate, Analyte) %>% 
  mutate(ResQual = mean(ResQual)) %>% 
  ungroup() %>% 
  # Remove rows with StationName = "Field Duplicate Sample"
  filter(StationName != "Field Duplicate Sample")
    
# Select variables that are unique in FieldDups df
FieldDupsU <- FieldDups %>% 
  select(-c(ResQual, RL, MDL, Units, ParentSample)) %>% 
  # Create a Key variable from StationName variable
  mutate(
    Key = case_when(
      StationName == "Field Duplicate Sample" ~ "FD",
      TRUE                                    ~ "PS"
    )
  ) %>% 
  # Remove StationName
  select(-StationName)
    
# Create different df to spread out unique variables based on Key variable
  # SampleCode
  S_SampleCode <- FieldDupsU %>% 
    select(SampleDate, Analyte, Key, SampleCode) %>%
    spread(Key, SampleCode) %>% 
    rename(SampleCodeFD = FD,
           SampleCodePS = PS)
    
  # CollectionTime
  S_CollectionTime <- FieldDupsU %>% 
    select(SampleDate, Analyte, Key, CollectionTime) %>%
    spread(Key, CollectionTime) %>% 
    rename(CollectionTimeFD = FD,
           CollectionTimePS = PS)
    
  # Conc
  S_Conc <- FieldDupsU %>% 
    select(SampleDate, Analyte, Key, Conc) %>%
    spread(Key, Conc) %>% 
    rename(ConcFD = FD,
           ConcPS = PS)
    
  # MME_Comments
  S_MME_Comments <- FieldDupsU %>% 
    select(SampleDate, Analyte, Key, MME_Comments) %>%
    spread(Key, MME_Comments) %>% 
    rename(MME_CommentsFD = FD,
           MME_CommentsPS = PS)
    
  # Result
  S_Result <- FieldDupsU %>% 
    select(SampleDate, Analyte, Key, Result) %>%
    spread(Key, Result) %>% 
    rename(ResultFD = FD,
           ResultPS = PS)
    
  # LabComments
  S_LabComments <- FieldDupsU %>% 
    select(SampleDate, Analyte, Key, LabComments) %>%
    spread(Key, LabComments) %>% 
    rename(LabCommentsFD = FD,
           LabCommentsPS = PS)
    
# Join all of the spreaded df back together with FieldDupsI
FieldDupData <-
  left_join(FieldDupsI, S_SampleCode) %>%
  left_join(S_CollectionTime) %>% 
  left_join(S_Conc) %>% 
  left_join(S_MME_Comments) %>%
  left_join(S_Result) %>%
  left_join(S_LabComments)
    
# Export FieldDupData to .csv file for further analysis- only needed once
# FieldDupData %>% write_excel_csv("FieldDuplicates.csv")

# Bind back Unusual FD's
# Import unusual FD data
FieldDupsUnusual <- read_excel(path = "Concentrations/FieldDupsUnusual.xlsx") %>% 
  # Correct data types so df can be bound to FieldDupData df
  mutate(SampleDate = as_date(SampleDate),
         CollectionTimeFD = hms::as.hms(CollectionTimeFD, tz = "UTC"),
         CollectionTimePS = hms::as.hms(CollectionTimePS, tz = "UTC")) %>%
  convert(chr(ResultFD, ResultPS))
         
FieldDupData <- bind_rows(FieldDupData, FieldDupsUnusual)
  
# Modify the FieldDupData df
FieldDupData <- FieldDupData %>% 
  mutate(Conc = (ConcFD + ConcPS)/2,
         MME_Comments = paste0("Average of Field Duplicates: ", ResultPS, ", ", ResultFD),
         LabComments = paste(LabCommentsPS, LabCommentsFD, sep = "; ")) %>% 
  # Delete a few variables
  select(-c(ConcFD, ConcPS, ResultFD, ResultPS, LabCommentsFD, LabCommentsPS, SampleCodeFD, CollectionTimeFD)) %>% 
  # Rename a few variables
  rename(SampleCode = SampleCodePS,
         CollectionTime = CollectionTimePS) %>% 
  # Round the Conc variable to 3 significant digits
  mutate(Conc = signif(Conc, 3)) %>% 
  # Add Result variable back to df
  mutate(Result = if_else(ResQual == 1, "< DL", as.character(Conc)))

# Bind the FieldDupData df back with the NoFieldDups df
AllData <- bind_rows(FieldDupData, NoFieldDups)

# Remove df that are no longer necessary
rm(FieldDups, NoFieldDups, ParentSamples, FD_StationDates, FieldDupsSameDay,
   FieldDupsI, FieldDupsU, FieldDupData, FieldDupsUnusual, S_CollectionTime,
   S_Conc, S_LabComments, S_MME_Comments, S_Result, S_SampleCode)


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
  