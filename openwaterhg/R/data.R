#' @title Combined Parameter Data
#' @description Contains all combined parameter data for the Yolo Bypass
#'     Mass Balance Study. The calculations for this dataset are in the
#'     "YB_Mass_Balance/Concentrations/CombinedCalcs_Plots_and_SummaryStats.R"
#'     script file. The combined parameters include:
#' \describe{
#' \item{THg and MeHg Concentrations on Solids}{Provide estimates of the
#'     amount of total mercury or methylmercury bound to the suspended
#'     sediment. An increasing or decreasing trend in these parameters
#'     indicate either an enrichment or reduction of Hg or MeHg on suspended
#'     particles.}
#' \item{THg and MeHg Partitioning Coefficients (Kd)}{An increasing trend in
#'     these parameters indicates greater partitioning of Hg or MeHg to the
#'     solid-bound phase, and a decreasing trend indicates greater partitioning
#'     to the filtered or dissolved phase.}
#' \item{Percent MeHg Concentration of the THg Concentration}{The percent of
#'     THg thatis MeHg. An increasing trend indicates more of the total mercury
#'     is methylmercury. Values for all three fractions (filtered, particulate,
#'     and unfiltered) are provided.}
#' \item{THg or MeHg normalized by OC}{Either THg or MeHg concentrations
#'     divided by organic carbon concentrations. Values for all three fractions
#'     (filtered, particulate, and unfiltered) are provided.}
#' \item{TOC or POC Concentrations on solids}{Either TOC or POC concentrations
#'     divided by the TSS concentration multiplied by 1,000.}
#' \item{Total Aluminum Concentration on Solids}{The total aluminum
#'     concentration divided by the TSS concentration multiplied by 1,000.}
#' }
#'
#' @format data frame with 2,487 rows and 8 columns
#' \describe{
#' \item{StationName}{The station name}
#' \item{SampleDate}{The sampling date}
#' \item{CollectionTimePST}{The collection time in PST}
#' \item{Year}{The year of the sampling date}
#' \item{SamplingEvent}{The sampling event}
#' \item{Parameter}{The name of the calculated parameter}
#' \item{Value}{Calculated value}
#' \item{Units}{The units of the value}
#' }
#'
"comb_param_calc"


#' @title Concentration Data in Water
#' @description Contains all water concentration data collected for the Yolo Bypass
#'     Mass Balance Study. The raw data for this dataset were integrated,
#'     cleaned, and QA'ed in the "Compile_and_Clean_Conc_Data.R" script file.
#'
#' @format data frame with 3,582 rows and 13 columns
#' \describe{
#' \item{SampleCode}{The unique identifier for each sample produced by Bryte Lab}
#' \item{StationName}{The station name where the water samples were collected}
#' \item{SampleDate}{The sampling date}
#' \item{CollectionTimePST}{The sample collection time in PST}
#' \item{Analyte}{The analytical parameter measured}
#' \item{Result}{The analytical result}
#' \item{ResQual}{A numeric variable that represents whether the result was below
#'     the detection limit (RL or MDL). A value of "0" indicates that the result
#'     was detected, a value of "1" indicates that the result was below the
#'     detection limit, and a value of "0.5" indicates that either the replicate
#'     or parent sample was below the detection limit.}
#' \item{RL}{The analytical reporting limit of the sample}
#' \item{MDL}{The analytical method detection limit of the sample}
#' \item{Units}{The units of the result}
#' \item{LabComments}{Comments provided by the analytical laboratory}
#' \item{MME_Comments}{Comments provided by staff from the Mercury Monitoring and
#'     Evaluation section}
#' \item{QualCode}{Qualification code(s) for the sample. "J" indicates that there
#'     was an issue with the sample, but it is still being used in analyses. "R"
#'     indicates that there was an issue with the sample, and it is not being used
#'     in analyses. "BD" indicates that there was a detection in a blank sample
#'     associated with the sample. "FV" indicates that there was a RPD value greater
#'     than its acceptable limit in a field duplicate pair associated with the
#'     sample.}
#' }
#'
"conc_data"


#' @title Daily Average Flow Data for the 2014, 2016, and 2017 Floods
#' @description Contains daily average flow data for all sampling locations
#'     for the 2014, 2016, and 2017 flood events in the Yolo Bypass. The raw data
#'     for this dataset were integrated, cleaned, and averaged in the
#'     "YB_Mass_Balance/Flows/Process_Flow_Data.R" script file.
#'
#' @format data frame with 1,900 rows and 5 columns
#' \describe{
#' \item{Date}{The date for the daily average flow data}
#' \item{Year}{The year of the flood}
#' \item{StationName}{The station name where the flow measurements were collected}
#' \item{LocType}{A categorical variable indicating whether the station was an
#'     inlet, outlet, or Below Liberty Island location}
#' \item{Flow}{The daily average flow value in cubic feet per second}
#' }
#'
"daily_flow_data_all"


#' @title Daily Average Flow Data for the Sampling Events
#' @description Contains a subset of the \code{daily_flow_data_all} dataset with
#'     daily average flow data for the 11 sampling events conducted for the
#'     Yolo Bypass Mass Balance study. These flow values were used in the load
#'     calculations for these 11 sampling events. The raw data for this dataset
#'     were integrated, cleaned, and averaged in the
#'     "YB_Mass_Balance/Flows/Process_Flow_Data.R" script file.
#'
#' @format data frame with 132 rows and 5 columns
#' \describe{
#' \item{SamplingEvent}{The sampling event for the daily average flow data}
#' \item{Year}{The year of the flood}
#' \item{StationName}{The station name where the flow measurements were collected}
#' \item{LocType}{A categorical variable indicating whether the station was an
#'     inlet, outlet, or Below Liberty Island location}
#' \item{Flow}{The daily average flow value in cubic feet per second}
#' }
#'
"daily_flow_data_se"


#' @title Field Measurements
#' @description Contains field measurements collected for the Yolo Bypass
#'     Mass Balance Study. Measurements include Water Temperature, Specific
#'     Conductance, Dissolved Oxygen, and Turbidity.
#'
#' @format data frame with 165 rows and 7 columns
#' \describe{
#' \item{StationName}{The station name where the measurement was collected}
#' \item{SampleDate}{The sampling date of the field measurement}
#' \item{CollectionTimePST}{The collection time of the field measurement in PST}
#' \item{WaterTempC}{The water temperature in degrees Celcius}
#' \item{SpCond}{The specific conductance in uS/cm}
#' \item{DissOxy}{The dissolved oxygen concentration in mg/L}
#' \item{Turbidity}{The turbidity in NTU}
#' }
#'
"field_data"


#' @title Calculated Mass Loads
#' @description Contains calculated mass loads for the Yolo Bypass Mass Balance
#'     Study. The calculations for this dataset are in the
#'     "YB_Mass_Balance/Loads/All_LoadCalcs.R" script file.
#'
#' @format data frame with 1,460 rows and 10 columns
#' \describe{
#' \item{SamplingEvent}{The sampling event for the mass load value}
#' \item{Year}{The year of the flood}
#' \item{StationName}{The station name}
#' \item{Analyte}{The analytical parameter for the mass load value}
#' \item{Conc}{The water concentration of the analyte used in the load calculation}
#' \item{ConcUnits}{The units of the \code{Conc} variable}
#' \item{Flow}{The daily average flow value in cubic feet per second used in the
#'     load calculations}
#' \item{LocType}{A categorical variable indicating whether the station was an
#'     inlet, outlet, or Below Liberty Island location}
#' \item{Load}{The calculated mass load value}
#' \item{LoadUnits}{The units of the load value}
#' }
#'
"loads_calc"


#' @title Flow and Mass Load Data from the CALFED 2008 report
#' @description Contains total daily inflow and net methylmercury load data from
#'     the 2006 flood event in the Yolo Bypass as presented in the CALFED 2008 report.
#'     This dataset is used as a comparison to the net MeHg loads from the Yolo Bypass
#'     Mass Balance study.
#'
#' @format data frame with 14 rows and 7 columns
#' \describe{
#' \item{SampleDate}{The date of the total inflow and net load values}
#' \item{Year}{The year of the flood}
#' \item{Analyte}{The analytical parameter for the net load value}
#' \item{Segment}{The geographical segment for the net load value}
#' \item{LoadUnits}{The units of the net load value}
#' \item{NetLoad}{The net load value}
#' \item{TotalInputFlow}{The total daily inflow value in cubic feet per second}
#' }
#'
"loads_flow_cf"


#' @title Calculated Particulate Concentrations
#' @description Contains calculated particulate concentration data for mercury,
#'     methylmercury, and organic carbon for the Yolo Bypass Mass Balance Study.
#'     Particulate concentrations were calculated as the difference between the
#'     unfiltered and filtered concentrations. The calculations for this dataset
#'     are in the "YB_Mass_Balance/Concentrations/Calculate_Particulate_Fractions.R"
#'     script file.
#'
#' @format data frame with 500 rows and 6 columns
#' \describe{
#' \item{StationName}{The station name}
#' \item{SampleDate}{The sampling date}
#' \item{CollectionTimePST}{The sample collection time in PST}
#' \item{Analyte}{The name of the calculated particulate parameter}
#' \item{Conc}{The calculated particulate concentration}
#' \item{Units}{The units of the \code{Conc} variable}
#' }
#'
"part_conc_calc"



