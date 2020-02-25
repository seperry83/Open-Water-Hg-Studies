#' @title Convert \code{StationName} Variable to a Factor
#' @description Converts the \code{StationName} variable from a character
#'     variable to a factor variable to apply a consistent order in
#'     plots of the data. The \code{StationName} variable contains a
#'     standardized set of longer station names.
#'
#' @param df The dataframe to apply the function to. Dataframe must have
#'     a character variable named \code{StationName}. Each observation
#'     under the \code{StationName} variable must have one of the
#'     following values:
#' \itemize{
#' \item Cache Slough near Ryer Island
#' \item CCSB- Low Flow Channel
#' \item CCSB Overflow Weir- North
#' \item CCSB Overflow Weir- South
#' \item Fremont Weir- East Side
#' \item Fremont Weir- Middle
#' \item Fremont Weir- West Side
#' \item Knights Landing Ridge Cut
#' \item Liberty Cut below Stairsteps
#' \item Miner Slough near Sac River
#' \item Prospect Slough
#' \item Putah Creek at Mace Blvd
#' \item Sac River above the Sacramento Weir
#' \item Shag Slough below Stairsteps
#' \item Toe Drain at 1/2 Lisbon
#' \item Toe Drain at County Road 22
#' \item Toe Drain at Interstate 80
#' \item Toe Drain at Lisbon Weir
#' }
#'
#' @return A dataframe with the \code{StationName} variable converted to
#'     a factor
#' @export
#' @importFrom assertthat assert_that
#' @importFrom assertthat noNA
#' @importFrom dplyr mutate
conv_fact_long_sta_names <- function(df) {

  # Make sure StationName variable exists in df
  assertthat::assert_that(
    "StationName" %in% names(df),
    msg = "conv_fact_long_sta_names function\nDataframe doesn't have 'StationName' variable."
  )

  # Run function if StationName variable exists in df
  # Create vector of StationNames for factor levels
  stationname_levels <- c(
    "Fremont Weir- East Side",
    "Fremont Weir- Middle",
    "Fremont Weir- West Side",
    "Sac River above the Sacramento Weir",
    "CCSB- Low Flow Channel",
    "CCSB Overflow Weir- North",
    "CCSB Overflow Weir- South",
    "Knights Landing Ridge Cut",
    "Putah Creek at Mace Blvd",
    "Toe Drain at County Road 22",
    "Toe Drain at Interstate 80",
    "Toe Drain at Lisbon Weir",
    "Toe Drain at 1/2 Lisbon",
    "Prospect Slough",
    "Liberty Cut below Stairsteps",
    "Shag Slough below Stairsteps",
    "Cache Slough near Ryer Island",
    "Miner Slough near Sac River"
  )

  # Mutate StationName variable to a factor
  df1 <- df %>%
    dplyr::mutate(StationName = factor(StationName, levels = stationname_levels))

  # Stop function and notify if any NA values exist in StationName variable
  assertthat::assert_that(
    assertthat::noNA(df1$StationName),
    msg = "conv_fact_long_sta_names function\nNA values in 'StationName' variable after converting to factor.\nEach observation under the 'StationName' variable must have one of the following values:
    Cache Slough near Ryer Island
    CCSB- Low Flow Channel
    CCSB Overflow Weir- North
    CCSB Overflow Weir- South
    Fremont Weir- East Side
    Fremont Weir- Middle
    Fremont Weir- West Side
    Knights Landing Ridge Cut
    Liberty Cut below Stairsteps
    Miner Slough near Sac River
    Prospect Slough
    Putah Creek at Mace Blvd
    Sac River above the Sacramento Weir
    Shag Slough below Stairsteps
    Toe Drain at 1/2 Lisbon
    Toe Drain at County Road 22
    Toe Drain at Interstate 80
    Toe Drain at Lisbon Weir"
  )

  return(df1)
}


#' @title Convert \code{ShortName} Variable to a Factor
#' @description Converts the \code{ShortName} variable from a character
#'     variable to a factor variable to apply a consistent order in
#'     plots of the data. The \code{ShortName} variable contains a
#'     standardized set of shorter station names.
#'
#' @param df The dataframe to apply the function to. Dataframe must have
#'     a character variable named \code{ShortName}. Each observation under
#'     the \code{ShortName} variable must have one of the following
#'     values:
#' \itemize{
#' \item Cache Sl
#' \item CCSB- LFC
#' \item CCSB- OW N
#' \item CCSB- OW S
#' \item Fremont Wr- E
#' \item Fremont Wr- M
#' \item Fremont Wr- W
#' \item KLRC
#' \item Liberty Cut
#' \item Miner Sl
#' \item Prospect Sl
#' \item Putah Ck
#' \item Shag Sl
#' \item SR above the Sac Wr
#' \item Toe Dr at 1/2 Lisbon
#' \item Toe Dr at I-80
#' \item Toe Dr at Lisbon
#' \item Toe Dr at Rd 22
#' }
#'
#' @return A dataframe with the \code{ShortName} variable converted to
#'     a factor
#' @export
#' @importFrom assertthat assert_that
#' @importFrom assertthat noNA
#' @importFrom dplyr mutate
conv_fact_short_sta_names <- function(df) {

  # Make sure ShortName variable exists in df
  assertthat::assert_that(
    "ShortName" %in% names(df),
    msg = "conv_fact_short_sta_names function\nDataframe doesn't have 'ShortName' variable."
  )

  # Run function if ShortName variable exists in df
  # Create vector of ShortNames for factor levels
  shortname_levels <- c(
    "Fremont Wr- E",
    "Fremont Wr- M",
    "Fremont Wr- W",
    "SR above the Sac Wr",
    "CCSB- LFC",
    "CCSB- OW N",
    "CCSB- OW S",
    "KLRC",
    "Putah Ck",
    "Toe Dr at Rd 22",
    "Toe Dr at I-80",
    "Toe Dr at Lisbon",
    "Toe Dr at 1/2 Lisbon",
    "Prospect Sl",
    "Liberty Cut",
    "Shag Sl",
    "Cache Sl",
    "Miner Sl"
  )

  # Mutate ShortName variable to a factor
  df1 <- df %>%
    dplyr::mutate(ShortName = factor(ShortName, levels = shortname_levels))

  # Stop function and notify if any NA values exist in ShortName variable
  assertthat::assert_that(
    assertthat::noNA(df1$ShortName),
    msg = "conv_fact_short_sta_names function\nNA values in 'ShortName' variable after converting to factor.\nEach observation under the 'ShortName' variable must have one of the following values:
    Cache Sl
    CCSB- LFC
    CCSB- OW N
    CCSB- OW S
    Fremont Wr- E
    Fremont Wr- M
    Fremont Wr- W
    KLRC
    Liberty Cut
    Miner Sl
    Prospect Sl
    Putah Ck
    Shag Sl
    SR above the Sac Wr
    Toe Dr at 1/2 Lisbon
    Toe Dr at I-80
    Toe Dr at Lisbon
    Toe Dr at Rd 22"
  )

  return(df1)
}


#' @title Apply Factor Order to Inlet Stations
#' @description Converts the \code{StationName} variable from a character
#'     variable to a factor variable to apply a consistent order in
#'     plots of the inlet station data. The \code{StationName} variable
#'     contains a standardized set of inlet station names only.
#'
#' @param df The dataframe to apply the function to. Dataframe must have
#'     a character variable named \code{StationName}. Each observation under
#'     the \code{StationName} variable must have one of the following
#'     values:
#' \itemize{
#' \item CCSB
#' \item Fremont Weir
#' \item KLRC
#' \item Putah Creek
#' \item Sacramento Weir
#' }
#'
#' @return A dataframe with the \code{StationName} variable converted to
#'     a factor
#' @export
#' @importFrom assertthat assert_that
#' @importFrom assertthat noNA
#' @importFrom dplyr mutate
conv_fact_inlet_names <- function(df) {

  # Make sure StationName variable exists in df
  assertthat::assert_that(
    "StationName" %in% names(df),
    msg = "conv_fact_inlet_names function\nDataframe doesn't have 'StationName' variable."
  )

  # Run function if StationName variable exists in df
  # Create vector of inlet names for factor levels
  inlet_levels <- c(
    "KLRC",
    "CCSB",
    "Putah Creek",
    "Sacramento Weir",
    "Fremont Weir"
  )

  # Mutate StationName variable to a factor
  df1 <- df %>%
    dplyr::mutate(StationName = factor(StationName, levels = inlet_levels))

  # Stop function and notify if any NA values exist in StationName variable
  assertthat::assert_that(
    assertthat::noNA(df1$StationName),
    msg = "conv_fact_inlet_names function\nNA values in 'StationName' variable after converting to factor.\nEach observation under the 'StationName' variable must have one of the following values:
    CCSB
    Fremont Weir
    KLRC
    Putah Creek
    Sacramento Weir"
  )

  return(df1)
}


#' @title Convert \code{SamplingEvent} Variable to a Factor
#' @description Converts the \code{SamplingEvent} variable from a
#'     character variable to a factor variable to apply a consistent
#'     order in plots of the data. The \code{SamplingEvent} variable
#'     contains a standardized set of sampling event names.
#'
#' @param df The dataframe to apply the function to. Dataframe must have
#'     a character variable named \code{SamplingEvent}. Each observation
#'     under the \code{SamplingEvent} variable must have one of the
#'     following values:
#' \itemize{
#' \item Dec 22-23, 2014
#' \item Mar 15-16, 2016
#' \item Jan 11-12, 2017
#' \item Jan 24-25, 2017
#' \item Jan 31-Feb 1, 2017
#' \item Feb 14-15, 2017
#' \item Mar 1-2, 2017
#' \item Mar 15-16, 2017
#' \item Mar 28-29, 2017
#' \item Apr 11-12, 2017
#' \item Apr 25-26, 2017
#' }
#'
#' @return A dataframe with the \code{SamplingEvent} variable converted
#'     to a factor
#' @export
#' @importFrom assertthat assert_that
#' @importFrom assertthat noNA
#' @importFrom dplyr mutate
conv_fact_samplingevent <- function(df) {

  # Make sure StationName variable exists in df
  assertthat::assert_that(
    "SamplingEvent" %in% names(df),
    msg = "conv_fact_samplingevent function\nDataframe doesn't have 'SamplingEvent' variable."
  )

  # Run function if SamplingEvent variable exists in df
  # Create vector of SamplingEvents for factor levels
  samplingevent_levels <- c(
    "Dec 22-23, 2014",
    "Mar 15-16, 2016",
    "Jan 11-12, 2017",
    "Jan 24-25, 2017",
    "Jan 31-Feb 1, 2017",
    "Feb 14-15, 2017",
    "Mar 1-2, 2017",
    "Mar 15-16, 2017",
    "Mar 28-29, 2017",
    "Apr 11-12, 2017",
    "Apr 25-26, 2017"
  )

  # Mutate SamplingEvent variable to a factor
  df1 <- df %>%
    dplyr::mutate(SamplingEvent = factor(SamplingEvent, levels = samplingevent_levels))

  # Stop function and notify if any NA values exist in SamplingEvent variable
  assertthat::assert_that(
    assertthat::noNA(df1$SamplingEvent),
    msg = "conv_fact_samplingevent function\nNA values in 'SamplingEvent' variable after converting to factor.\nEach observation under the 'SamplingEvent' variable must have one of the following values:
    Dec 22-23, 2014
    Mar 15-16, 2016
    Jan 11-12, 2017
    Jan 24-25, 2017
    Jan 31-Feb 1, 2017
    Feb 14-15, 2017
    Mar 1-2, 2017
    Mar 15-16, 2017
    Mar 28-29, 2017
    Apr 11-12, 2017
    Apr 25-26, 2017"
  )

  return(df1)
}
