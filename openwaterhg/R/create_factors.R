# Convert StationName variable to factor to apply plotting order
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
conv_fact_long_sta_names <- function(df) {

  # Make sure StationName variable exists in df
  assertthat::assert_that(
    "StationName" %in% names(df),
    msg = "Dataframe doesn't have 'StationName' variable."
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
    mutate(StationName = factor(StationName, levels = stationname_levels))

  # Stop function and notify if any NA values exist in StationName variable
  assertthat::assert_that(
    assertthat::noNA(df1$StationName),
    msg = "Not all station names present; NA values in 'StationName' variable after converting to factor."
  )

  return(df1)
}


# Convert ShortName variable to factor to apply plotting order
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
conv_fact_short_sta_names <- function(df) {

  # Make sure ShortName variable exists in df
  assertthat::assert_that(
    "ShortName" %in% names(df),
    msg = "Dataframe doesn't have 'ShortName' variable."
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
    mutate(ShortName = factor(ShortName, levels = shortname_levels))

  # Stop function and notify if any NA values exist in ShortName variable
  assertthat::assert_that(
    assertthat::noNA(df1$ShortName),
    msg = "Not all station names present; NA values in 'ShortName' variable after converting to factor."
  )

  return(df1)
}


# Convert SamplingEvent variable to factor to apply plotting order
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
conv_fact_samplingevent <- function(df) {

  # Make sure StationName variable exists in df
  assertthat::assert_that(
    "SamplingEvent" %in% names(df),
    msg = "Dataframe doesn't have 'SamplingEvent' variable."
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
    mutate(SamplingEvent = factor(SamplingEvent, levels = samplingevent_levels))

  # Stop function and notify if any NA values exist in SamplingEvent variable
  assertthat::assert_that(
    assertthat::noNA(df1$SamplingEvent),
    msg = "Not all sampling events present; NA values in 'SamplingEvent' variable after converting to factor."
  )

  return(df1)
}
