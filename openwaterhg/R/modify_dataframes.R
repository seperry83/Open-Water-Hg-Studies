#' @title Create a Numeric Results Variable
#' @description Creates a new variable named \code{Conc}, which is a clone
#'     of the \code{Result} variable converted to "numeric" class. This
#'     function substitutes the Method Detection Limit (MDL) or Reporting
#'     Limit (RL) for the non-detect values contained in the \code{Result}
#'     variable.
#'
#' @param df The dataframe to add the \code{Conc} variable to. Dataframe
#'     must have variables named \code{Result}, \code{MDL}, and \code{RL}.
#'     The character \code{Result} variable must have non-detect values
#'     identified as either "< RL" or "< MDL". \code{MDL} and \code{RL}
#'     must be "numeric" class.
#'
#' @return A new variable named \code{Conc}, which is a clone of the
#'     \code{Result} variable converted to "numeric" class.
#' @export
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom magrittr %>%
add_num_result <- function(df) {

  # Make sure all necessary variables exist in df
  assertthat::assert_that(
    "Result" %in% names(df),
    msg = "add_num_result function\nDataframe doesn't have 'Result' variable."
  )

  assertthat::assert_that(
    "RL" %in% names(df),
    msg = "add_num_result function\nDataframe doesn't have 'RL' variable."
  )

  assertthat::assert_that(
    "MDL" %in% names(df),
    msg = "add_num_result function\nDataframe doesn't have 'MDL' variable."
  )

  # Run function if all variables exist in df
  df1 <- df %>%
    dplyr::mutate(
      Conc = dplyr::case_when(
        Result == "< RL"  ~ RL,
        Result == "< MDL" ~ MDL,
        TRUE              ~ as.numeric(Result)
      )
    )

  return(df1)
}


#' @title Create a Variable of Short Station Names
#' @description Creates a new variable named \code{ShortName}, which contains
#'     shortened station names of the \code{StationName} variable. These
#'     short station names will be used in plots of the data.
#'
#' @param df The dataframe to add the \code{ShortName} variable to. Dataframe
#'     must have a character variable named \code{StationName}. Each
#'     observation under the \code{StationName} variable must have one of the
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
#' @return A new character variable named \code{ShortName}, which contains
#'     shortened station names of the \code{StationName} variable.
#' @export
#' @importFrom assertthat assert_that
#' @importFrom assertthat noNA
#' @importFrom tibble tibble
#' @importFrom dplyr left_join
add_short_sta_names <- function(df) {

  # Make sure StationName variable exists in df
  assertthat::assert_that(
    "StationName" %in% names(df),
    msg = "add_short_sta_names function\nDataframe doesn't have 'StationName' variable."
  )

  # Run function if StationName variable exists in df
  # Create a df of all stations and their shortened names
  station_key <- tibble::tibble(
    StationName = c(
      "Cache Slough near Ryer Island",
      "CCSB- Low Flow Channel",
      "CCSB Overflow Weir- North",
      "CCSB Overflow Weir- South",
      "Fremont Weir- East Side",
      "Fremont Weir- Middle",
      "Fremont Weir- West Side",
      "Knights Landing Ridge Cut",
      "Liberty Cut below Stairsteps",
      "Miner Slough near Sac River",
      "Prospect Slough",
      "Putah Creek at Mace Blvd",
      "Sac River above the Sacramento Weir",
      "Shag Slough below Stairsteps",
      "Toe Drain at 1/2 Lisbon",
      "Toe Drain at County Road 22",
      "Toe Drain at Interstate 80",
      "Toe Drain at Lisbon Weir"
    ),
    ShortName = c(
      "Cache Sl",
      "CCSB- LFC",
      "CCSB- OW N",
      "CCSB- OW S",
      "Fremont Wr- E",
      "Fremont Wr- M",
      "Fremont Wr- W",
      "KLRC",
      "Liberty Cut",
      "Miner Sl",
      "Prospect Sl",
      "Putah Ck",
      "SR above the Sac Wr",
      "Shag Sl",
      "Toe Dr at 1/2 Lisbon",
      "Toe Dr at Rd 22",
      "Toe Dr at I-80",
      "Toe Dr at Lisbon"
    )
  )

  # left join station_key to df
  df1 <-dplyr::left_join(df, station_key)

  # Stop function and notify if any NA values exist in ShortName variable
  assertthat::assert_that(
    assertthat::noNA(df1$ShortName),
    msg = "add_short_sta_names function\nNA values in 'ShortName' variable.\nEach observation under the 'StationName' variable must have one of the following values:
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


#' @title Create a Variable of Sampling Events
#' @description Creates a new variable named \code{SamplingEvent}, which
#'     contains standardized sampling event names. This variable is derived
#'     from the \code{SampleDate} variable. These sampling event names will
#'     be used in plots of the data.
#'
#' @param df The dataframe to add the \code{SamplingEvent} variable to.
#'     Dataframe must have a variable named \code{SampleDate}. \code{SampleDate}
#'     must be "date" class. Each observation under the \code{SampleDate}
#'     variable must have one of the following values:
#' \itemize{
#' \item 2014-12-22
#' \item 2014-12-23
#' \item 2016-03-15
#' \item 2016-03-16
#' \item 2017-01-11
#' \item 2017-01-12
#' \item 2017-01-24
#' \item 2017-01-25
#' \item 2017-01-31
#' \item 2017-02-01
#' \item 2017-02-14
#' \item 2017-02-16
#' \item 2017-03-01
#' \item 2017-03-02
#' \item 2017-03-15
#' \item 2017-03-16
#' \item 2017-03-28
#' \item 2017-03-29
#' \item 2017-04-11
#' \item 2017-04-12
#' \item 2017-04-25
#' \item 2017-04-26
#' }
#'
#' @return A new character variable named \code{SamplingEvent}, which contains
#'     standardized sampling event names derived from the \code{SampleDate}
#'     variable.
#' @export
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.date
#' @importFrom assertthat noNA
#' @importFrom tibble tibble
#' @importFrom dplyr left_join
#' @importFrom lubridate as_date
add_samplingevent <- function(df) {

  # Make sure SampleDate variable exists in df
  assertthat::assert_that(
    "SampleDate" %in% names(df),
    msg = "add_samplingevent function\nDataframe doesn't have 'SampleDate' variable."
  )

  # Make sure SampleDate is a date object
  assertthat::assert_that(
    assertthat::is.date(df$SampleDate),
    msg = "add_samplingevent function\n'SampleDate' is not a Date object."
  )

  # Run function if no errors exist
  # Create a df of all SampleDates and their associated Sampling events
  samplingevent_key <- tibble::tibble(
    SampleDate = lubridate::as_date(
      c(
        "2014-12-22",
        "2014-12-23",
        "2016-03-15",
        "2016-03-16",
        "2017-01-11",
        "2017-01-12",
        "2017-01-24",
        "2017-01-25",
        "2017-01-31",
        "2017-02-01",
        "2017-02-14",
        "2017-02-16",
        "2017-03-01",
        "2017-03-02",
        "2017-03-15",
        "2017-03-16",
        "2017-03-28",
        "2017-03-29",
        "2017-04-11",
        "2017-04-12",
        "2017-04-25",
        "2017-04-26"
      )
    ),
    SamplingEvent = c(
      "Dec 22-23, 2014",
      "Dec 22-23, 2014",
      "Mar 15-16, 2016",
      "Mar 15-16, 2016",
      "Jan 11-12, 2017",
      "Jan 11-12, 2017",
      "Jan 24-25, 2017",
      "Jan 24-25, 2017",
      "Jan 31-Feb 1, 2017",
      "Jan 31-Feb 1, 2017",
      "Feb 14-15, 2017",
      "Feb 14-15, 2017",
      "Mar 1-2, 2017",
      "Mar 1-2, 2017",
      "Mar 15-16, 2017",
      "Mar 15-16, 2017",
      "Mar 28-29, 2017",
      "Mar 28-29, 2017",
      "Apr 11-12, 2017",
      "Apr 11-12, 2017",
      "Apr 25-26, 2017",
      "Apr 25-26, 2017"
    )
  )

  # left join samplingevent_key to df
  df1 <- dplyr::left_join(df, samplingevent_key)

  # Stop function and notify if any NA values exist in SamplingEvent variable
  assertthat::assert_that(
    assertthat::noNA(df1$SamplingEvent),
    msg = "add_samplingevent function\nNA values in 'SamplingEvent' variable.\nEach observation under the 'SampleDate' variable must have one of the following values:
    2014-12-22
    2014-12-23
    2016-03-15
    2016-03-16
    2017-01-11
    2017-01-12
    2017-01-24
    2017-01-25
    2017-01-31
    2017-02-01
    2017-02-14
    2017-02-16
    2017-03-01
    2017-03-02
    2017-03-15
    2017-03-16
    2017-03-28
    2017-03-29
    2017-04-11
    2017-04-12
    2017-04-25
    2017-04-26"
  )

  return(df1)
}
