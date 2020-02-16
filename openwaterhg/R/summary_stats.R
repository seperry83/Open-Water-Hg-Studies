# Generate a table of descriptive summary statistics for one or two grouping variables

library(rlang)

# INPUT:
# df is the dataframe to run the summary statistics on
# data is the variable in the df that contains the numerical data to summarize
# g1 is the first grouping variable in the df
# g2 is an optional argument to add a second grouping variable from the df

# OUTPUT:
# a tibble of the following summary statistics for either one or two grouping variables:
# sample size, mean, standard deviation, minimum value, Q1 (25% quantile), median,
# Q3 (75% quantile), maximum value, and the Interquartile Range (IQR = Q3 - Q1)

#' Title
#'
#' @param df
#' @param data
#' @param g1
#' @param g2
#'
#' @return
#' @export
#'
#' @examples
SummStat <- function(df, data, g1, g2) {
  Data.quo <- enquo(data)
  g1.quo <- enquo(g1)

  # if there are 2 grouping variables
  if (!missing(g2)) {
    g2.quo <- enquo(g2)

    df2 <- df %>%
      group_by(!!g1.quo, !!g2.quo) %>%
      summarize(
        N = n(),
        Mean = mean(!!Data.quo),
        StDev = sd(!!Data.quo),
        Minimum = min(!!Data.quo),
        Q1 = quantile(!!Data.quo, 0.25),
        Median = median(!!Data.quo),
        Q3 = quantile(!!Data.quo, 0.75),
        Maximum = max(!!Data.quo),
        IQR = IQR(!!Data.quo)
      )
  }
  # if there is only one grouping variable
  else {
    df2 <- df %>%
      group_by(!!g1.quo) %>%
      summarize(
        N = n(),
        Mean = mean(!!Data.quo),
        StDev = sd(!!Data.quo),
        Minimum = min(!!Data.quo),
        Q1 = quantile(!!Data.quo, 0.25),
        Median = median(!!Data.quo),
        Q3 = quantile(!!Data.quo, 0.75),
        Maximum = max(!!Data.quo),
        IQR = IQR(!!Data.quo)
      )
  }

  return(df2)
}

