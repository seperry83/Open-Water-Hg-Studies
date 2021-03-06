#' @title Calculate Descriptive Summary Statistics
#' @description Calculates descriptive summary statistics for one or two
#'     grouping variables.
#'
#' @param df The dataframe with the data to calculate summary statistics on
#' @param data_var The variable in \code{df} that contains the numerical
#'     data to summarize. Must be "numeric" class.
#' @param group_var1 The mandatory first (or only) grouping variable in
#'     \code{df}.
#' @param group_var2 The optional second grouping variable in \code{df}.
#'
#' @return A dataframe with the following summary statistics for either
#'     one (\code{group_var1}) or two (\code{group_var1} and
#'     \code{group_var2}) grouping variables:
#' \itemize{
#' \item "N" - Sample Size
#' \item "Mean" - Average Value
#' \item "StDev" - Standard Deviation
#' \item "Minimum" - Minimum Value
#' \item "Q1" - 25 percent quantile
#' \item "Median" - Median Value
#' \item "Q3" - 75 percent quantile
#' \item "Maximum" - Maximum Value
#' \item "IQR" - Interquartile Range (Q3 - Q1)
#' }
#' @export
#' @importFrom rlang enquo
#' @importFrom dplyr group_by summarize ungroup n
#' @importFrom magrittr %>%
#' @importFrom stats sd quantile median IQR
summ_stat <- function(df, data_var, group_var1, group_var2 = NULL) {
  data_var_enquo <- enquo(data_var)
  group_var1_enquo <- enquo(group_var1)

  # if there are 2 grouping variables
  if (!missing(group_var2)) {
    group_var2_enquo <- enquo(group_var2)

    df2 <- df %>%
      group_by(!!group_var1_enquo, !!group_var2_enquo) %>%
      summarize(
        N = n(),
        Mean = mean(!!data_var_enquo),
        StDev = sd(!!data_var_enquo),
        Minimum = min(!!data_var_enquo),
        Q1 = quantile(!!data_var_enquo, 0.25),
        Median = median(!!data_var_enquo),
        Q3 = quantile(!!data_var_enquo, 0.75),
        Maximum = max(!!data_var_enquo),
        IQR = IQR(!!data_var_enquo)
      ) %>%
      ungroup()
  }
  # if there is only one grouping variable
  else {
    df2 <- df %>%
      group_by(!!group_var1_enquo) %>%
      summarize(
        N = n(),
        Mean = mean(!!data_var_enquo),
        StDev = sd(!!data_var_enquo),
        Minimum = min(!!data_var_enquo),
        Q1 = quantile(!!data_var_enquo, 0.25),
        Median = median(!!data_var_enquo),
        Q3 = quantile(!!data_var_enquo, 0.75),
        Maximum = max(!!data_var_enquo),
        IQR = IQR(!!data_var_enquo)
      ) %>%
      ungroup()
  }

  return(df2)
}

