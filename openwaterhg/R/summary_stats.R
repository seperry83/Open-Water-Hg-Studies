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
#' \item "Q1" - 25% quantile
#' \item "Median" - Median Value
#' \item "Q3" - 75% quantile
#' \item "Maximum" - Maximum Value
#' \item "IQR" - Interquartile Range (Q3 - Q1)
#' }
#' @export
#' @importFrom rlang enquo
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom dplyr n
#' @importFrom magrittr %>%
#' @importFrom stats sd
#' @importFrom stats quantile
#' @importFrom stats median
#' @importFrom stats IQR
summ_stat <- function(df, data_var, group_var1, group_var2 = NULL) {
  data_var_enquo <- rlang::enquo(data_var)
  group_var1_enquo <- rlang::enquo(group_var1)

  # if there are 2 grouping variables
  if (!missing(group_var2)) {
    group_var2_enquo <- rlang::enquo(group_var2)

    df2 <- df %>%
      dplyr::group_by(!!group_var1_enquo, !!group_var2_enquo) %>%
      dplyr::summarize(
        N = dplyr::n(),
        Mean = mean(!!data_var_enquo),
        StDev = stats::sd(!!data_var_enquo),
        Minimum = min(!!data_var_enquo),
        Q1 = stats::quantile(!!data_var_enquo, 0.25),
        Median = stats::median(!!data_var_enquo),
        Q3 = stats::quantile(!!data_var_enquo, 0.75),
        Maximum = max(!!data_var_enquo),
        IQR = stats::IQR(!!data_var_enquo)
      ) %>%
      dplyr::ungroup()
  }
  # if there is only one grouping variable
  else {
    df2 <- df %>%
      dplyr::group_by(!!group_var1_enquo) %>%
      dplyr::summarize(
        N = dplyr::n(),
        Mean = mean(!!data_var_enquo),
        StDev = stats::sd(!!data_var_enquo),
        Minimum = min(!!data_var_enquo),
        Q1 = stats::quantile(!!data_var_enquo, 0.25),
        Median = stats::median(!!data_var_enquo),
        Q3 = stats::quantile(!!data_var_enquo, 0.75),
        Maximum = max(!!data_var_enquo),
        IQR = stats::IQR(!!data_var_enquo)
      ) %>%
      dplyr::ungroup()
  }

  return(df2)
}

