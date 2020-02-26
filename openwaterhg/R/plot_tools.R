#' @title Custom Color Palette for 2-4 Groups
#' @description Applies a custom color palette for 2-4 groups to be used
#'     in figures in the final report for the Open Water Mercury studies.
#'     The palette is based on the Viridis palette. Function is to be used
#'     on ggplot objects. Can be used to represent any categorical grouping
#'     variable as long as it has the same number of groups that is specified
#'     in \code{num_colors}.
#'
#' @param numb_colors An integer representing the number of colors for the
#'     palette. Must be between 2-4.
#' @param aes_type The aesthetic type to apply the palette to. Must be
#'     either \code{"fill"} or \code{"color"}.
#' @param legend_title An optional argument used to provide a title for the
#'     legend of the color palette. Default is \code{NULL}.
#'
#' @return A ggplot object with a custom color palette applied to it
#'     with the specified number of colors and aesthetic type.
#' @export
#' @importFrom colorspace sequential_hcl
#' @import ggplot2
add_gen_color_pal <- function(num_colors = c(2:4),
                              aes_type = c("fill", "color"),
                              legend_title = NULL) {
  # evaluate choices for num_colors
  if (!(as.integer(num_colors) %in% 2L:4L)) stop("numb_colors must be between 2 and 4")

  # evaluate choices for aes_type
  aes_type <- match.arg(aes_type, c("fill", "color"))

  # define colors for each palette
  pal_two <- c("#F69422", "#00588B")
  pal_three <- rev(sequential_hcl(3, palette = "Viridis"))
  pal_four <- rev(sequential_hcl(4, palette = "Viridis"))

  # apply palette based on num_colors and aes_type
  if (aes_type == "fill") {
    if (num_colors == 2) {
      scale_fill_manual(name = legend_title, values = pal_two)
    } else if (num_colors == 3) {
      scale_fill_manual(name = legend_title, values = pal_three)
    } else {
      scale_fill_manual(name = legend_title, values = pal_four)
    }
  } else {
    if (num_colors == 2) {
      scale_color_manual(name = legend_title, values = pal_two)
    } else if (num_colors == 3) {
      scale_color_manual(name = legend_title, values = pal_three)
    } else {
      scale_color_manual(name = legend_title, values = pal_four)
    }
  }
}
