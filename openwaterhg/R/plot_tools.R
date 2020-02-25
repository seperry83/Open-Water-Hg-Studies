#' @title Custom Color Palette for Report Figures
#' @description Applies a custom color palette for figures in the final
#'     report for the Open Water Mercury studies. The palette is based
#'     on the Viridis palette. Function is to be used on ggplot objects.
#'
#' @param num An integer representing the number of colors for the
#'     palette.
#' @param aes_type The aesthetic type to apply the palette to. Can be
#'     either \code{"fill"} or \code{"color"}.
#'
#' @return A ggplot object with a custom color palette applied to it
#'     with the specified number of colors and aesthetic type.
#' @export
#' @importFrom colorspace scale_fill_discrete_sequential
#' @importFrom colorspace scale_color_discrete_sequential
color_pal_owhg <- function(num, aes_type = c("fill", "color")) {
  # evaluate choices for aes_type
  aes_type <- match.arg(aes_type)

  # apply palette based on aes_type
  if (aes_type == "fill") {
    colorspace::scale_fill_discrete_sequential(palette = "Viridis", nmax = num)
  } else if (aes_type == "color") {
    colorspace::scale_color_discrete_sequential(palette = "Viridis", nmax = num)
  }
}
