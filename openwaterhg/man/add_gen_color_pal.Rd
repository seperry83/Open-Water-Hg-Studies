% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_tools.R
\name{add_gen_color_pal}
\alias{add_gen_color_pal}
\title{Custom Color Palette for 2-4 Groups}
\usage{
add_gen_color_pal(
  num_colors = c(2:4),
  aes_type = c("fill", "color"),
  legend_title = NULL
)
}
\arguments{
\item{aes_type}{The aesthetic type to apply the palette to. Must be
either \code{"fill"} or \code{"color"}.}

\item{legend_title}{An optional argument used to provide a title for the
legend of the color palette. Default is \code{NULL}.}

\item{numb_colors}{An integer representing the number of colors for the
palette. Must be between 2-4.}
}
\value{
A ggplot object with a custom color palette applied to it
    with the specified number of colors and aesthetic type.
}
\description{
Applies a custom color palette for 2-4 groups to be used
    in figures in the final report for the Open Water Mercury studies.
    The palette is based on the Viridis palette. Function is to be used
    on ggplot objects. Can be used to represent any categorical grouping
    variable as long as it has the same number of groups that is specified
    in \code{num_colors}.
}
