% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_tools.R
\name{add_inlet_color_pal}
\alias{add_inlet_color_pal}
\title{Custom Color Palette for Inlet Stations}
\usage{
add_inlet_color_pal(aes_type = c("fill", "color"), legend_title = NULL)
}
\arguments{
\item{aes_type}{The aesthetic type to apply the palette to. Must be
either \code{"fill"} or \code{"color"}.}

\item{legend_title}{An optional argument used to provide a title for the
legend of the color palette. Default is \code{NULL}.}
}
\value{
A ggplot object with the custom color palette for the inlets applied
    to it.
}
\description{
Applies a custom color palette for the five inlet stations
    to be used in figures in the final report for the Open Water Mercury
    studies. The palette is based on the Viridis palette. Function is to
    be used on ggplot objects. Each observation under the inlet grouping
    variable used in the ggplot must have one of the following values:
\itemize{
\item KLRC
\item CCSB
\item Putah Creek
\item Sacramento Weir
\item Fremont Weir
}
This function works best if the inlet grouping variable is a factor in
above order.
}
