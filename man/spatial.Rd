\name{receiver_level}
\alias{receiver_level}
\alias{noise_grid}
\alias{noise_contours}
\alias{estimate_source_levels}
\alias{barrier_segments_from_paths}
\title{Spatial acoustic calculations / Cálculos acústicos espaciales}
\usage{
receiver_level(x, y, sources, barriers = NULL, barrier = NULL,
  reverberation = NULL)

noise_grid(sources, domain = NULL, polygon = NULL, resolution = NULL,
  barriers = NULL, barrier = NULL, reverberation = NULL)

noise_contours(grid, breaks = c(75, 80, 85, 90, 95, 100, 105, 110),
  level_col = "Lp")

estimate_source_levels(measurements, sources, barriers = NULL, barrier = NULL,
  reverberation = NULL, start = 95, lower = 40, upper = 140)

barrier_segments_from_paths(df)
}
\arguments{
  \item{x, y}{Receiver coordinates in metres.}
  \item{sources}{Data frame containing source coordinates and a source level at one metre.}
  \item{barriers}{Optional barrier-segment data frame with x1, y1, x2 and y2.}
  \item{barrier}{Optional list of barrier-model parameters.}
  \item{reverberation}{Optional reverberation configuration list.}
  \item{domain}{Calculation domain as c(xmin, xmax, ymin, ymax).}
  \item{polygon}{Optional polygon data frame with x and y coordinates.}
  \item{resolution}{Grid spacing target in metres.}
  \item{grid}{Noise grid returned by \code{noise_grid()}.}
  \item{breaks}{Contour levels.}
  \item{level_col}{Name of the grid level column.}
  \item{measurements}{Data frame containing x, y and measured Lp.}
  \item{start, lower, upper}{Initial, lower and upper source-level values for inverse estimation.}
  \item{df}{Polyline path data with x, y and group columns.}
}
\value{A receiver level, grid data frame, contour data frame, inverse-estimation result list, or barrier-segment data frame.}
\description{Shiny-independent spatial engine for WorknoiseR. / Motor espacial de WorknoiseR independiente de Shiny.}
