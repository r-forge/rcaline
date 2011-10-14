\name{aggregate.HourlyConcentrations}
\alias{aggregate.HourlyConcentrations}
\title{Aggregate the "raw" result matrix obtained from \code{\link{predict.Caline3Model}},
summarizing the hourly estimates for each receptor.}
\usage{
  aggregate.HourlyConcentrations(x, FUN = list("min",
  "mean", "median", "GM", "max", "sd"), na.rm = T, ...)
}
\arguments{
  \item{x}{hourly concentrations obtained from
  \code{\link{predict.Caline3Model}}}

  \item{FUN}{a list of summary functions to apply to each
  receptor}

  \item{na.rm}{logical; passed to each summary function in
  turn}

  \item{...}{other arguments}
}
\value{
  matrix of summary statistics\
}
\description{
  Use \code{as(x, "SpatialPointsDataFrame")} to re-bind
  these summary statistics with the locations (and other
  attributes) of the receptors used in the prediction step.
}

