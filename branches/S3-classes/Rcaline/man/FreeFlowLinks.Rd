\name{FreeFlowLinks}
\alias{FreeFlowLinks}
\title{Construct a FreeFlowLinks object.}
\usage{
  FreeFlowLinks(x, vehiclesPerHour, emissionFactor, width =
  30, height = 0, classification = "AG", ...)
}
\arguments{
  \item{x}{a .shp file, or a SpatialLines object}

  \item{vehiclesPerHour}{average number of vehicles per
  hour (also known as "flow")}

  \item{emissionFactor}{emissions per vehicle, in grams per
  mile (per hour)}

  \item{width}{mixing zone width, in meters}

  \item{height}{elevation above ground level (not sea
  level!)}

  \item{classification}{see \code{\link{LinkType}} for
  possible values}

  \item{...}{other arguments}
}
\value{
  a FreeFlowLinks object
}
\description{
  \code{vehiclesPerHour}, \code{emissionFactor}, and other
  arguments can all be specified as expressions. Use them
  the same way you would use arguments to
  \code{\link{transform}}. A FreeFlowLinks object can be
  plotted with \code{\link{plot}} or coerced to a
  \code{data.frame} with \code{\link{as.data.frame}}.
}
\keyword{links}

