\name{ReceptorLocations}
\alias{ReceptorLocations}
\title{Read receptor locations from a shapefile layer.}
\usage{
  ReceptorLocations(x, elevation = 1.8, ...)
}
\arguments{
  \item{file}{shapefile layer (filename ending in ".shp")}

  \item{elevation}{elevation in meters}

  \item{distances}{list of distances to the roadway
  centerline, in meters}

  \item{spacing}{TODO}

  \item{rgeos.scale}{TODO}
}
\value{
  SpatialPointsDataFrame
}
\description{
  Read receptor locations from a shapefile layer.
}
\seealso{
  ReceptorGrid
}
\keyword{receptors}

