\name{ReceptorRings}
\alias{ReceptorRings}
\title{Construct a set of receptors in the vicinity of a set of links.}
\usage{
  ReceptorRings(links, elevation = 1.8, distances = c(50,
  100, 250, 500, 1000), spacing = identity, rgeos.scale =
  1e+06)
}
\arguments{
  \item{links}{basis}

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
  This function constructs concentric rings of receptors at
  specific distances from \code{links}.
}
\seealso{
  ReceptorGrid
}
\keyword{receptors}

