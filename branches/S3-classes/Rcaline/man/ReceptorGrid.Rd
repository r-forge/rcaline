\name{ReceptorGrid}
\alias{ReceptorGrid}
\title{Construct a set of receptors in the vicinity of a set of links.}
\usage{
  ReceptorGrid(links, elevation = 1.8, resolution = 1000,
  maxDistance = 1000, rgeos.scale = 1e+06)
}
\arguments{
  \item{links}{basis}

  \item{elevation}{elevation in meters}

  \item{resolution}{spacing between receptors (both
  directions), in meters}

  \item{maxDistance}{TODO}

  \item{rgeos.scale}{TODO}
}
\value{
  SpatialPointsDataFrame
}
\description{
  This function constructs a regular Cartesian grid of
  receptors no more than \code{maxDistance} from
  \code{links}.
}
\seealso{
  ReceptorRings
}
\keyword{receptors}

