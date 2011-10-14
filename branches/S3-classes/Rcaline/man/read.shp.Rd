\name{read.shp}
\alias{read.shp}
\title{Read a shapefile into memory.}
\usage{
  read.shp(filename, ...)
}
\arguments{
  \item{filename}{filename}

  \item{...}{other arguments}
}
\description{
  The shapefile must be in projected coordinates. Latitude
  and longitude are not acceptable. If needed, reproject
  the shapefile first using your favorite GIS program. If
  you don't have ArcGIS, try Quantum GIS (it's free).
}
\details{
  Requires \code{rgdal} (FIXME: use maptools also?)
}
\keyword{shapefile}

