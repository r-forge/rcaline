\name{as.SpatialLines}
\alias{as.SpatialLines}
\alias{as.SpatialLines.SpatialPolygons}
\alias{as.SpatialLines,SpatialPolygons-method}
\title{
Convert a SpatialPolygons object to a SpatialLines object.
}
\description{
Extracts the edges from a SpatialPolygons object. Useful for sampling along the edges of buffers.
}
\usage{
as.SpatialLines(spobj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spobj}{
	A SpatialPolygons object. See ?sp package documentation for more.
}
}
\value{
A SpatialLines object.
}
\author{
David Holstius
}

\note{ There doesn't seem to be a matching method in the \code{sp} package, but if there were, that would be the one to use. }

\keyword{ SpatialLines }
\keyword{ SpatialPolygons }