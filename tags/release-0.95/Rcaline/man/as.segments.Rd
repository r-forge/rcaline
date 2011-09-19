\name{as.segments}
\alias{as.segments}
\alias{as.segments.SpatialLinesDataFrame}
\alias{as.segments.SpatialLines}
\alias{as.segments.Lines}
\alias{as.segments.Line}
\alias{as.segments,SpatialLinesDataFrame-method}
\alias{as.segments,SpatialLines-method}
\alias{as.segments,Lines-method}
\alias{as.segments,Line-method}
\title{
Segmentize a SpatialLines* object.
}
\description{
Converts a SpatialLines* object to a regular data.frame, with one row per planar segment.
}
\usage{
as.segments(spobj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spobj}{
	A SpatialLines or SpatialLinesDataFrame object. See ?sp package documentation for more.
}
}
\details{
This convenience function assists in converting roadway geometry from a shapefile (or other GIS datasource) to the format needed by the CALINE model.

Rows are merged with the @data from the original SpatialLinesDataFrame, such that attributes from the polyline feature containing a segment are copied to the new row representing that segment.
}
\value{
A data.frame with new columns x, y, xend, and yend, representing the start and end coordinates of each segment, plus all of the @data columns from the original SpatialLinesDataFrame.
}
\author{
David Holstius
}

\keyword{ shapefile }
\keyword{ segments }
\keyword{ segment }