\name{Meteorology}
\alias{Meteorology}
\title{Load meteorological data into memory.}
\usage{
  Meteorology(iscfile, use = c("urban", "rural"))
}
\arguments{
  \item{iscfile}{filename or ISCFile object}

  \item{use}{whether to use urban or rural mixing heights}
}
\value{
  a Meteorology object (basically a \code{data.frame})
}
\description{
  Compatible with ISC-format files. Rotates the "flow
  vector" (wind direction) from ISC-format files by 180
  degrees to obtain the wind bearing. Defaults to retaining
  the urban mixing heights only.
}
\keyword{meteorology}

