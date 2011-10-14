\name{plot.FreeFlowLinks}
\alias{plot.FreeFlowLinks}
\title{Plot a FreeFlowLinks object using \code{ggplot2}.}
\usage{
  plot.FreeFlowLinks(x, ...)
}
\arguments{
  \item{x}{a FreeFlowLinks object}

  \item{...}{other arguments}
}
\description{
  Roadways are colored by the total emissions strength, Q
  (g/mi), the product of flow \code{vehiclesPerHour} and
  \code{emissionFactor}.
}
\details{
  Prettier but slower than \code{\link{lines}}.
}
\keyword{links}

