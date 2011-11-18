\name{predict.Caline3Model}
\alias{predict.Caline3Model}
\title{Use a Caline3Model to predict concentrations at a given set of receptors.}
\usage{
  predict.Caline3Model(links, receptors, meteorology,
  parameters, .parallel, ...)
}
\arguments{
  \item{object}{a \code{\link{Caline3Model}} object}

  \item{receptors}{created with \code{\link{ReceptorRings}}
  or \code{\link{ReceptorGrid}}}

  \item{.parallel}{logical; attempt to use the
  \code{foreach} package to exploit multiple cores or
  hosts?}

  \item{...}{other arguments}
}
\value{
  matrix of predicted values
}
\description{
  Returns "raw" estimates in the form of a matrix. Each row
  corresponds to a receptor, and each column to a
  meteorological condition. Use \code{aggregate()} on the
  result to obtain summary statistics for each receptor:
  mean, max, etc.
}
\keyword{model}
\keyword{predict}

