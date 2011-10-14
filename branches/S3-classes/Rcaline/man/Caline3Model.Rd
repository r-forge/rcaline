\name{Caline3Model}
\alias{Caline3Model}
\title{Construct a Caline3Model object.}
\usage{
  Caline3Model(links, meteorology, surfaceRoughness,
  averagingTime = 60, settlingVelocity = 0,
  depositionVelocity = 0)
}
\arguments{
  \item{links}{a \code{\link{FreeFlowLinks}} object}

  \item{meteorology}{a \code{\link{Meteorology}} object}

  \item{surfaceRoughness}{defaults to 300.0 (semi-urban);
  values between 0-400 cm are sane.}

  \item{averagingTime}{in minutes}

  \item{settlingVelocity}{of the modeled pollutant}

  \item{depositionVelocity}{of the modeled pollutant}
}
\value{
  a Caline3Model object
}
\description{
  The model (object) contains all of the link and
  meteorology data, as well as site-specific model
  parameters, such as surface roughness.
}
\details{
  Internally, these are stored as single-precision arrays.
  If you want to experiment by changing a parameter,
  construct a new Caline3Model, unless you really know what
  you are doing.

  Use \code{\link{predict.Caline3Model}} to predict
  concentrations at a given set of receptors.
}
\seealso{
  predict.Caline3Model HourlyConcentrations
  AggregatedConcentrations
}
\keyword{model}

