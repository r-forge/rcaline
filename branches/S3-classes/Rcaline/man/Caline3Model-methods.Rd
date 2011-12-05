\name{predict.Caline3Model}
\alias{ggplot.Caline3Model}
\alias{predict.Caline3Model}
\title{predict.Caline3Model}
\usage{
  predict.Caline3Model(object, .parallel = TRUE)

  ggplot.Caline3Model(data, ...)
}
\arguments{
  \item{object}{a \code{\link{Caline3Model}} object}

  \item{.parallel}{logical; attempt to use the
  \code{foreach} package to exploit multiple cores or
  hosts?}
}
\value{
  matrix of predicted values
}
\description{
  predict.Caline3Model

  Plot model elements using ggplot2.
}
\keyword{ggplot}
\keyword{ggplot2}
\keyword{model}
\keyword{predict}

