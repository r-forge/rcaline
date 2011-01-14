\name{CALINE3.examples}
\alias{CALINE3.examples}
\alias{ExampleOne}
\alias{ExampleTwo}
\alias{ExampleThree}
\alias{ExampleFour}
\docType{data}
\title{Examples from the CALINE3 User's Guide}
\description{Four environments, each with parameters and link, receptor, and meteorology data.}
\usage{data(CALINE3.examples)}
\source{
Benson, P. (1979) CALINE-3: A versatile dispersion model for predicting air pollution levels near highways and urban streets. Federal Highway Authority report FHWA/CA/TL-79/23, California DOT, Sacramento (1979).
}
\examples{
data(CALINE3.examples)
ls(ExampleThree)
with(ExampleThree,
	CALINE3.predict(
		receptors, links, meteorology,
		averaging.time,	surface.roughness,
		settling.velocity, deposition.velocity,
		background.concentration))		
}
\keyword{datasets}
