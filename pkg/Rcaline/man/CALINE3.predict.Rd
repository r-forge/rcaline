\name{CALINE3.predict} 
\alias{CALINE3.predict}
\alias{ExampleOne}
\alias{ExampleTwo}
\alias{ExampleThree}
\alias{ExampleFour}
\title{Predict concentrations at the given receptors} 
\description{
  Predicts the concentration at each receptor under each given meteorological condition.
} 
\usage{
CALINE3.predict(
	receptors,
	links,
	meteorology,
	surface.roughness,
	mixing.heights = "urban",
	averaging.time = 60.0,
	settling.velocity = 0.0, 
	deposition.velocity = 0.0)
} 
\arguments{
  \item{receptors}{
    a SpatialPoints object or a SpatialPointsDataFrame giving the location of each receptor. 
	Alternatively, this can be a regular data.frame, as long as it has columns named x and y (and, optionally, z).
    Row names, if provided, are used to label the return value(s).
  }
  \item{links}{
    a data.frame with four columns describing the location and orientation of each road segment. 
	These columns can be named x, xend, y, and yend; or they can be named x0, y0, x1, and y1.
  	
  	This data.frame must also contain two columns, \var{flow} and \var{emissions}. These correspond to 
    the time-averaged volume of traffic (typically, vehicles per hour) and the emission factor 
    (average aerosol mass emitted per vehicle per unit length) for each link described in 
    \var{links}.
    
  	A column named \var{width} is also required. The width should include a buffer of 3 m on
    each side. See the CALINE3 documentation for more.

	Optionally, \var{classification} and \var{height} columns can be supplied to
    describe elevated links (e.g., bridges) or depressed links (below ground level). The height,
    if given, should not exceed 10 m in either direction. See the CALINE3 User's Guide for details.

	
    Row names, if given, are used to label the result matrix.
  }
  \item{meteorology}{
  a data.frame with four variables describing the prevailing conditions. 
  \var{wind.bearing} specifies the prevailing source of wind, in degrees. 0 corresponds to the positive y direction (conventionally, North). A wind bearing is the direction that the wind is coming FROM.
  \var{wind.speed} specifies the prevailing wind speed, in meters per second. Wind speeds under 1.0 meters per second are disallowed. See the CALINE3 User's Guide [1] for details.
  \var{stability.class} describes the atmospheric stability. It should be 1, 2, 3, 4, 5, or 6, corresponding to Pasquill stability classes A-F, respectively. See the CALINE3 User's Guide [1] for details.
  \var{mixing.height} can be used in simulations of nocturnal conditions or other instances where an inversion layer might occur. Mixing heights of 1000 meters or more have no effect on predicted concentrations. See the CALINE3 User's Guide [1] for details.
  }
  \item{surface.roughness}{
    the terrain surface roughness length, in centimeters. Values between 3 and 400 are recommended, as exemplified below. See the CALINE3 User's Guide [1] for details.
    \tabular{lc}{
Smooth mud flats			\tab	0.001	\cr
Tarmac (pavement)			\tab	0.002	\cr
Dry lake bed				\tab	0.003	\cr
Smooth desert				\tab	0.03	\cr
Grass (5-6 cm)				\tab	0.75	\cr
Grass (4 cm)				\tab	0.14	\cr
Alfalfa (15.2 cm)			\tab	2.72	\cr
Grass (60-70 cm)			\tab	11.4	\cr
Wheat (60 cm)				\tab	22.		\cr
Corn (220 cm)				\tab	74.		\cr
Single family residential	\tab	108.	\cr
Park						\tab	127.	\cr
Office						\tab	175.	\cr
Citrus orchard				\tab	198.	\cr
Fir forest					\tab	283.	\cr
Central business district	\tab	321.	\cr
Apartment residential		\tab	370.	\cr
	}
  }
  \item{mixing.heights}{
    indicates which of the two mixing heights, urban or rural, to use (if passing a data.frame returned by read.ISC). See ?read.ISC for more.
  }
  \item{averaging.time}{
    (optional) the steady-state averaging time, in minutes. Defaults to 60; values between 3 and 120 are recommended. See the CALINE3 User's Guide [1] for details.
  }
  \item{settling.velocity}{
    (optional) the settling velocity of the modeled pollutant, in meters per
    second. For carbon monoxide (the default), settling is
    negligible and defaults to 0.0.
  }
  \item{deposition.velocity}{
    (optional) the deposition velocity of the modeled pollutant, in meters
    per second. For carbon monoxide, deposition is negligible and defaults to 0.0.
  }
  
} 

\value{
  A two-dimensional array, where the (i,j)th cell is the sum of the contributions from all links to 
  the ith receptor under the jth meteorological conditions. 
  
  To obtain the annual average concentration at each receptor, compute the row means with \code{apply(predicted, 1, mean)}.

  If row names are supplied for the receptor locations, or for the meteorological conditions, then these will be 
  used to name the entries in the resulting array.
}

\section{Warnings}{
  In specifying coordinates, be sure to use a Cartesian
  coordinate reference system. Do not use geographic
  coordinates (latitude and longitude), as this could give
  unexpected results. 
}

\examples{
data(CALINE3.examples)
attach(ExampleThree)
predicted <- CALINE3.predict(receptors, links, meteorology, surface.roughness)
show(predicted)
}

\references{
Benson, P. (1979) CALINE-3: A versatile dispersion model for predicting air pollution levels near highways and urban streets. Federal Highway Authority report FHWA/CA/TL-79/23, California DOT, Sacramento (1979).
}

\keyword{ prediction }
