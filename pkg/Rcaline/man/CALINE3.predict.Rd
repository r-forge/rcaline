\name{CALINE3.predict} 
\alias{CALINE3.predict} 
\title{Predict total concentration at each receptor} 
\description{
  Predicts the total concentration at each receptor.
} 
\usage{
CALINE3.predict(
	receptors,
	links,
	meteorology,
	averaging.time, 
	surface.roughness,
	settling.velocity = 0.0, 
	deposition.velocity = 0.0,
	background.concentration = 0.0)
} 
\arguments{
  \item{receptors}{
    a data frame with three columns, \var{x}, \var{y}, and \var{z}, giving the 
    location of each receptor. Row names, if provided, are used to label the return value(s).
  }
  \item{links}{
    a data frame with four columns, \var{x0}, \var{y0}, \var{x1}, and \var{y1}, 
    describing the location and orientation of each road segment. 
  	
  	Should also contain two columns, \var{flow} and \var{emissions}. These correspond to 
    the time-averaged volume of traffic (typically, vehicles per hour) and the emission factor 
    (average aerosol mass emitted per vehicle per unit length) for each link described in 
    \var{links}.
    
  	Optionally, \var{classification} and \var{height} columns can be supplied to
    describe elevated links (e.g., bridges) or depressed links (below ground level). The height,
    if given, should not exceed 10 m in either direction. See the CALINE3 User's Guide for details.

	Including a \var{width} column is strongly recommended. The width should include a buffer of 3 m on
    each side. A reasonable minimum would be 10 m. If not given, 30 m will be assumed.
    
    Row names, if given, are used to label the result matrix.
  }
  \item{meteorology}{
  a list with four variables describing the prevailing conditions. 
  \var{wind.bearing} specifies the prevailing source of wind, in degrees. 0 corresponds to the positive y direction (conventionally, North).
  \var{wind.speed} specifies the prevailing wind speed, in meters per second. Wind speeds under 1.0 meters per second are disallowed. See the CALINE3 User's Guide for details.
  \var{stability.class} describes the atmospheric stability. It should be 1, 2, 3, 4, 5, or 6, corresponding to Pasquill stability classes A-F, respectively.
  \var{mixing.height} can be used in simulations of noctural conditions or other instances where an inversion layer might occur. Mixing heights of 1000 meters or more have no effect on predicted concentrations.
  }
  \item{averaging.time}{
    steady-state averaging time, in minutes. Values between 3 and 120 are recommended.
  }
  \item{surface.roughness}{
    terrain surface roughness, in centimeters. Values between 3 and 400 are recommended, as exemplified below:
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
  \item{settling.velocity}{
    (optional) the settling velocity of the modeled pollutant, in meters per
    second. For carbon monoxide (the default), settling is
    negligible.
  }
  \item{deposition.velocity}{
    (optional) the deposition velocity of the modeled pollutant, in meters
    per second. For carbon monoxide, deposition is negligible.
  }
  \item{background.concentration}{
    (optional) the background concentration of the modeled pollutant. 
    Will be added to all receptor totals.
  }
  
} 

\value{
  A one-dimensional array, where the ith value is the sum of the background
  concentration (if any) plus the predicted contributions from all links to 
  the ith receptor. 
  
  If [row] names are supplied for the receptor locations, then these will be 
  used to name the entries in the array.
}

\note{
  In specifying coordinates, be sure to use a Cartesian
  coordinate reference system. Do not use geographic
  coordinates (latitude and longitude), as this could give
  unexpected results. 
}

\references{
Benson, P. (1979) CALINE-3: A versatile dispersion model for predicting air pollution levels near highways and urban streets. Federal Highway Authority report FHWA/CA/TL-79/23, California DOT, Sacramento (1979).
}

\keyword{ prediction }
