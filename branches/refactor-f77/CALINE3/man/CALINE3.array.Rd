\name{CALINE3.array}
\alias{CALINE3.array}
\title{CALINE3.array}
\usage{
  CALINE3.array(XR, YR, ZR, XL1, YL1, XL2, YL2, WL, HL,
    NTYP, VPHL, EFL, U, BRG, CLAS, MIXH, ATIM, Z0, VS, VD,
    .coerce = TRUE)
}
\arguments{
  \item{XR}{x-coordinates of the receptors}

  \item{YR}{y-coordinates of the receptors}

  \item{ZR}{z-coordinates of the receptors (height above
  ground level, usually 1.8m)}

  \item{XL1}{starting x-coordinates of the links}

  \item{YL1}{starting y-coordinates of the links}

  \item{XL2}{ending x-coordinates of the links}

  \item{YL2}{ending y-coordinates of the links}

  \item{WL}{widths of the links}

  \item{HL}{heights of the links (above ground level)}

  \item{NTYP}{link classifications (1=at grade, 2=bridge,
  3=fill, 4=depressed)}

  \item{VPHL}{link-level traffic volumes, in vehicles per
  hour}

  \item{EFL}{link-level emission factors, in grams per
  vehicle-mile per hour}

  \item{U}{wind speeds, in meters per second (not less than
  1.0)}

  \item{BRG}{wind bearings, in degrees (direction wind is
  blowing from)}

  \item{CLAS}{stability classes (1, 2, 3, 4, 5, or 6)}

  \item{MIXH}{mixing heights, in meters (over 1000 skips
  mixing height calculations)}

  \item{ATIM}{averaging time, in minutes (usually 60)}

  \item{Z0}{surface roughness, in centimeters}

  \item{VS}{settling velocity, in cm/sec}

  \item{VD}{deposition velocity, in cm/sec}

  \item{.coerce}{force arguments to be cast to correct
  type}
}
\value{
  matrix of concentrations, in ug/m3
}
\description{
  Wrapper for single-precision Fortran routine.
}
\details{
  All coordinates are in meters unless otherwise specified.
}

