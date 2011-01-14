\name{read.ISC}
\alias{read.ISC}
\title{Read ISCST3-formatted meteorological records}
\description{Read meteorological records formatted for use with the U.S. EPA's Short-Term Industrial Source Complex (ISCST3) model family.}
\usage{
read.ISC(file)
}
\arguments{
  \item{file}{path to ISC-formatted file containing meteorological records. Typically ends with file extension \code{.met}.}
}
\details{
The ISCST3 format is a flat ASCII file format. It consists of one free-form header row (integers separated by commas or whitespace) followed by multiple fixed-width records, one for each hour of the year.
}
\value{
\item{surface.station}{ surface station number, e.g., WBAN Number for NWS Stations }
\item{surface.year}{ year for surface data (2 or 4 digits) }
\item{upper.station}{ upper air station number }
\item{upper.year}{ year for upper air data (2 or 4 digits) }
\item{records}{ a data frame with column names and units of measure as follows:
\tabular{ll}{
\var{year}					\tab 2 digit	\cr
\var{month}					\tab 			\cr
\var{day}					\tab 			\cr
\var{hour}					\tab 24-hour	\cr
\var{wind.bearing}			\tab degrees	\cr
\var{wind.speed}			\tab meters per second		\cr
\var{stability.class}		\tab Pasquill (1--6 = A--F)	\cr
\var{temperature}			\tab Kelvin		\cr
\var{rural.mixing.height}	\tab meters		\cr
\var{urban.mixing.height}	\tab meters		\cr
}
}
}
\references{U.S. EPA (1995) EPA ISC3 User's Guide, EPA-454/B-95-003a. <http://www.epa.gov/scram001/userg/regmod/isc3v1.pdf>}
\author{David Holstius}
\examples{
print(getwd())
longbeach <- read.ISC(system.file('extdata', 'LongBeach', 'LONGBCH.met', package='Rcaline'))
head(longbeach$records)
}
\keyword{ ISC }
\keyword{ ISCST3 }
\keyword{ meteorology }
\keyword{ weather }
