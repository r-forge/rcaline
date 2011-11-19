#' Create a vector of atmospheric stability classes.
#'
#' Basically a restricted kind of \code{\link{factor}}.
#'
#' @param x TODO
#' @keywords meteorology
#' @export
#' @examples
#' LinkType(rep('AG', 5))
Pasquill <- function(x) {
	obj <- ordered(x, levels=1:7, labels=LETTERS[1:7])
	#class(obj) <- "Pasquill"
	return(obj)
}

#' Read a file in ISC format.
#'
#' Use \code{\link{Meteorology}} to get meteorology in the format
#' required by CALINE3.
#'
#' @param filename filename
#'
#' @return an ISCFile object
#'
#' @keywords meteorology
#' @export
ISCFile <- function(filename) {

	# Assert that the file exists
	stopifnot(file.exists(filename))
	
	# Skip the header and read the records
	obj <- read.fwf(filename, 
		widths = c(2, 2, 2, 2, 9, 9, 6, 2, 7, 7),
		skip = 1,
		stringsAsFactors = FALSE,
		# c("4X2", "2F9", "F6", "I2", "2F7"),
		colClasses = c("integer", "integer", "integer", "integer",
			"numeric", "numeric", "numeric", "integer",
			"numeric", "numeric"),
		col.names = c("year", "month", "day", "hour", 
			"flowVector", "windSpeed", "temperature", "stabilityClass", 
			"mixingHeight.rural", "mixingHeight.urban")
	)
	
	if(all(obj$year < 70)) {
		warning("Adding 2000 to two-digit years")
		obj <- transform(obj, year = year + 2000)
      } else if(all(obj$year < 100)) {
      	warning("Adding 1900 to two-digit years")
      	obj <- transform(obj, year = year + 1900)
      }
	
	# FIXME put the header data here
	attr(obj, "metadata") <- NA
	
	row.names(obj) <- with(obj,
		as.POSIXlt(sprintf("%02d/%02d/%02d %02d:00:00", year, month, day, hour - 1))
	)	
      
	class(obj) <- c("ISCFile", class(obj))
	return(obj)

}

#' Load meteorological data into memory.
#'
#' Compatible with ISC-format files. Rotates the "flow vector" (wind direction)
#' from ISC-format files by 180 degrees to obtain the wind bearing. Defaults to
#' retaining the urban mixing heights only. 
#'
#' @param iscfile filename or ISCFile object
#' @param use whether to use urban or rural mixing heights
#'
#' @return a Meteorology object (basically a \code{data.frame})
#'
#' @keywords meteorology
#' @export
Meteorology <- function(iscfile, use = c("urban", "rural")) {

	# Determine whether to use urban or rural mixing heights column
	use <- match.arg(use)
	message("Using ", use, " mixing heights (See ?Meteorology for more).")
	
	# Load ISC file
	if(!inherits(iscfile, "ISCFile")) 
		iscfile <- ISCFile(iscfile)

	# Rotate flow vector by 180 to get wind bearing;
	# Convert stability classes to Pasquill;
	# Discard rural or urban mixing height (depending on user's preference)
	rotate.degrees <- function(angle, by) ((angle + by) + 360) %% 360
	met <- with(iscfile, data.frame( 
			windSpeed = windSpeed, 
			windBearing = rotate.degrees(flowVector, by = 180.0),
			stabilityClass = Pasquill(stabilityClass),
			mixingHeight = switch(use, 
				rural = mixingHeight.rural, 
				urban = mixingHeight.urban
			)
		)
	)
	row.names(met) <- row.names(iscfile)

	# Warn about calm wind speeds
	calm <- which(met$windSpeed < 1)
	if(length(calm) > 0) {
		warning(length(calm), " wind speeds less than 1.0 m/s (will produce NAs)")
		show(met[calm,])
	}

	met <- within(met, {
		units(windSpeed) <- "m/s"
		units(windBearing) <- "deg"
		units(mixingHeight) <- "m"
	})
	class(met) <- c("Meteorology", "data.frame")
	return(met)
}

#' Visualize meteorological data.
#'
#' Plots a wind rose using ggplot2.
#'
#' @param x a Meteorology object
#' @param ... other arguments
#'
#' @keywords meteorology
#' @S3method plot Meteorology
#' @export
plot.Meteorology <- function(x, ...) {
	require(ggplot2)
	p <- ggplot(data=as.data.frame(x))
	p <- p + geom_histogram(aes(x=windBearing, y=..density..), binwidth=45/2)
	p <- p + scale_x_continuous(limits=c(0,360), breaks=seq(0, 360, by=45))
	return(p + coord_polar(theta='x'))
}
