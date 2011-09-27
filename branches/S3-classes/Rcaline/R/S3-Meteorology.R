# Factory method (converts numbers to ordered factor)
Pasquill <- function(x) {
	obj <- ordered(x, levels=1:7, labels=LETTERS[1:7])
	#class(obj) <- "Pasquill"
	return(obj)
}

# Factory method (from ISC-format metfile)
ISCFile <- function(filename) {

	# Assert that the file exists
	stopifnot(file.exists(filename))
	
	# Skip the header and read the records
	records <- read.fwf(filename, 
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
	
	obj <- list(
		metadata = NA,
		records = records
	)
	class(obj) <- "ISCFile"
	return(obj)

}

Meteorology <- function(x) {
	if(class(x) == "ISCFile") isc <- x
	else isc <- ISCFile(x)
	obj <- list(metadata = isc$metadata, records = isc$records)
	row.names(obj$records) <- with(obj$records,
		as.POSIXlt(sprintf("%02d/%02d/%02d %02d:00:00", year, month, day, hour - 1))
	)
	class(obj) <- "Meteorology"
	return(obj)
}

as.data.frame.Meteorology <- function(met, use = c("urban", "rural")) {

	# For converting wind directions to wind bearings
	rotate <- function(angle, by) ((angle + by) + 360) %% 360

	# Determine whether to use urban or rural mixing heights column
	use <- match.arg(use)
	message("Using ", use, " mixing heights")

	# Rotate flow vector by 180 to get wind bearing;
	# Convert stability classes to Pasquill;
	# Discard rural or urban mixing height (depending on user's preference)
	transformedRecords <- with(met$records, data.frame( 
			windSpeed = windSpeed, 
			windBearing = rotate(flowVector, by = 180.0),
			stabilityClass = Pasquill(stabilityClass),
			mixingHeight = switch(use, 
				rural = mixingHeight.rural, 
				urban = mixingHeight.urban
			)
		)
	)
	row.names(transformedRecords) <- row.names(met$records)
	return(transformedRecords)
}

plot.Meteorology <- function(met) {
	require(ggplot2)
	p <- ggplot(data=as.data.frame(met))
	p <- p + geom_histogram(aes(x=windBearing, y=..density..), binwidth=45/2)
	p <- p + scale_x_continuous(limits=c(0,360), breaks=seq(0, 360, by=45))
	return(p + coord_polar(theta='x'))
}