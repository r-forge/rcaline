#
# Functions for reading input from contemporary- and legacy-formatted sources.
#

read.ISC <- function(
	file
) {
	header <- readLines(file, 1)
	header.tokens <- strsplit(header, '\\s+')[[1]]
	result <- lapply(as.list(header.tokens)[-1], as.integer)
	names(result) <- c('surface.station','surface.year','upper.station','upper.year')
	result$records <- read.fortran(
		file,
		c("4I2","2F9","F6","I2","2F7"),
		skip = 1,
		col.names = c(
			"year", 
			"month", 
			"day", 
			"hour", 
			"wind.bearing", 
			"wind.speed", 
			"temperature", 
			"stability.class", 
			"rural.mixing.height", 
			"urban.mixing.height")
	)
	return(result)
	
}
