#' Check if a package is installed.
#'
#' @param pkgname name of the package
#' @return logical
#' @export
is.installed <- function(pkgname) pkgname %in% .packages(TRUE)

#' Convert a numeric vector to 32-bit (4-byte) precision.
#'
#' @param x numeric vector
#' @export
real4 <- function(x) as.single(x)

#' Get the "units" attribute on a numeric vector.
#'
#' @param x numeric vector
#' @keywords units
#' @name units
#' @export
units <- function(x) attr(x, "units")

#' Set the "units" attribute on a numeric vector.
#'
#' @usage units(x) <- value
#'
#' @param value replacement value
#'
#' @keywords units
#' @rdname units
#' @export
`units<-` <- function(x, value) {
	attr(x, "units") <- value
	return(x)
}

.kiloFormatter <- function(x) x / 1000
easting <- function(...) scale_x_continuous("Easting (km)", formatter=.kiloFormatter, ...)
northing <- function(...) scale_y_continuous("Northing (km)", formatter=.kiloFormatter, ...)

#' Take a random sample of rows from a data.frame.
#'
#' @param x a data.frame
#' @param p the fraction of rows to sample (between 0 and 1)
#' @keywords sample data.frame
#' @return a subset of the original data.frame
#' @export
sample.data.frame <- function(x, p) x[sample(1:nrow(x), p * nrow(x)),]

#' Suppress all output while evaluating an expression.
#'
#' @param expr an \link{expression}
#' @return the value of expr
#' @seealso suppressMessages suppressWarnings
#' @export
suppressOutput <- function(expr) {
	if(.Platform$OS.type == 'windows') {
		sink("NUL")
	} else {
		sink("/dev/null")
	}
	result <- eval(expr)
	sink()
	return(result)
}

#' Read a shapefile into memory.
#'
#' The shapefile must be in projected coordinates. Latitude and 
#' longitude are not acceptable. If needed, reproject the shapefile
#' first using your favorite GIS program. If you don't have ArcGIS,
#' try Quantum GIS (it's free).
#'
#' Requires \code{rgdal} (FIXME: use maptools also?)
#' 
#' @param filename filename
#' @param ... other arguments
#'
#' @keywords shapefile
#' @export
read.shp <- function(filename, ...) {
	message("Loading shapefile: ", filename)
	if(is.installed("rgdal")) {
		suppressPackageStartupMessages(require(rgdal))
		dsn <- dirname(filename)
		layer <- gsub(".shp$", "", basename(filename))
		spobj <- suppressOutput(rgdal::readOGR(dsn, layer, ...)) 
	} else {
		stop("rgdal must be installed in order to read shapefiles.") 
	}
	if(is.na(proj4string(spobj))) {
		warning("Missing projection info for ", filename, ". Assuming planar coords.")
	} else {
		if(!is.projected(spobj)) {
			stop(filename, " has non-planar coordinates. See ?read.shp for more.")
		}
	}
	return(spobj)
}
