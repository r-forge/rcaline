#' Convert a FreeFlowLinks object to a data.frame.
#'
#' Each row in the data.frame corresponds to one or more segments from
#' the original (polyline) geometry. The starting coordinates (XL1, YL1)
#' and ending coordinates (XL2, YL2) are preserved as columns in the data.frame. 
#' Attributes are also preserved as columns.
#'
#' @param x a FreeFlowLinks object
#' @param row.names TODO
#' @param optional TODO
#' @param ... other arguments
#' 
#' @return a data.frame
#' @keywords links
#' @export
as.data.frame.FreeFlowLinks <- function(x, row.names, optional, ...) {
	try({
		.data <- attr(x, ".data")
		if(!is.null(.data))
			return(.data)
	})
	links <- x
	segments <- decimate(centerlines(links))
	colnames(segments) <- c("XL1", "YL1", "XL2", "YL2")
	attrs <- do.call("transform", c(list(centerlines(links)@data), links$transformArgs))
	dat <- suppressWarnings(merge(segments, attrs, by="row.names"))
	colnames(dat)[colnames(dat) == "Row.names"] <- "ID"
	return(dat)
}

as.Fortran <- function(x, ...) UseMethod("as.Fortran", x)
setGeneric("as.Fortran")

as.Fortran.FreeFlowLinks <- function(x) {
	dat <- as.data.frame(x)
	with(dat, list(
		XL1 = real4(XL1),
		YL1 = real4(YL1),
		XL2 = real4(XL2),
		YL2 = real4(YL2),
		WL = real4(width),
		HL = real4(height),
		TYP = as.character(classification),
		VPHL = real4(vehiclesPerHour),
		EFL = real4(emissionFactor)
	))
}

setOldClass("FreeFlowLinks")
setMethod("as.Fortran", "FreeFlowLinks", as.Fortran.FreeFlowLinks)

as.Fortran.Meteorology <- function(x) {
	dat <- as.data.frame(x)
	with(dat, list(	
		UM = real4(windSpeed),
		BRGM = real4(windBearing),
		CLASM = as.integer(pmin(stabilityClass, Pasquill(6))),
		MIXHM = real4(mixingHeight)
	))
}

setOldClass("Meteorology")
setMethod("as.Fortran", "Meteorology", as.Fortran.Meteorology)

as.Fortran.Receptors <- function(x) {
	dat <- as.data.frame(x)
	elev <- attr(x, "elevation")
	with(dat, list(
		XR = real4(x),
		YR = real4(y),
		ZR = real4(rep(elev, nrow(dat)))
	))
}

setMethod("as.Fortran", "SpatialPoints", as.Fortran.Receptors)

as.Fortran.Parameters <- function(x) {
	dat <- as.data.frame(x)
	with(dat, list(
		ATIM = real4(averagingTime),
		Z0 = real4(surfaceRoughness),
		VS = real4(settlingVelocity),
		VD = real4(depositionVelocity)
	))
}

setOldClass("Parameters")
setMethod("as.Fortran", "Parameters", as.Fortran.Parameters)
