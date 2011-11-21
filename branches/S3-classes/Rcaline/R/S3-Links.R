#' Create a vector of link classifications.
#'
#' Basically a restricted kind of \code{\link{factor}}.
#'
#' @param x TODO
#' @keywords links
#' @export
#' @examples
#' LinkType(rep('AG', 5))
LinkType <- function(x) {
	factor(x, 
		levels = c("AG", "BR", "FL", "DP"), 
		labels = c("At Grade", "Bridge", "Fill", "Depressed")
	)
}

#' Construct a FreeFlowLinks object.
#'
#' \code{vehiclesPerHour}, \code{emissionFactor}, and other arguments
#' can all be specified as expressions. Use them the same way you
#' would use arguments to \code{\link{transform}}.
#
#' A FreeFlowLinks object can be plotted with \code{\link{plot}} or coerced 
#' to a \code{data.frame} with \code{\link{as.data.frame}}.
#' 
#' @param x a .shp file, or a SpatialLines object
#' @param vehiclesPerHour average number of vehicles per hour (also known as "flow")
#' @param emissionFactor emissions per vehicle, in grams per mile (per hour)
#' @param width mixing zone width, in meters
#' @param height elevation above ground level (not sea level!)
#' @param classification see \code{\link{LinkType}} for possible values
#' @param ... other arguments
#' 
#' @return a FreeFlowLinks object
#' @keywords links
#' @export
FreeFlowLinks <- function(x, vehiclesPerHour, emissionFactor, width, height=0.0, classification='AG', ...) 
{
	if(missing(height))
		height <- 0.0
	
	if(missing(classification))
		classification <- 'AG'
		
	if(is(x, "character")) {
		filename <- x
		sldf <- read.shp(filename)
	} else if(inherits(x, "SpatialLinesDataFrame")) {
		sldf <- x
	} else {
		stop("I don't know what to do with a ", class(x))
	}

	args <- as.list(match.call())[-1]
	formalArgs <- args[names(args) %in% names(formals())]
	
	obj <- list()
	class(obj) <- "FreeFlowLinks"
	obj$centerlines <- sldf
	obj$transformArgs <- formalArgs[-1]
	if(is.null(obj$transformArgs$height))
		 obj$transformArgs$height = height
	if(is.null(obj$transformArgs$classification))
		 obj$transformArgs$classification = classification
	
	# Precompute the segments
	attr(obj, ".data") <- as.data.frame(obj)
	
	return(obj)
}

#' Get the centerlines from a FreeFlowLinks object.
#'
#' @param links a FreeFlowLinks object
#'
#' @return a SpatialLines object
#'
#' @keywords links
#' @export
centerlines <- function(links) {
	return(links$centerlines)
}

#' Plot a FreeFlowLinks object using base graphics.
#'
#' Works just like \code{\link{lines}}.
#'
#' @param x a FreeFlowLinks object
#' @param ... other arguments
#' 
#' @keywords links
#' @S3method lines FreeFlowLinks
#' @export
lines.FreeFlowLinks <- function(x, ...) {
	lines(centerlines(x), ...)
}

#' Plot a FreeFlowLinks object using \code{ggplot2}.
#'
#' Roadways are colored by the total emissions strength, Q (g/mi),
#' the product of flow \code{vehiclesPerHour} and \code{emissionFactor}.
#' 
#' Prettier but slower than \code{\link{lines}}.
#'
#' @param x a FreeFlowLinks object
#' @param ... other arguments
#' 
#' @keywords links
#' @S3method plot FreeFlowLinks
#' @export
plot.FreeFlowLinks <- function(x, ...) {
	links <- x
	require(ggplot2)
	kilo <- function(x) x / 1000.0
	easting <- function(...) scale_x_continuous("Easting (km)", formatter = kilo, ...)
	northing <- function(...) scale_y_continuous("Northing (km)", formatter = kilo, ...)
	dat <- as.data.frame(links)
	dat <- transform(dat, Q = vehiclesPerHour * emissionFactor)
	Q.breaks <- pretty(dat$Q, n = 3)
	map <- ggplot(dat) + coord_equal() + easting() + northing()
	centerlines <- geom_segment(aes(x=XL1, y=YL1, xend=XL2, yend=YL2, color=Q))
	return(map + centerlines + scale_color_gradient2(expression(bold(Q) ~~ bgroup("(", over(g, mi %.% hr), ")")), 
		low="green", mid="yellow", high="red", midpoint=median(dat$Q)))
}

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
