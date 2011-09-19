is.installed <- function(pkgname) pkgname %in% .packages(TRUE)

LinkType <- function(x) {
	factor(x, 
		levels = c("AG", "BR", "FL", "DP"), 
		labels = c("At Grade", "Bridge", "Fill", "Depressed")
	)
}

read.shp <- function(filename, ...) {
	if(is.installed("rgdal")) {
		suppressPackageStartupMessages(require(rgdal))
		dsn <- dirname(filename)
		layer <- gsub(".shp$", "", basename(filename))
		sldf <- suppressMessages(rgdal::readOGR(dsn, layer, ...)) 
	} else {
		stop("rgdal must be installed in order to read shapefiles.") 
	}
	return(sldf)
}

# Factory method for filename or SpatialLines* object
FreeFlowLinks <- function(
	x, 
	vehiclesPerHour = stop("vehiclesPerHour must be specified"), 
	emissionFactor = stop("emissionFactor must be specified"),
	...) 
{
	if(missing(vehiclesPerHour))
		stop("vehiclesPerHour must be specified")
		
	if(missing(emissionFactor))
		stop("emissionFactor must be specified")
		
	if(is(x, "character")) {
		filename <- x
		sldf <- read.shp(filename)
	} else if(extends(x, "SpatialLinesDataFrame")) {
		sldf <- x
	} else {
		stop("I don't know what to do with a ", class(x))
	}
	
	geoms <- decimate(sldf)
	colnames(geoms) <- c("XL1", "YL1", "XL2", "YL2")
	
	args <- as.list(match.call())[-1]
	formalArgs <- args[names(args) %in% names(formals())]
	transformArgs <- c(list(sldf@data), formalArgs[-1])
	attrs <- do.call("transform", transformArgs)
	
	dat <- suppressWarnings(merge(geoms, attrs, by="row.names"))
	colnames(dat)[colnames(dat) == "Row.names"] <- "ID"
	class(dat) <- c("FreeFlowLinks", "data.frame")
	return(dat)
}

plot.FreeFlowLinks <- function(links, ...) {
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

highways.shp <- system.file("extdata", "WestOakland", "highways.shp", package = "Rcaline")
links <- FreeFlowLinks(highways.shp, vehiclesPerHour = AADT / 24, emissionFactor = 1.0)
plot(links)
