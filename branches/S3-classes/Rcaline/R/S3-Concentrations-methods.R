#' Handling model results
#'
#' Aggregate the "raw" result matrix obtained from \code{\link{predict.Caline3Model}},
#' summarizing the pred estimates for each receptor. 
#'
#' Use \code{as(x, "SpatialPointsDataFrame")} to re-bind these summary statistics 
#' with the locations (and other attributes) of the rcp used in the prediction step.
#'
#' @param x concentrations obtained from \code{\link{predict.Caline3Model}}
#' @param FUN a list of summary functions to apply to each receptor location
#' @param na.rm logical; passed to each summary function in turn
#' @param ... other arguments
#'
#' @return matrix of summary statistics\
#'
#' @S3method aggregate HourlyConcentrations
#' @importFrom stats aggregate
#' @rdname HourlyConcentrations-methods
#' @export
aggregate.HourlyConcentrations <- function(x, FUN=list("min", "mean", "median", "GM", "max", "sd"), na.rm=T, ...) {
	pred <- x
	GM <- function(x, ...) exp(mean(log(x), ...))
	agg <- do.call(cbind, lapply(FUN, function(f) apply(pred, MARGIN=1, FUN=f, na.rm=na.rm)))
	colnames(agg) <- FUN
	rownames(agg) <- rownames(pred)
	class(agg) <- c("AggregatedConcentrations", "matrix")
	attr(agg, "model") <- attr(pred, "model")
	return(agg)
}

#' @param from an AggregatedConcentrations object
#'
#' @return a SpatialPointsDataFrame
#'
#' @name as.AggregatedConcentrations.SpatialPointsDataFrame
#' @rdname AggregatedConcentrations-methods
#' @export
as.AggregatedConcentrations.SpatialPointsDataFrame <- function(from) {
	model <- attr(from, 'model')
	rcp <- receptors(model)
	if("data" %in% slotNames(rcp)) {
		dat <- data.frame(rcp@data, as.data.frame(from))
	} else {
		dat <- as.data.frame(from)
	}
	spdf <- SpatialPointsDataFrame(coordinates(rcp), data=dat)
	proj4string(spdf) <- proj4string(rcp)
	return(spdf)
}

setOldClass("AggregatedConcentrations")
setAs("AggregatedConcentrations", "SpatialPointsDataFrame", as.AggregatedConcentrations.SpatialPointsDataFrame)

#' Visualization
#'
#' Plot aggregate concentrations using spplot or ggplot2.
#'
#' @param x an AggregatedConcentrations object, obtained by calling aggregate() on the results of a modeling run
#' @param ... further arguments to spplot
#'
#' @keywords spplot
#' @importFrom sp spplot
#' @rdname AggregatedConcentrations-methods
#' @export
spplot.AggregatedConcentrations <- function(obj, ...) {
	spdf <- as(obj, 'SpatialPointsDataFrame')
	spplot(spdf, ...)
}

setOldClass("AggregatedConcentrations")
setMethod("spplot", "AggregatedConcentrations", spplot.AggregatedConcentrations)

#' Visualization
#'
#' @keywords ggplot2
#' @importFrom ggplot2 ggplot
#' @S3method ggplot AggregatedConcentrations
#' @rdname AggregatedConcentrations-methods
#' @export
ggplot.AggregatedConcentrations <- function(data, select=NA, ...) {
	model <- attr(data, 'model')
	map <- ggplot(model)
	results.spatial <- as(data, 'SpatialPointsDataFrame')
	results.data <- as.data.frame(results.spatial)
	if(is.na(select)) {
		results.geom <- geom_point(
			aes(x=x, y=y, fill=mean, color=mean, size=mean, order=mean))
		map %+% results.data + results.geom
	} else {
		varnames <- names(results.spatial@data)
		results.wide <- results.data[,c('x', 'y', select)]
		results.long <- reshape(results.wide,
			idvar = c('x', 'y'),
			varying = list(select),
			times = select,
			timevar = 'Variable',
			v.names = 'Value',
			direction = 'long')
		results.geom <- geom_point(
			aes(x=x, y=y, fill=Value, color=Value, size=Value, order=Value))
		map %+% results.long + results.geom + facet_wrap(~ Variable, ...)
	}
}