spplot.AggregatedConcentrations <- function(x, ...) {
	spdf <- as(x, 'SpatialPointsDataFrame')
	spplot(spdf, ...)
}

ggplot.Caline3Model <- function(x, ...) {
	receptor.data <- as.data.frame(x$receptors)
	link.data <- as.data.frame(x$links)
	receptor.layer <- geom_point(aes(x=x, y=y), pch=3, alpha=0.5, data=receptor.data)
	link.layer <- geom_segment(aes(x=XL1, y=YL1, xend=XL2, yend=YL2), data=link.data)
	base.layer <- ggplot() + coord_equal() + easting() + northing()
	return(base.layer + link.layer + receptor.layer)
}

ggplot.AggregatedConcentrations <- function(x, select=NA, ...) {
	model <- attr(x, 'model')
	map <- ggplot(model)
	results.spatial <- as(x, 'SpatialPointsDataFrame')
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
	
# scale_fill_gradient2(low="yellow", mid="orange", high="brown") + scale_alpha(to=c(0.0, 0.8)))