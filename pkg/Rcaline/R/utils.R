#
# Private, undocumented helper functions.
#

segmentize <- function(shp) {
	# Convert a SpatialLines object to a collection of segments.
	require(spatstat)
	patterns <- lapply(shp@lines, function(x) as.psp(SpatialLines(list(x))))
	df <- do.call(rbind, lapply(patterns, as.data.frame))
	return(df)
}

fast.dilation <- function(shp, radius, eps, dmin) {
	if(missing(eps)) eps <- radius / 10
	if(missing(dmin)) dmin <- eps * 2
	geom <- as.psp(SpatialLines(shp@lines))
	mask <- dilation(geom, radius, eps=eps, polygonal=FALSE)
	poly <- simplify.owin(as.polygonal(mask), dmin)
	return(as(poly, "SpatialPolygons"))
}

import.shapefile <- function(file) {
	#
	# Read a shapefile and convert to Rcaline's preferred representation.
	#
	# Presently returns a data.frame, but the plan is to use the 'psp' class 
	# as soon as multiple marks are supported by the 'spatstat' package.
	#
	require(maptools)
	shp <- readShapeSpatial(file)
	segments <- segmentize(shp)
	merge(segments, shp, by.x = "marks", by.y = "row.names")
}

plot.scenario <- function(receptor.grid, predicted) {
	make.scale <- function(map.bbox) {
		row.names(map.bbox) <- c("x","y")
		map.center <- apply(map.bbox, 1, mean)
		map.size <- map.bbox[,"max"] - map.bbox[,"min"]
		width <- floor(map.size["x"] / 5e3) * 1e3
		left <- map.center - map.size * 0.475
		right <- left + c(width, 0)
		bar = list("SpatialPolygonsRescale", 
			layout.scale.bar(), offset = left, scale = width, 
			fill = c("transparent","black"), which = 1)
		label1 = list("sp.text", left, "0", which = 1, adj=c(0.5,-1.0))
		label2 = list("sp.text", right, sprintf("%d m", width), which = 1, adj=c(0.5,-1.0))
		list(bar, label1, label2)
	}
	map.bbox <- receptor.grid@bbox 
	map.elements <- make.scale(map.bbox)
	cells <- SpatialPointsDataFrame(
		receptor.grid,
		data.frame(predicted))
	gridded(cells) <- TRUE
	heatmap.colors <- colorRampPalette(c("white","orange","darkred"))
	spplot(cells, main='Predicted concentrations', col.regions=heatmap.colors, sp.layout=map.elements)
}
