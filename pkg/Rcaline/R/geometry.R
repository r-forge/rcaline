#
# Helper functions for line decimation, reprojection, etc.
#

hypot <- function(x0, y0, x1, y1) {
	dx <- x1 - x0
	dy <- y1 - y0
	sqrt(dx*dx + dy*dy)
}

flatten.line <- function(line) {
	vertices <- line@coords
	n <- nrow(vertices)
	segments <- data.frame(
		x0 = vertices[1:(n-1),1],
		y0 = vertices[1:(n-1),2], 
		x1 = vertices[2:n,1], 
		y1 = vertices[2:n,2])
	return(segments)
}

flatten.polyline <- function(polyline, compute.lengths=TRUE) {
	segments <- do.call(rbind, lapply(polyline@Lines, flatten.line))
	segments$polyline.ID <- polyline@ID
	if(compute.lengths)
		segments$length <- hypot(segments$x0, segments$y0, segments$x1, segments$y1)
	return(segments)
}

flatten.polyline_layer <- function(shp) {
	segments <- do.call(rbind, lapply(shp@lines, flatten.polyline))
	attrs <- shp@data
	combined <- merge(segments, attrs, by.x='polyline.ID', by.y='row.names')
	combined[,-which(names(combined)=='polyline.ID')]
}

shp2link <- function(shp, transform) {
	if (! missing(transform))
		shp <- spTransform(shp, transform)
	links <- flatten.polyline_layer(shp)
}

