id.attr <- 'Rcaline.id.attr'

as.segments.Line <- function(spobj) {
	vertices <- coordinates(spobj)	# a 2-column (?) matrix
	stopifnot(ncol(vertices) == 2)
	n <- nrow(vertices)				# the number of vertices
	x0y0 <- matrix(vertices[1:(n-1),], ncol=2)
	x1y1 <- matrix(vertices[2:n,], ncol=2)
	segs <- cbind(x0y0, x1y1)
	colnames(segs) <- c('x0','y0','x1','y1')
	return(segs)
}

as.segments.Lines <- function(spobj) {
	df <- as.data.frame(do.call(rbind, lapply(spobj@Lines, as.segments)))
	df[,id.attr] <- spobj@ID
	return(df)
}

as.segments.SpatialLines <- function(spobj) {
	seg.list <- lapply(spobj@lines, as.segments)
	do.call(rbind, seg.list)
}

as.segments.SpatialLinesDataFrame <- function(spobj) {
	segs.list <- lapply(spobj@lines, as.segments)
	segs.df <- do.call(rbind, segs.list)
	merged.df <- merge(segs.df, spobj@data, by.x=id.attr, by.y='row.names')
	merged.df[,id.attr] <- NULL
	return(merged.df)
}

as.segments <- function(spobj) stop(paste("Sorry, I don't know what to do with a", class(spobj)))
setGeneric("as.segments")
setMethod("as.segments", "SpatialLinesDataFrame", as.segments.SpatialLinesDataFrame)
setMethod("as.segments", "SpatialLines", as.segments.SpatialLines)
setMethod("as.segments", "Lines", as.segments.Lines)
setMethod("as.segments", "Line", as.segments.Line)

