# Generic function for splitting polylines into segments
setGeneric("decimate", function(x, ...) standardGeneric("decimate"))

decimate.default <- function(x, suffixes=c(".start",".end")) {
	coords <- coordinates(x)
	result <- cbind(coords[-nrow(coords),,drop=FALSE], coords[-1,,drop=FALSE])
	if(!is.null(colnames(coords)))
		colnames(result) <- paste(colnames(coords), rep(suffixes, each=ncol(coords)), sep="")
	return(result)
}

decimate.Lines <- function(x, ...) {
	nested <- rapply(x@Lines, decimate.default, ..., how="list")
	flattened <- do.call(rbind, nested)
	rownames(flattened) <- rep(x@ID, nrow(flattened))
	return(flattened)
}

decimate.SpatialLines <- function(x, ...) {
	nested <- lapply(x@lines, decimate.Lines)
	do.call(rbind, nested)
}

setMethod("decimate", "Lines", decimate.Lines)
setMethod("decimate", "SpatialLines", decimate.SpatialLines)