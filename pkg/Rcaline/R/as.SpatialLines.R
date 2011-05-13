as.SpatialLines.SpatialPolygons <- function(spobj) {
	stopifnot(class(spobj)=="SpatialPolygons")
	as.Lines <- function(polygons) Lines(lapply(polygons@Polygons, Line), ID=polygons@ID)
	result <- SpatialLines(lapply(spobj@polygons, as.Lines))
	proj4string(result) <- proj4string(spobj)
	return(result)
}

as.SpatialLines <- function(spobj) stop(paste("Sorry, I don't know what to do with a", class(spobj)))
setGeneric("as.SpatialLines")
setMethod("as.SpatialLines", "SpatialPolygons", as.SpatialLines.SpatialPolygons)
