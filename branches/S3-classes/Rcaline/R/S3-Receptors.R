ReceptorGrid <- function(links, height=1.8, resolution=100.0, maxDistance=1000.0) {
      require(rgeos)
      buf <- suppressWarnings(rgeos::gBuffer(links$polylines, width=maxDistance))
      xy <- spsample(buf, cellsize = c(resolution, resolution), type = "regular")
      coordnames(xy) <- c("x", "y")
      z <- data.frame(z = rep(1.8, length(xy)))
      spdf <- SpatialPointsDataFrame(xy, data=z)
      return(spdf)
}

ReceptorRings <- function(links, height=1.8, 
	distances=c(50, 100, 250, 500, 1000), spacing=identity, rgeos.scale=1e+06) {
		
	require(rgeos)
	rgeos::setScale(rgeos.scale)
	centerlines <- links$polylines	# FIXME: better encapsulation (maybe via as.SpatialLines()?)
	buffers <- lapply(distances, function(x) gBuffer(centerlines, width = x))
	rings <- lapply(buffers, as.SpatialLines)
	perimeter <- function(ring) sum(unlist(lapply(ring@lines, LinesLength)))
	spsample.ring <- function(ring, ring.width, spacings) {
		pts <- spsample(ring, type = "regular", n = perimeter(ring) / spacings) 
		coordnames(pts) <- c("x", "y") 
		d <- rep(ring.width, nrow(pts@coords)) 
		SpatialPointsDataFrame(pts, data.frame(distance = d, spacing = spacings))
	}
	receptors <- do.call(rbind, mapply(spsample.ring, rings, distances, spacings = spacing(distances)))
	receptors$z <- height
	return(receptors)
}