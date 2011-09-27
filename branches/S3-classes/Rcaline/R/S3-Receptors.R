ReceptorGrid <- function(links, height=1.8, resolution=1000.0, maxDistance=1000.0, rgeos.scale=1e+06) {
	require(rgeos)
	rgeos::setScale(rgeos.scale)
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
	
	# Create buffers from centerlines and 'distances' vector;
	# then discard inside/outside topology (save only the edges)
	centerlines <- links$polylines	# FIXME: better encapsulation (maybe via as.SpatialLines()?)
	buffers <- lapply(distances, function(x) gBuffer(centerlines, width = x)) 
	rings <- lapply(buffers, as.SpatialLines)
	
	# Sample at fixed intervals along each ring
	perimeter <- function(ring) sum(unlist(lapply(ring@lines, LinesLength)))
	spsample.ring <- function(ring, ring.width, spacings) {
		pts <- spsample(ring, type = "regular", n = perimeter(ring) / spacings) 
		coordnames(pts) <- c("x", "y") 
		d <- rep(ring.width, nrow(pts@coords)) 
		SpatialPointsDataFrame(pts, data.frame(distance = d, spacing = spacings))
	}
	receptors <- do.call(rbind, mapply(spsample.ring, rings, distances, spacings = spacing(distances)))
	
	# Add the user-specified height
	receptors$z <- height
	
	return(receptors)
}