#' Construct a set of receptors in the vicinity of a set of links.
#'
#' This function constructs a regular Cartesian grid of receptors no more 
#' than \code{maxDistance} from \code{links}.
#'
#' @param links basis
#' @param height height in meters
#' @param resolution spacing between receptors (both directions), in meters
#' @param maxDistance TODO
#' @param rgeos.scale TODO
#'
#' @return SpatialPointsDataFrame
#'
#' @keywords receptors
#' @seealso ReceptorRings
#' @export
ReceptorGrid <- function(links, height=1.8, resolution=1000.0, maxDistance=1000.0, rgeos.scale=1e+06) {
	require(rgeos)
	rgeos::setScale(rgeos.scale)
	# TODO: take width into account (don't measure distance from centerline, but from edge of road)
	spgeom <- centerlines(links)
	buf <- rgeos::gBuffer(spgeom, width=maxDistance)
	xy <- spsample(buf, cellsize = c(resolution, resolution), type = "regular")
	coordnames(xy) <- c("x", "y")
	z <- data.frame(z = rep(1.8, length(xy)))
	spdf <- SpatialPointsDataFrame(xy, data=z)
	return(spdf)
}

#' Construct a set of receptors in the vicinity of a set of links.
#'
#' This function constructs concentric rings of receptors at specific
#' distances from \code{links}. 
#'
#' @param links basis
#' @param height height in meters
#' @param distances list of distances to the roadway centerline, in meters
#' @param spacing TODO
#' @param rgeos.scale TODO
#'
#' @return SpatialPointsDataFrame
#'
#' @keywords receptors
#' @seealso ReceptorGrid
#' @export
ReceptorRings <- function(links, height=1.8, 
	distances=c(50, 100, 250, 500, 1000), spacing=identity, rgeos.scale=1e+06) {
		
	require(rgeos)
	rgeos::setScale(rgeos.scale)
	
	# Create buffers from centerlines and 'distances' vector;
	# then discard inside/outside topology (save only the edges)
	# TODO: take width into account (don't measure distance from centerline, but from edge of road)
	buffers <- lapply(distances, function(x) gBuffer(centerlines(links), width = x)) 
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
