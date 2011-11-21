#' Construct a set of receptors in the vicinity of a set of links.
#'
#' This function constructs a regular Cartesian grid of receptors no more 
#' than \code{maxDistance} from \code{links}.
#'
#' @param links basis
#' @param elevation elevation in meters
#' @param resolution spacing between receptors (both directions), in meters
#' @param maxDistance TODO
#' @param rgeos.scale TODO
#'
#' @return SpatialPointsDataFrame
#'
#' @keywords receptors
#' @seealso ReceptorRings
#' @export
ReceptorGrid <- function(links, elevation=1.8, resolution=1000.0, maxDistance=1000.0, rgeos.scale=1e+06) {
	require(rgeos)
	rgeos::setScale(rgeos.scale)
	# TODO: take width into account (don't measure distance from centerline, but from edge of road)
	spgeom <- centerlines(links)
	buf <- rgeos::gBuffer(spgeom, width=maxDistance)
	xy <- spsample(buf, cellsize = c(resolution, resolution), type = "regular")
	coordnames(xy) <- c("x", "y")
	spobj <- SpatialPoints(xy)
	return(ReceptorSurface(spobj, elevation=elevation))
}

#' Construct a set of receptors in the vicinity of a set of links.
#'
#' This function constructs concentric rings of receptors at specific
#' distances from \code{links}. 
#'
#' @param links basis
#' @param elevation elevation in meters
#' @param distances list of distances to the roadway centerline, in meters
#' @param spacing TODO
#' @param rgeos.scale TODO
#'
#' @return SpatialPointsDataFrame
#'
#' @keywords receptors
#' @seealso ReceptorGrid
#' @export
ReceptorRings <- function(links, elevation=1.8, 
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
	spobj <- do.call(rbind, mapply(spsample.ring, rings, distances, spacings=spacing(distances)))
	return(ReceptorSurface(spobj, elevation=elevation))
}

#' Read receptor locations from a shapefile layer.
#'
#' @param file shapefile layer (filename ending in ".shp")
#' @param elevation elevation in meters
#' @param distances list of distances to the roadway centerline, in meters
#' @param spacing TODO
#' @param rgeos.scale TODO
#'
#' @return SpatialPointsDataFrame
#'
#' @keywords receptors
#' @seealso ReceptorGrid
#' @export
ReceptorLocations <- function(x, elevation=1.8, ...) {
	if(inherits(x, "Spatial")) {
		spobj <- x
	} else if(file.exists(x)) {
		spobj <- read.shp(x)
	} else {
		stop("I don't know what to do with a ", class(x))
	}
	return(ReceptorSurface(spobj, elevation=elevation))
}

#' Promote a SpatialPoints* object to a ReceptorSurface object.
#'
#' @param spobj an object of class SpatialPoints or SpatialPointsDataFrame
#' @param elevation elevation above ground, in meters
#'
#' @return object of class ReceptorSurface
#'
#' @keywords receptors
#' @export
ReceptorSurface <- function(spobj, elevation) {
	stopifnot(inherits(spobj, "SpatialPoints"))
	rcp <- spobj
	coordnames(rcp) <- c("x", "y")
	attr(rcp, "elevation") <- elevation
	return(rcp)
}

Receptors <- function(x, y, z, rownames) {
	coords <- cbind(x, y)
	dat <- data.frame(z=rep(z, len=nrow(coords)))
	spdf <- SpatialPointsDataFrame(coords, data=dat)
	if(missing(rownames)) {
		rownames <- paste('RECP.', row.names(spdf))
	}
	row.names(spdf) <- rownames
	return(spdf)
}

as.Fortran.Receptors <- function(x) {
	dat <- as.data.frame(x)
	if(!('z' %in% names(dat))) {
		dat$z <- attr(x, "elevation")
	}
	with(dat, list(XR=real4(x), YR=real4(y), ZR=real4(z)))
}

setMethod("as.Fortran", "SpatialPoints", as.Fortran.Receptors)

