#' Modeling
#'
#' Construct, run, summarize and show results from a Caline3Model object.
#'
#' The model (object) contains all of the link and met data,
#' as well as site-specific model parameters, such as surface roughness.
#' Internally, these are stored as single-precision arrays. If you want to
#' experiment by changing a parameter, construct a new Caline3Model, unless 
#' you really know what you are doing.
#'
#' Use \code{predict} to predict concentrations; \code{aggregate} to compute summary statistics,
#' such as means and maxima; and \code{spplot} or \code{ggplot} to visualize results.
#'
#' @param links a \code{\link{FreeFlowLinks}} object
#' @param meteorology a \code{\link{Meteorology}} object
#' @param receptors a \code{\link{Receptors}} object
#' @param terrain a \code{\link{Terrain}} object
#' @param pollutant a \code{\link{Pollutant}} object
#' 
#' @return a Caline3Model object
#' 
#' @keywords model predict aggregate spplot ggplot2
#' @export
Caline3Model <- function(links, meteorology, receptors, terrain, pollutant) {
	stopifnot(inherits(links, "FreeFlowLinks"))
	stopifnot(inherits(meteorology, "Meteorology"))
	stopifnot(inherits(receptors, "SpatialPoints"))	
	stopifnot(inherits(terrain, "Terrain"))	
	stopifnot(inherits(pollutant, "Pollutant"))
	obj <- list(
		links = links,
		meteorology = meteorology,
		receptors = receptors,
		terrain = terrain,
		pollutant = pollutant)
	class(obj) <- "Caline3Model"
	return(obj)
}

run.CALINE3 <- function(lnk, met, rcp, ter, pol) {
	
	# Initialize the full matrix
	pred <- matrix(NA, 
		nrow = nrow(rcp), ncol = nrow(met), 
		dimnames = list(rownames(rcp), rownames(met)))	
	
	# Compute only the conditions (columns) for which wind speed >= 1.0
	non.calm <- with(met, which(windSpeed >= 1.0))
	args <- c(
		as.Fortran(lnk), 
		as.Fortran(met[non.calm,]), 
		as.Fortran(rcp),
		list(
			ATIM = real4(60.0),
			Z0 = real4(ter$surfaceRoughness),
			VS = real4(pol$settlingVelocity),
			VD = real4(pol$depositionVelocity)))
	computed <- do.call(".caline3.receptor_totals", args)
	
	# Assign the computed estimates back to the matrix
	stopifnot(nrow(computed) == nrow(pred))
	stopifnot(ncol(computed) == length(non.calm))
	pred[,non.calm] <- computed
	
	MOWT <- pol$molecularWeight
	if(!is.na(MOWT)) {
		FPPM <- 0.0245 / MOWT
		pred <- pred * FPPM 
	}
	
	class(pred) <- c("HourlyConcentrations", "matrix")
	return(pred)
}

