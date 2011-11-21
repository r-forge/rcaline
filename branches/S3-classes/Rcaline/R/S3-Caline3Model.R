links <- function(model, ...) UseMethod("links")
links.default <- function(model, ...) model$links

receptors <- function(model, ...) UseMethod("receptors")
receptors.default <- function(model, ...) {
	rcp <- model$receptors
	expect_true(identical(coordnames(rcp), c("x", "y")))
	return(rcp)
}

meteorology <- function(model, ...) UseMethod("meteorology")
meteorology.default <- function(model, ...) model$meteorology

pollutant <- function(model, ...) UseMethod("pollutant")
pollutant.default <- function(model, ...) model$pollutant

terrain <- function(model, ...) UseMethod("terrain")
terrain.default <- function(model, ...) model$terrain

#' Construct a Caline3Model object.
#'
#' The model (object) contains all of the link and met data,
#' as well as site-specific model parameters, such as surface roughness.
#'
#' Internally, these are stored as single-precision arrays. If you want to
#' experiment by changing a parameter, construct a new Caline3Model, unless 
#' you really know what you are doing.
#'
#' Use \code{\link{predict.Caline3Model}} to predict concentrations at a given set of rcp.
#'
#' @param lnk a \code{\link{FreeFlowLinks}} object
#' @param met a \code{\link{Meteorology}} object
#' @param rcp a \code{\link{Receptors}} object
#' @param ter a \code{\link{Terrain}} object
#' @param pollutant a \code{\link{Pollutant}} object
#' 
#' @return a Caline3Model object
#' 
#' @keywords model
#' @seealso predict.Caline3Model HourlyConcentrations AggregatedConcentrations
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

#' Use a Caline3Model to predict concentrations at a given set of rcp.
#'
#' Returns "raw" estimates in the form of a matrix. Each row corresponds to a receptor,
#' and each column to a meteorological condition. Use \code{aggregate()}
#' on the result to obtain summary statistics for each receptor: mean, max, etc.
#'
#' @param object a \code{\link{Caline3Model}} object
#' @param rcp created with \code{\link{ReceptorRings}} or \code{\link{ReceptorGrid}}
#' @param .parallel logical; attempt to use the \code{foreach} package to exploit multiple cores or hosts?
#' @param ... other arguments
#'
#' @return matrix of predicted values
#'
#' @keywords predict model
#' @S3method predict Caline3Model
#' @importFrom stats predict
#' @export
predict.Caline3Model <- function(object, .parallel=TRUE, ...) {
	
	stopifnot(inherits(object, 'Caline3Model'))
	lnk <- links(object)
	met <- meteorology(object)
	rcp <- receptors(object)
	ter <- terrain(object)
	pol <- pollutant(object)
	
	# Default to sequential processing
	if(missing(.parallel)) 
		.parallel <- FALSE
	
	# Disallow parallel processing in Mac OS X "R.app" GUI
	if(.Platform$GUI %in% c("AQUA") && .parallel==TRUE) {
		warning("Using R.app precludes safe use of multicore. Try xterm instead?")
		.parallel = FALSE
		registerDoSEQ()
	}
	
	# This should be quick
	# TODO: split by receptors or meteorology, whichever is greater
	NR <- nrow(as.data.frame(rcp))
	NM <- nrow(as.data.frame(met))
	NL <- nrow(as.data.frame(lnk))
	
	if(.parallel == TRUE) {
	
		require(foreach)
		require(multicore)
		n.cores <- multicore:::detectCores() - 1
		require(doMC)
		registerDoMC(cores=n.cores)
	
		if(NR > NM) {
			# Split by rcp
			k <- sort(rep(1:n.cores, length.out=NR))
			jobs <- suppressWarnings(split(rcp, k))
			pred <- foreach(rcp=iter(jobs), .combine=rbind) %dopar% 
				run.CALINE3(lnk, met, rcp, ter, pol)
		} else {
			# Split by pred conditions
			k <- sort(rep(1:n.cores, length.out=NM))
			jobs <- suppressWarnings(split(met, k))
			pred <- foreach(met=iter(jobs), .combine=cbind) %dopar% 
				run.CALINE3(lnk, met, rcp, ter, pol)
		}
		
	} else {
		pred <- run.CALINE3(lnk, met, rcp, ter, pol)
	}	

	attr(pred, "model") <- object
	return(pred)
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

#' Aggregate the "raw" result matrix obtained from \code{\link{predict.Caline3Model}},
#' summarizing the pred estimates for each receptor. 
#'
#' Use \code{as(x, "SpatialPointsDataFrame")} to re-bind these summary statistics 
#' with the locations (and other attributes) of the rcp used in the prediction step.
#'
#' @param x pred concentrations obtained from \code{\link{predict.Caline3Model}}
#' @param FUN a list of summary functions to apply to each receptor
#' @param na.rm logical; passed to each summary function in turn
#' @param ... other arguments
#'
#' @return matrix of summary statistics\
#'
#' @S3method aggregate HourlyConcentrations
#' @importFrom stats aggregate
#' @export
aggregate.HourlyConcentrations <- function(x, FUN=list("min", "mean", "median", "GM", "max", "sd"), na.rm=T, ...) {
	pred <- x
	GM <- function(x, ...) exp(mean(log(x), ...))
	agg <- do.call(cbind, lapply(FUN, function(f) apply(pred, MARGIN=1, FUN=f, na.rm=na.rm)))
	colnames(agg) <- FUN
	rownames(agg) <- rownames(pred)
	class(agg) <- c("AggregatedConcentrations", "matrix")
	attr(agg, "model") <- attr(pred, "model")
	return(agg)
}

setOldClass("AggregatedConcentrations")

#' Re-bind results obtained from \code{aggregate(x)} to a SpatialPointsDataFrame.
#'
#' @param from an AggregatedConcentrations object
#'
#' @return a SpatialPointsDataFrame
#'
#' @name as
#' @keywords predict model
#' @export
setAs("AggregatedConcentrations", "SpatialPointsDataFrame", function(from) {
	model <- attr(from, 'model')
	rcp <- model$rcp
	if("data" %in% slotNames(rcp)) {
		dat <- data.frame(rcp@data, as.data.frame(from))
	} else {
		dat <- as.data.frame(from)
	}
	spdf <- SpatialPointsDataFrame(coordinates(rcp), data=dat)
	proj4string(spdf) <- proj4string(rcp)
	return(spdf)
})

