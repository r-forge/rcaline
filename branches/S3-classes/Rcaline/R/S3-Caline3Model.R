#' Construct a Caline3Model object.
#'
#' The model (object) contains all of the link and meteorology data,
#' as well as site-specific model parameters, such as surface roughness.
#'
#' Internally, these are stored as single-precision arrays. If you want to
#' experiment by changing a parameter, construct a new Caline3Model, unless 
#' you really know what you are doing.
#'
#' Use \code{\link{predict.Caline3Model}} to predict concentrations at a given set of receptors.
#'
#' @param links a \code{\link{FreeFlowLinks}} object
#' @param meteorology a \code{\link{Meteorology}} object
#' @param surfaceRoughness defaults to 300.0 (semi-urban); values between 0-400 cm are sane.
#' @param averagingTime in minutes
#' @param settlingVelocity of the modeled pollutant
#' @param depositionVelocity of the modeled pollutant
#' 
#' @return a Caline3Model object
#' 
#' @keywords model
#' @seealso predict.Caline3Model HourlyConcentrations AggregatedConcentrations
#' @export
Caline3Model <- function(links, meteorology, receptors, parameters) {
	stopifnot(inherits(links, "FreeFlowLinks"))
	stopifnot(inherits(meteorology, "Meteorology"))
	#stopifnot(inherits(receptors, "Receptors"))	
	stopifnot(inherits(parameters, "Parameters"))
	obj <- list(
		links = links,
		meteorology = meteorology,
		receptors = receptors,
		parameters = parameters)
	class(obj) <- "Caline3Model"
	return(obj)
}

#' Use a Caline3Model to predict concentrations at a given set of receptors.
#'
#' Returns "raw" estimates in the form of a matrix. Each row corresponds to a receptor,
#' and each column to a meteorological condition. Use \code{aggregate()}
#' on the result to obtain summary statistics for each receptor: mean, max, etc.
#'
#' @param object a \code{\link{Caline3Model}} object
#' @param receptors created with \code{\link{ReceptorRings}} or \code{\link{ReceptorGrid}}
#' @param .parallel logical; attempt to use the \code{foreach} package to exploit multiple cores or hosts?
#' @param ... other arguments
#'
#' @return matrix of predicted values
#'
#' @keywords predict model
#' @S3method predict Caline3Model
#' @importFrom stats predict
#' @export
predict.Caline3Model <- function(object, .parallel, ...) {
	
	links <- object$links
	receptors <- object$receptors
	meteorology <- object$meteorology
	parameters <- object$parameters

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
	NR <- nrow(as.data.frame(receptors))
	NM <- nrow(as.data.frame(meteorology))
	NL <- nrow(as.data.frame(links))
	
	# Should be fixed
	expect_true(identical(coordnames(receptors), c("x", "y")))
	
	if(.parallel == TRUE) {
	
		require(foreach)
		require(multicore)
		n.cores <- multicore:::detectCores() - 1
		require(doMC)
		registerDoMC(cores=n.cores)
	
		if(NR > NM) {
			# Split by receptors
			k <- sort(rep(1:n.cores, length.out=NR))
			jobs <- suppressWarnings(split(receptors, k))
			hourly <- foreach(receptors=iter(jobs), .combine=rbind) %dopar% 
				run.CALINE3(links, meteorology, receptors, parameters)
		} else {
			# Split by hourly conditions
			k <- sort(rep(1:n.cores, length.out=NM))
			jobs <- suppressWarnings(split(meteorology, k))
			hourly <- foreach(meteorology=iter(jobs), .combine=cbind) %dopar% 
				run.CALINE3(links, meteorology, receptors, parameters)
		}
		
	} else {
		hourly <- run.CALINE3(links, meteorology, receptors, parameters)
	}	

	return(hourly)
}

run.CALINE3 <- function(links, meteorology, receptors, parameters)
	if(missing(parameters)) {
		parameters = Parameters(surfaceRoughness = 80.0)
		warning("Missing parameters. Defaults subsituted.")
	}
	fortranArgs <- sapply(c(links, meteorology, receptors, parameters), as.Fortran)
	hourly <- do.call(".caline3.receptor_totals", fortranArgs)
	rownames(hourly) <- rownames(receptors)
	colnames(hourly) <- rownames(meteorology)
	class(hourly) <- c("HourlyConcentrations", "matrix")
	attr(hourly, "model") <- model
	return(hourly)
	
}

#' Aggregate the "raw" result matrix obtained from \code{\link{predict.Caline3Model}},
#' summarizing the hourly estimates for each receptor. 
#'
#' Use \code{as(x, "SpatialPointsDataFrame")} to re-bind these summary statistics 
#' with the locations (and other attributes) of the receptors used in the prediction step.
#'
#' @param x hourly concentrations obtained from \code{\link{predict.Caline3Model}}
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
	hourly <- x
	GM <- function(x, ...) exp(mean(log(x), ...))
	agg <- do.call(cbind, lapply(FUN, function(f) apply(hourly, 1, f, na.rm=na.rm)))
	colnames(agg) <- FUN
	rownames(agg) <- rownames(hourly)
	units(agg) <- units(hourly)
	class(agg) <- c("AggregatedConcentrations", "matrix")
	attr(agg, "model") <- attr(hourly, "model")
	attr(agg, "receptors") <- attr(hourly, "receptors")
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
	receptors <- attr(from, "receptors")
	if("data" %in% slotNames(receptors)) {
		dat <- data.frame(receptors@data, as.data.frame(from))
	} else {
		dat <- as.data.frame(from)
	}
	spdf <- SpatialPointsDataFrame(coordinates(receptors), data=dat)
	proj4string(spdf) <- proj4string(receptors)
	return(spdf)
})

