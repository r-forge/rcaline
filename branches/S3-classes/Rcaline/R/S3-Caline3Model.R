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
Caline3Model <- function(links, meteorology, surfaceRoughness, 
	averagingTime=60.0, settlingVelocity=0.0, depositionVelocity=0.0) {
	
	if(missing(surfaceRoughness)) {
		surfaceRoughness <- 300.0
		warning("Surface roughness not specified. Defaulting to ", surfaceRoughness, " cm")
	}
	
	lnk <- as.data.frame(links)
	met <- as.data.frame(meteorology)
	pollutant <- Pollutant("CO", 28.0)	# FIXME: allow to vary
	
	args <- list(
		XL1 = real4(lnk$XL1),
		YL1 = real4(lnk$YL1),
		XL2 = real4(lnk$XL2),
		YL2 = real4(lnk$YL2),
		WL = real4(lnk$width),
		HL = real4(lnk$height),
		TYP = as.character(lnk$classification),
		VPHL = real4(lnk$vehiclesPerHour),
		EFL = real4(lnk$emissionFactor),	
		UM = real4(pmax(met$windSpeed, 1.0)),	# round calm winds up to 1.0 m/s
		BRGM = real4(met$windBearing),
		CLASM = as.integer(met$stabilityClass),
		MIXHM = real4(rep(1e5, length(met$mixingHeight))),						# FIXME: was:real4(met$mixingHeight)
		ATIM = real4(averagingTime),
		Z0 = real4(surfaceRoughness),
		VS = real4(pollutant$settlingVelocity),
		VD = real4(pollutant$depositionVelocity))

	obj <- list(
		links = links,
		meteorology = meteorology,
		surfaceRoughness = surfaceRoughness,
		averagingTime = averagingTime,
		pollutant = pollutant,
		.args = args)
	
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
predict.Caline3Model <- function(object, receptors, .parallel=TRUE, ...) {
	model <- object
	rcp <- as.data.frame(receptors)
	if(.Platform$GUI %in% c("AQUA") && .parallel) {
		warning("Using R.app precludes safe use of multicore. Try xterm instead?")
		.parallel = FALSE
		registerDoSEQ()
	}
	if(.parallel == TRUE) {
		require(foreach)
		require(multicore)
		n.cores <- multicore:::detectCores() - 1
		require(doMC)
		registerDoMC(cores=n.cores)
		k <- sort(rep(1:n.cores, length.out=nrow(rcp)))
		jobs <- suppressWarnings(split(rcp, k))
		hourly <- foreach(rcp=iter(jobs), .combine=rbind) %dopar% predict(model, rcp, .parallel=FALSE)
	} else {
		args <- c(list(XR = real4(rcp$x), YR = real4(rcp$y), ZR = real4(rcp$z)), model$.args)
		hourly <- do.call(".caline3.receptor_totals", args)
	}	
	rownames(hourly) <- rownames(receptors)
	colnames(hourly) <- rownames(model$meteorology)
	class(hourly) <- c("HourlyConcentrations", "matrix")
	attr(hourly, "model") <- model
	attr(hourly, "receptors") <- receptors
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
#' @importClassesFrom sp SpatialPointsDataFrame
#' @export
setAs("AggregatedConcentrations", "SpatialPointsDataFrame", function(from) {
	receptors <- attr(from, "receptors")
	spdf <- SpatialPointsDataFrame(
		coordinates(receptors), 
		data = data.frame(receptors@data, as.data.frame(from))
	)
	proj4string(spdf) <- proj4string(receptors)
	return(spdf)
})

