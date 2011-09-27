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

predict.Caline3Model <- function(model, receptors, .parallel=TRUE) {
	rcp <- as.data.frame(receptors)
	if(.Platform$GUI %in% c("AQUA") && .parallel) {
		warning("The R.app GUI precludes the safe use of multicore functionality. You can use Terminal though ...")
		.parallel = FALSE
		registerDoSEQ()
	}
	if(.parallel) {
		require(foreach)
		require(multicore)
		n.cores <- multicore:::detectCores() - 1
		require(doMC)
		registerDoMC(cores=n.cores)
		k <- sort(rep(1:n.cores, length.out=nrow(rcp)))
		jobs <- suppressWarnings(split(rcp, k))
		pred <- foreach(rcp=iter(jobs), .combine=rbind) %dopar% predict(model, rcp, .parallel=FALSE)
	} else {
		args <- c(list(XR = real4(rcp$x), YR = real4(rcp$y), ZR = real4(rcp$z)), model$.args)
		pred <- do.call(".caline3.receptor_totals", args)
	}	
	rownames(pred) <- rownames(receptors)
	colnames(pred) <- rownames(model$meteorology$records)
	obj <- list(
		model = model,
		receptors = receptors,
		predicted = pred)
	class(obj) <- "Caline3Estimate"
	return(obj)
}

summary.Caline3Estimate <- function(est, functions=list("min", "mean", "median", "GM", "max", "sd"), window=1, na.rm=T) {
	rcp <- est$receptors
	GM <- function(x) exp(mean(log(x)))
	MA <- function(w) function(x) as.numeric(na.omit(filter(x, rep(1/w, w))))
	summary.values <- lapply(functions, function(f) apply(est$predicted, 1, f))
	stopifnot(nrow(summary.values) == nrow(rcp))
	summary.data <- as.data.frame(do.call(cbind, summary.values))
	colnames(summary.data) <- functions
	obj <- SpatialPointsDataFrame(rcp, data=data.frame(rcp@data, summary.data))
	return(obj)
}