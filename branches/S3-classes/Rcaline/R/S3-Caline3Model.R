Caline3Model <- function(links, meteorology, surfaceRoughness, 
	averagingTime=60.0, settlingVelocity=0.0, depositionVelocity=0.0) {
	
	if(missing(surfaceRoughness)) {
		surfaceRoughness <- 300.0
		warning("Surface roughness not specified. Defaulting to ", surfaceRoughness, " cm")
	}

	obj <- list(
		links = links,
		meteorology = meteorology,
		surfaceRoughness = surfaceRoughness,
		averagingTime = averagingTime,
		pollutant = Pollutant("CO", 28.0)
	)
	
	class(obj) <- "Caline3Model"
	return(obj)
		
}

predict.Caline3Model <- function(model, receptors) {
	
    real4 <- as.single
	
	attach(as.data.frame(receptors))
	XR <- real4(x)
	YR <- real4(y)
	ZR <- real4(z)
	
	attach(model)
	XL1 <- real4(links$XL1)
	YL1 <- real4(links$YL1)
	XL2 <- real4(links$XL2)
	YL2 <- real4(links$YL2)
	WL <- real4(links$width)
	HL <- real4(links$height)
	TYP <- links$classification
	VPHL <- real4(links$vehiclesPerHour)
	EFL <- real4(links$emissionFactor)
	
	UM <- real4(meteorology$windSpeed)
	BRGM <- real4(meteorology$windBearing)
	CLASM <- as.integer(meteorology$stabilityClass)
	MIXHM <- real4(meteorology$mixingHeight)
	
	ATIM <- averagingTime
	Z0 <- surfaceRoughness
	VS <- pollutant$settlingVelocity
	VD <- pollutant$depositionVelocity
	
	detach(model)
	
	summary(CLASM)
	
	#results <- .caline3.receptor_totals(
	#	XR, YR, ZR,
	#	XL1, YL1, XL2, YL2, WL, HL, TYP, VPHL, EFL,
	#	UM, BRGM, CLASM, MIXHM,
	#	ATIM, Z0, VS = 0.0, VD = 0.0
	#)
	
}