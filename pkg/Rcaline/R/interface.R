#
# Public API
#

require(sp)

CALINE3.predict <- function(
	receptors,
	links,
	meteorology,
	averaging.time, 
	surface.roughness,
	settling.velocity = 0.0, 
	deposition.velocity = 0.0,
	background.concentration = 0.0
) {	
			
	# Receptor specifications
	if(hasMethod('coordinates', class(receptors))) {
		coords <- coordinates(receptors)
	} else {
		coords <- receptors[,c('x','y','z')]
	}
	XR <- as.single(coords[, 1]) 
	YR <- as.single(coords[, 2]) 
	if(ncol(coords) > 2) {
		ZR <- as.single(coords[,3])
	} else {
		ZR <- as.single(rep(1.8, length(XR)))
		warning("Receptor heights not specified. Assuming 1.8 m above ground level (average human height).")
	}

	# Link specifications
	if(is.null(links$classification)) {
		links$classification <- 'AG'
		warning('Link classifications unspecified. Assuming AG ("at grade").')
	}
	if(is.null(links$height)) {
		links$height <- 0.0
		warning('Link heights unspecified. Assuming 0.0 m above ground level.')
	}
	if(is.null(links$width)) {
		links$width <- 30.0
		warning('Link widths unspecified. Assuming 30.0 m.')
	}
	XL1 	<- as.single(links$x0) 
	YL1 	<- as.single(links$y0) 
	XL2 	<- as.single(links$x1) 
	YL2 	<- as.single(links$y1)
	WL 		<- as.single(links$width)
	HL 		<- as.single(links$height) 
	TYP 	<- as.character(links$classification, 2)
	VPHL 	<- as.single(links$flow) 
	EFL 	<- as.single(links$emissions)
	
	# Meteorology specifications
	if(is.null(meteorology$stability.class)) {
		stability.class <- 4
		warning('Stability class unspecified. Assuming 4 (Pasquill class "D").')
	}		
	if(is.null(meteorology$mixing.height)) {
		mixing.height <- 1000.0
		warning('Mixing height unspecified. Assuming 1000 m (no effect).')
	}
	U 		<- as.single(meteorology$wind.speed)
	BRG 	<- as.single(meteorology$wind.bearing) 
	CLAS 	<- as.integer(meteorology$stability.class) 
	MIXH 	<- as.single(meteorology$mixing.height)

	# Model parameters
	if(missing(averaging.time)) {
		averaging.time <- 60.0
		warning('Averaging time unspecified. Assuming 60.0 min.')
	}		
	if(missing(surface.roughness)) {
		surface.roughness <- 100.0
		warning('Surface roughness unspecified. Assuming 100 cm.')
	}
	ATIM 	<- as.single(averaging.time)
	Z0 		<- as.single(surface.roughness)
	VS 		<- as.single(settling.velocity)
	VD 		<- as.single(deposition.velocity)

	predicted <- .caline3.receptor_totals(
		XR, YR, ZR,
		XL1, YL1, XL2, YL2, WL, HL, TYP, VPHL, EFL,
		U, BRG, CLAS, MIXH,
		ATIM, Z0, VS, VD)

	# Use row names, if provided, to label results
	if(!is.null(row.names(receptors))) {
		names(predicted) <- row.names(receptors)
	}

	return(predicted + background.concentration)
	
}
