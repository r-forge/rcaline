#
# R code wrapper. Performs some basic argument checking.
#

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
	
	if(is.null(meteorology$stability.class)) {
		stability.class <- 4
		warning('Stability class unspecified. Assuming 4 (Pasquill class "D").')
	}
		
	if(is.null(meteorology$mixing.height)) {
		mixing.height <- 1000.0
		warning('Mixing height unspecified. Assuming 1000 m (no effect).')
	}
	
	if(missing(averaging.time)) {
		averaging.time <- 60.0
		warning('Averaging time unspecified. Assuming 60.0 min.')
	}
		
	if(missing(surface.roughness)) {
		surface.roughness <- 100.0
		warning('Surface roughness unspecified. Assuming 100 cm.')
	}
	
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

	XL1 	<- as.single(links$x0) 
	YL1 	<- as.single(links$y0) 
	XL2 	<- as.single(links$x1) 
	YL2 	<- as.single(links$y1)
	WL 		<- as.single(links$width)
	HL 		<- as.single(links$height) 
	TYP 	<- as.character(links$classification, 2)
	VPHL 	<- as.single(links$flow) 
	EFL 	<- as.single(links$emissions)
	U 		<- as.single(meteorology$wind.speed)
	BRG 	<- as.single(meteorology$wind.bearing) 
	CLAS 	<- as.integer(meteorology$stability.class) 
	MIXH 	<- as.single(meteorology$mixing.height)
	ATIM 	<- as.single(averaging.time)
	Z0 		<- as.single(surface.roughness)
	VS 		<- as.single(settling.velocity)
	VD 		<- as.single(deposition.velocity)

	f <- function(X, Y, Z) {
		
		predicted <- .caline3.array(
			as.single(X), as.single(Y), as.single(Z),
			XL1, YL1, XL2, YL2, WL, HL, TYP, VPHL, EFL,
			U, BRG, CLAS, MIXH,
			ATIM, Z0, VS, VD)
	
		# Average over meteorological conditions (if more than 1)
		link_contributions <- apply(predicted, c(2,3), mean)
		
		return(sum(link_contributions))
	}
	
	vf <- Vectorize(f) 
	receptor_totals <- vf(XR, YR, ZR)

	# Use row names, if provided, to label results
	if(!is.null(row.names(receptors))) {
		names(receptor_totals) <- row.names(receptors)
	}

	return(receptor_totals + background.concentration)
	
}

#
# Native code (Fortran) wrapper function
#

.caline3.array <- function(
	XR, YR, ZR,
	XL1, YL1, XL2, YL2, WL, HL, TYP, VPHL, EFL,
	U, BRG, CLAS, MIXH,
	ATIM, Z0, VS = 0.0, VD = 0.0, MAXDIST = 1e5
) {	

	# TODO: type checking (all values should be single-precision)

	if( any(is.na(XR)) )
		stop("All receptor X coordinates must be specified.")
		
	if( any(is.na(YR)) )
		stop("All receptor Y coordinates must be specified.")

	if( any(is.na(ZR)) )
		stop("All receptor Z coordinates must be specified.")

	stopifnot( all.equal(length(XR), length(YR), length(ZR)) )
	
	if( any(is.na(WL)) )
		stop("All link widths must be specified.")
	
	if( any(is.na(HL)) )
		stop("All link heights must be specified.")
	
	if( any(is.na(TYP)) )
		stop("All link classifications must be specified.")

	if( any(is.na(VPHL)) )
		stop("All links must have a non-NULL flow (vehicles per hour).") 
	
	if( any(is.na(EFL)) )
		stop('All links must have a non-NULL emission factor (grams per vehicle-mile).') 

	stopifnot( all.equal(length(XL1), length(YL1), length(XL2), length(YL2),
		length(WL), length(HL), length(TYP), length(VPHL), length(EFL)) )
	
	if( any(U < 1.0) )
		stop("Wind speeds less than 1.0 m/s are not valid input for the CALINE3 model.") 
	
	if( any(BRG < 0) || any(BRG > 360) )
		stop("Wind bearing must be in [0, 360].")
		
	if( any(CLAS < 1) || any(CLAS > 6) )
		stop("Stability class must be an integer between 1 and 6 (inclusive).")
	
	if( any(MIXH < 0) )
		stop("Mixing height cannot be negative.")
	
	stopifnot( all.equal(length(U), length(BRG), length(CLAS), length(MIXH)) )

	# Allocate array to hold results
	NR 		<- as.integer(length(XR))
	NL 		<- as.integer(length(XL1))
	NM 		<- as.integer(length(U))
	C 		<- as.single(array(0.0, c(NR, NL, NM)))
	
	# Call native code
	returned_data <- suppressWarnings(
		.Fortran(
			'CALINE3_ARRAY', 
			NR, XR, YR, ZR,
			NL, XL1, YL1, XL2, YL2, WL, HL, TYP, VPHL, EFL,
			NM, U, BRG, CLAS, MIXH,
			ATIM, Z0, VS, VD, MAXDIST,
			C = C,
			PACKAGE = 'Rcaline'
		)
	)
	
	# Convert 1D array result back to 3D array 
	predicted <- as.numeric(returned_data$C)
	dim(predicted) <- c(NM, NL, NR)
	return(predicted)
	
}
