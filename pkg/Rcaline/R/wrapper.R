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

#
# Native code (Fortran) wrapper function
#

.caline3.receptor_totals <- function(
	XR, YR, ZR,
	XL1, YL1, XL2, YL2, WL, HL, TYP, VPHL, EFL,
	U, BRG, CLAS, MIXH,
	ATIM, Z0, VS = 0.0, VD = 0.0, MAXDIST = 1e5
) {	

	# Receptor specifications
	NR <- as.integer(length(XR))
	stopifnot( all.equal(NR, length(YR), length(ZR)) )
	if( any(is.na(XR)) )
		stop("All receptor X coordinates must be specified.")		
	if( any(is.na(YR)) )
		stop("All receptor Y coordinates must be specified.")
	if( any(is.na(ZR)) )
		stop("All receptor Z coordinates must be specified.")

	# Link specifications
	NL <- as.integer(length(XL1))
	stopifnot( all.equal(NL, length(YL1), length(XL2), length(YL2),
		length(WL), length(HL), length(TYP), length(VPHL), length(EFL)) )	
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
	
	# Coerce type classifications to integers 
	# (can't pass characters to .Fortran() with DUP = FALSE )
	if(is.character(TYP)) {
		clas.lookup <- list(AG=0, BR=1, FL=2, DP=3, 
			`At Grade`=0, `Bridge`=1, `Fill`=2, `Depressed`=3)
		#print(clas.lookup)
		#print(TYP)
		#print(clas.lookup[TYP])
		NTYP <- as.integer(clas.lookup[TYP])
	} else if(is.integer(TYP)) {
		NTYP <- TYP
	} else {
		stop('TYP argument must be character or integer')
	}
	
	# Meteorology specifications
	if( U < 1.0 )
		stop("Wind speeds less than 1.0 m/s are not valid input for the CALINE3 model.") 
	if( (BRG < 0) || (BRG > 360) )
		stop("Wind bearing must be in [0, 360].")
	if( (CLAS < 1) || (CLAS > 6) )
		stop("Stability class must be an integer between 1 and 6 (inclusive).")
	if( MIXH < 0 )
		stop("Mixing height cannot be negative.")
		
	# TODO: more rigorous type checking
	stopifnot(lapply(list(XR, YR, ZR), is.numeric) == TRUE)
	stopifnot(lapply(list(XL1, YL1, XL2, YL2, WL, HL, NTYP, VPHL, EFL), is.numeric) == TRUE)
	stopifnot(lapply(list(U, BRG, CLAS, MIXH), is.numeric) == TRUE)
	
	# Call native code, allocating array C to hold results
	returned_data <- .Fortran(
			"CALINE3_RECEPTOR_TOTALS", 
			NR, XR, YR, ZR,
			NL, XL1, YL1, XL2, YL2, WL, HL, NTYP, VPHL, EFL,
			U, BRG, CLAS, MIXH,
			ATIM, Z0, VS, VD, MAXDIST,
			C = as.single(array(0.0, NR)),
			PACKAGE = "Rcaline"
	)
	
	# Return results
	predicted <- as.numeric(returned_data$C)
	return(predicted)
	
}
