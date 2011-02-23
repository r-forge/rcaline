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
