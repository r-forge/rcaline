#' CALINE3.array
#'
#' Wrapper for single-precision Fortran routine.
#'
#' All coordinates are in meters unless otherwise specified.
#' 
#' @param XR x-coordinates of the receptors
#' @param YR y-coordinates of the receptors
#' @param ZR z-coordinates of the receptors (above ground level)
#' @param XL1 starting x-coordinates of the links
#' @param YL1 starting y-coordinates of the links
#' @param XL2 ending x-coordinates of the links
#' @param YL2 ending y-coordinates of the links
#' @param WL widths of the links
#' @param HL heights of the links (above ground level)
#' @param NTYP link classifications
#' @param VPHL link-level traffic volumes, in vehicles per hour
#' @param EFL link-level emission factors, in grams per vehicle-mile [per hour]
#' @param U wind speeds, in meters per second (<1.0 results in NA)
#' @param BRG wind bearings, in degrees (direction wind is blowing from)
#' @param CLAS stability classes (1, 2, 3, 4, 5, or 6)
#' @param MIXH mixing heights
#' @param ATIM averaging time, in minutes
#' @param Z0 surface roughness, in centimeters
#' @param .coerce force arguments to be cast to correct type
#' 
#' @return matrix of concentrations, in ug/m3
#'
#' @export
CALINE3.array <- function(
	XR, YR, ZR,
	XL1, YL1, XL2, YL2, WL, HL, NTYP, VPHL, EFL,
	U, BRG, CLAS, MIXH,
	ATIM, Z0, VS, VD,
	.coerce = TRUE
) {	
	
	if (.coerce) {
		XR <- as.single(XR)
		YR <- as.single(YR)
		ZR <- as.single(ZR)
		XL1 <- as.single(XL1)
		YL1 <- as.single(YL1)
		XL2 <- as.single(XL2)
		YL2 <- as.single(YL2)
		WL <- as.single(WL)
		HL <- as.single(HL)
		NTYP <- as.integer(NTYP)
		VPHL <- as.single(VPHL)
		EFL <- as.single(EFL)
		U <- as.single(U)
		BRG <- as.single(BRG)
		CLAS <- as.integer(CLAS)
		MIXH <- as.single(MIXH)
		ATIM <- as.single(ATIM)
		Z0 <- as.single(Z0)
		VS <- as.single(VS)
		VD <- as.single(VD)
	}

	NR <- length(XR)
	NL <- length(XL1)
	shape <- c(NR, NL)
	C <- as.single(array(0.0, dim=shape))
	
	retval <- .Fortran(
	 	'CALINE3_MATRIX', 
		NR, XR, YR, ZR,
		NL, XL1, YL1, XL2, YL2, WL, HL, NTYP, VPHL, EFL,
		U, BRG, CLAS, MIXH,
		ATIM, Z0, VS, VD,
		C = C,
	 	PACKAGE = "CALINE3"
	)

	C <- with(retval, array(as.double(C), dim=shape))
	return(C)

}

