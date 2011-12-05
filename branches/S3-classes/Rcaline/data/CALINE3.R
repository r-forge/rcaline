require(sp)
load_all("Rcaline", TRUE)

.link <- function(ID, XL1, YL1, XL2, YL2) {
	coords <- matrix(c(XL1, YL1, XL2, YL2), ncol=2, byrow=TRUE)
	return(Lines(list(Line(coords)), ID = ID))
}

#' Single Link
#' 
#' Example one is a simple illustration of a single link with one
#' receptor located near the downwind edge of the highway. The purpose of
#' this example is to show how the model handles link.geometry which are
#' identical in every way except for their section type and source
#' height.
#'
#' The link runs in a north-south direction and is 10,000 meters long.
#' The vehicle volume (VPH) is 7500 vehicles/hour, the emission factor
#' (EF) is 30 grams/mile and the mixing zone width (W) is 30 meters. The
#' site variables used are an averaging time (ATIM) of 60 minutes, an
#' atmospheric stability (CLAS) of 6(F), deposition and settling
#' velocities (VD,VS, respectively) of 0 cm/second, an ambient CO
#' concentration (AMB) of 3.0 ppm, and a surface roughness (ZO) of 10 cm.
#' The value for the surface roughness of 10 cm was chosen because the
#' link is assumed to be located in a flat, rural area composed mainly of
#' open fields. The meteorological conditions of wind speed (U) and wind
#' angle (BRG) are 1 m/s and 270 degrees, respectively. The 270 degree
#' wind angle puts the direction of the wind perpendicular to the link
#' (crosswind) and from the west. The receptor is located 30 meters east
#' of the highway centerline at a "nose height" of 1.8 meters.
#'
#' Cases two and four involve elevated link.geometry. Each link is assigned 
#' a height of 5 meters above the datum, but for case two the link is
#' defined as a bridge section (TYP=BR), while in case four it is
#' considered a fill section (TYP=FL). The resulting CO concentrations
#' are 6.2 ppm for the "bridge" link and 7.6 ppm for the "fill" link. The
#' difference in concentration is due to the method in which contributions 
#' from the "bridge" and "fill" link.geometry are calculated. For
#' the "bridge" link in case two, it is assumed that the wind is not only
#' blowing over the link, but also underneath it. Thus, the model can use
#' the Gaussian adjustment for source height which assumes a uniform
#' vertical wind distribution both above and below the elevated source.
#' For the "fill" link, the model assumes that the wind streamlines pass
#' over the fill parallel to the ground. Thus, the model treats case four
#' just as if it were an at-grade section.
#'
#' For case three, the link is designated a depressed section (TYP=DP).
#' All conditions are identical to the previous cases except the source
#' height. CALINE3 increases the pollutant residence time within the
#' mixing zone of a depressed section, thus enhancing initial vertical
#' dispersion. This accounts for the low CO concentration of 5.8 ppm
#' predicted for case three.
#'
#' @name SingleLink
#' @docType data
#' @format A \link{Caline3Model} object
#' @references P.E. Benson. (1979) CALINE3: a versatile dispersion model 
#'	for predicting air pollutant levels near highways and arterial 
#'	streets. Technical report, PB-80-220841, California State Dept. of 
#'	Transportation, Sacramento (USA)
#' @source CALINE3 User's Guide
#' @keywords datasets
SingleLink <- Caline3Model(
	links = FreeFlowLinks(
		SpatialLinesDataFrame(
			SpatialLines(list(
				.link("LINK A", 0.00, -5000.00, 0.00, 5000.00))), 
			data = data.frame(
				VPHL = 7500,
				EFL = 30,
				WL = 30.0,
				row.names = "LINK A")),
		vehiclesPerHour = VPHL,
		emissionFactor = EFL,
		width = WL), 
	meteorology = Meteorology(data.frame(
		windSpeed = 1.00, 
		windBearing = 270.00, 
		stabilityClass = Pasquill(6), 
		mixingHeight = 1000.00)),
	receptors = Receptors(
		cbind(x = 30.0, y = 0.0), 
		z = 1.8),
	terrain = Terrain(
		surfaceRoughness = 10.0), 
	pollutant = CO)

#' Rural Curved Alignment
#' 
#' Example two depicts the application of CALINE3 to a rural, curved
#' alignment. Ten connecting link.geometry are used to model the highway. The ten
#' link.geometry represent three straight sections, a 45E curve, and a 90E curve.
#' The 90E curve is made up of five link.geometry, while the 45E curve is made up
#' of only two link.geometry. The finer resolution for the 90E curve is needed to
#' obtain an adequate approximation of the highway alignment for the
#' nearby receptors. For the given wind angle, the 45E curve will not
#' contribute significantly to any of the receptors, and thus is only
#' divided up into two link.geometry.
#' 
#' The link conditions placed on this example are a constant vehicle
#' volume of 8500 vehicles per hour and a constant emission factor of 30
#' grams/mile. Also constant for all ten link.geometry are the at-grade source
#' height, and mixing zone width of 28 meters.
#' 
#' The two important site variables to note are the ambient concentration
#' (3.0 ppm) and the surface roughness (50 cm). The surface roughness of
#' 50 cm corresponds to assumed rolling, lightly wooded terrain. The
#' model results, which include the ambient concentration, appear to be
#' consistent with what would be expected under the wind angle of 45E.
#'
#' @name RuralCurved
#' @docType data
#' @format A \link{Caline3Model} object
#' @references P.E. Benson. (1979) CALINE3: a versatile dispersion model 
#'	for predicting air pollutant levels near highways and arterial 
#'	streets. Technical report, PB-80-220841, California State Dept. of 
#'	Transportation, Sacramento (USA)
#' @source CALINE3 User's Guide
#' @keywords datasets
RuralCurved <- Caline3Model(
	links = FreeFlowLinks(
		SpatialLinesDataFrame(
			SpatialLines(list(
				.link('LINK A', -707, -707, 0, 0),
				.link('LINK B', 0, 0, 120, 175),
				.link('LINK C', 120, 175, 150, 350),
				.link('LINK D', 150, 350, 150, 1350),
				.link('LINK E', 150, 1350, 175, 1510),
				.link('LINK F', 175, 1510, 265, 1640),
				.link('LINK G', 265, 1640, 350, 1760),
				.link('LINK H', 350, 1760, 475, 1830),
				.link('LINK I', 475, 1830, 650, 1850),
				.link('LINK J', 650, 1850, 1650, 1850))),
			data = data.frame(
				VPHL = rep(8500, 10),
				EFL = rep(30, 10),
				WL = rep(28, 10), 
				row.names = paste('LINK', LETTERS[1:10]))),
		vehiclesPerHour = VPHL,
		emissionFactor = EFL,
		width = WL), 
	meteorology = Meteorology(data.frame(
		windSpeed = 1.0, 
		windBearing = 45.0, 
		stabilityClass = Pasquill(6), 
		mixingHeight = 1000.0)),
	receptors = Receptors(
		cbind(x=c(400, 100, 200, 100), y=c(1700, 1500, 1300, 350)), 
		z = 1.8),
	terrain = Terrain(
		surfaceRoughness = 50.0), 
	pollutant = CO)
