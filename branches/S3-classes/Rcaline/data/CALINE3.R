require(sp)
load_all("Rcaline", TRUE)

.link <- function(ID, XL1, YL1, XL2, YL2) {
	coords <- matrix(c(XL1, YL1, XL2, YL2), ncol=2, byrow=TRUE)
	return(Lines(list(Line(coords)), ID = ID))
}

#
# Case 1
#

.SingleLink.sldf <- SpatialLinesDataFrame(
	SpatialLines(list(
		.link("LINK A", 0.00, -5000.00, 0.00, 5000.00))), 
	data = data.frame(
		VPHL = 7500,
		EFL = 30,
		WL = 30.0,
		row.names = "LINK A"))

SingleLink <- Caline3Model(
	links = FreeFlowLinks(.SingleLink.sldf,
		vehiclesPerHour = VPHL,
		emissionFactor = EFL,
		width = WL), 
	meteorology = Meteorology(data.frame(
		windSpeed = 1.00, 
		windBearing = 270.00, 
		stabilityClass = Pasquill(6), 
		mixingHeight = 1000.00)),
	receptors = Receptors(
		x = 30.0, 
		y = 0.0, 
		z = 1.8),
	terrain = Terrain(
		surfaceRoughness = 10.0), 
	pollutant = CO)

#
# Case 2
#

.MultiLink.sldf <-  SpatialLinesDataFrame(
	SpatialLines(list(
		.link('LINK A', -707, -707, 0, 0),
		.link('LINK B', 0, 0, 120, 175),
		.link('LINK C', 120, 175, 150, 130),
		.link('LINK D', 150, 350, 150, 1350),
		.link('LINK E', 175, 1510, 265, 1640),
		.link('LINK F', 265, 1640, 350, 1760),
		.link('LINK G', 350, 1760, 475, 1830),
		.link('LINK H', 475, 1830, 650, 1850),
		.link('LINK I', 650, 1850, 1650, 1850))),
	data = data.frame(
		VPHL = rep(8500, 9),
		EFL = rep(30, 9),
		WL = rep(28, 9), 
		row.names = paste('LINK', LETTERS[1:9])))

MultiLink <- Caline3Model(
	links = FreeFlowLinks(.MultiLink.sldf,
		vehiclesPerHour = VPHL,
		emissionFactor = EFL,
		width = WL), 
	meteorology = Meteorology(data.frame(
		windSpeed = 1.0, 
		windBearing = 45.0, 
		stabilityClass = Pasquill(6), 
		mixingHeight = 1000.0)),
	receptors = Receptors(
		x = c(400, 100, 200, 100), 
		y = c(1700, 1500, 1300, 350), 
		z = c(1.8, 1.8, 1.8, 1.8)),
	terrain = Terrain(
		surfaceRoughness = 50.0), 
	pollutant = CO)
