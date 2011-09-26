highways.shp <- system.file("extdata", "WestOakland", "highways.shp", package = "Rcaline")
links <- FreeFlowLinks(highways.shp, vehiclesPerHour = AADT / 24, emissionFactor = 1.0)
grd <- ReceptorGrid(links, resolution=500.0)
