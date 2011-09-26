highways.shp <- system.file("extdata", "WestOakland", "highways.shp", package = "Rcaline")
links <- FreeFlowLinks(highways.shp, vehiclesPerHour = AADT / 24, emissionFactor = 1.0)

receptors <- ReceptorGrid(links)
if(interactive()) plot(receptors)

receptors <- ReceptorRings(links)
if(interactive()) plot(receptors)
