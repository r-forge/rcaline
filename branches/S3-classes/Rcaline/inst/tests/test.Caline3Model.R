context("S3-Caline3Model")

highways.shp <- system.file("extdata", "WestOakland", "highways.shp", package = "Rcaline")
links <- FreeFlowLinks(highways.shp, vehiclesPerHour = AADT / 24, emissionFactor = 1.0)

oakland.asc <- system.file("extdata", "WestOakland", "OaklandSTP-2000.ASC", package="Rcaline")
meteorology <- Meteorology(oakland.asc)

receptors <- ReceptorGrid(links, resolution=1000.0)

model <- Caline3Model(links, meteorology)
results <- predict(model, receptors)