context("S3-Caline3Model")
require(Rcaline)
BayArea.roadways <- read.shp(system.file("extdata", "BayArea", "Roadways", "STRte_BAAQMD_v2.shp", package="Rcaline"))
SF.county <- read.shp(system.file("extdata", "BayArea", "Counties", "SF_county.shp", package="Rcaline"))

# Region of interest: SF county + 2000 m buffer
region <- gBuffer(SF.county, width=2000)

# Transform roadways to the same coordinate system
datum <- CRS(proj4string(region))
BayArea.roadways <- spTransform(BayArea.roadways, datum)

# Select only those roadways that intersect the region of interest
in.region <- as.logical(gIntersects(region, BayArea.roadways, byid=TRUE))
SF.roadways <- BayArea.roadways[in.region,]
links <- FreeFlowLinks(SF.roadways,
	vehiclesPerHour = TRVol2009 / 24, 
	emissionFactor = 1.0)

# Convention: construct receptors as a Cartesian grid
receptors <- ReceptorGrid(links, height=1.8, resolution=500)
plot(SF.county, border="lightgray")
lines(links)
points(receptors, pch="+", col="darkgray")

# Better: construct receptors based on distance to roadway
receptors <- ReceptorRings(links, height=1.8, distances=c(200, 500, 1000))
plot(SF.county, border="lightgray")
lines(links)
points(receptors, pch="+", col="darkgray")

# Import meteorology
meteorology <- Meteorology(system.file("extdata", "BayArea", "Meteorology", "met_5801.isc", package="Rcaline"))

save(meteorology, links, receptors, file="/opt/Rcaline/branches/S3-classes/Rcaline/data/SanFrancisco.RData")

# Select just a fraction of the meteorological records, 
# so that the computations are faster

# Construct model
expect_that(model <- Caline3Model(links, meteorology), 
	gives_warning("Surface roughness not specified"))

receptors <- ReceptorGrid(links, resolution=1000.0)

parallel <- predict(model, receptors, .parallel=TRUE)
sequential <- predict(model, receptors, .parallel=FALSE)

expect_equal(nrow(receptors), nrow(parallel$predicted), nrow(sequential$predicted))
expect_equal(nrow(meteorology$records), ncol(parallel$predicted), ncol(sequential$predicted))
expect_true(all.equal(parallel$predicted, sequential$predicted, check.attributes=FALSE))

fn <- paste(tempfile(), ".Rdata", sep="", collapse="")
save.image(file=fn)
message("Test data saved to ", fn)