context("S3-Caline3Model")

load_all('Rcaline', TRUE)
data(SanFrancisco, package="Rcaline")

links <- FreeFlowLinks(STRte_BAAQMD_v2.shp,
	vehiclesPerHour = TRVol2009 / 24, 
	emissionFactor = 1.0)

receptors <- ReceptorGrid(links, elevation=1.8, resolution=500)

# plot(SF_county.shp, border="lightgray")
# lines(links)
# points(receptors, pch="+", col="darkgray")

# Better: construct receptors based on distance to roadway
receptors <- ReceptorRings(links, elevation=1.8, distances=c(200, 500, 1000))

# plot(SF_county.shp, border="lightgray")
# lines(links)
# points(receptors, pch="+", col="darkgray")

meteorology <- Meteorology(met_5801.isc)
parameters <- Parameters(surfaceRoughness = 80.0)
model <- Caline3Model(links, meteorology, receptors, parameters)

test_that("Parallel vs sequential", {

      parallel <- predict(model, .parallel=TRUE)
      sequential <- predict(model, .parallel=FALSE)

      expect_equal(nrow(receptors), nrow(parallel$predicted), nrow(sequential$predicted))
      expect_equal(nrow(meteorology), ncol(parallel$predicted), ncol(sequential$predicted))
      expect_true(all.equal(parallel$predicted, sequential$predicted, check.attributes=FALSE))

})