context("S3-Classes")

data(SanFrancisco, package="Rcaline")

expect_that(STRte_BAAQMD_v2.shp, is_a('SpatialLinesDataFrame'))

lnk <- FreeFlowLinks(STRte_BAAQMD_v2.shp,
	vehiclesPerHour = TRVol2009 / 24, 
	emissionFactor = 1.0,
	width = 30.0)
expect_that(lnk, is_a('FreeFlowLinks'))

# Conventional: Cartesian receptor grid
rcp <- ReceptorGrid(lnk, elevation=1.8, resolution=500)
expect_that(rcp, is_a('SpatialPoints'))

# Better: construct rcp based on distance to roadway
rcp <- ReceptorRings(lnk, elevation=1.8, distances=c(200, 500, 1000))
expect_that(rcp, is_a('SpatialPoints'))

# met_5801.isc is provided by package data
expect_warning(
	met <- Meteorology(met_5801.isc, use='urban')[1:3,],
	fixed('2 wind speeds less than 1.0 m/s (will produce NAs)'))
	
# Semi-urban terrain
ter <- Terrain(surfaceRoughness = 80.0)

# Put them all together
mod <- Caline3Model(lnk, met, rcp, ter, pollutant = CO)
expect_equal(links(mod), lnk)
expect_equal(receptors(mod), rcp)
expect_equal(meteorology(mod), met)
expect_equal(terrain(mod), ter)
expect_equal(pollutant(mod), CO)

# # Compute predicted concentrations
pred <- predict(mod, .parallel=FALSE)
# 
# load_all('Rcaline', TRUE)
# agg <- aggregate(pred)
# as(agg, 'SpatialPointsDataFrame')
# 
# load_all('Rcaline', TRUE)
# ggplot(mod)
# 
# load_all('Rcaline', TRUE)
# ggplot(agg, fill=mean, color=mean)
# ggplot(agg, select=c('mean', 'median', 'max'), nrow=1)

# test_that("Parallel vs sequential", {

    # sequential <- predict(mod, .parallel=FALSE)
	# parallel <- predict(mod, .parallel=TRUE)
      
    # expect_equal(nrow(rcp), nrow(parallel$predicted), nrow(sequential$predicted))
    # expect_equal(nrow(met), ncol(parallel$predicted), ncol(sequential$predicted))
    # expect_true(all.equal(parallel$predicted, sequential$predicted, check.attributes=FALSE))

# })