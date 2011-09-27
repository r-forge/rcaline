context("S3-Caline3Model")
require(Rcaline)

# Import links
links <- FreeFlowLinks(
	system.file("extdata", "WestOakland", "highways.shp", package = "Rcaline"), 
	vehiclesPerHour = AADT / 24, 
	emissionFactor = 1.0)

meteorology <- Meteorology(
	system.file("extdata", "WestOakland", "OaklandSTP-2000.ASC", package="Rcaline"))

# Select just a fraction of the meteorological records, 
# so that the computations are faster
meteorology$records <- meteorology$records[1:24,]

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