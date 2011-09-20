context("S3-Links")

fn <- system.file("extdata", "WestOakland", "highways.shp", package = "Rcaline")

test_that("West Oakland highway data is available", {
	expect_true(file.exists(fn))
})

links <- FreeFlowLinks(fn, vehiclesPerHour = AADT / 24, emissionFactor = 1.0)

#plot(links)