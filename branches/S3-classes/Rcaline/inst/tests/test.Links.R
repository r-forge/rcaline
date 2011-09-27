context("S3-Links")

highways.shp <- system.file("extdata", "WestOakland", "highways.shp", package = "Rcaline")

test_that("West Oakland highway data is available", {
	expect_true(file.exists(highways.shp))
})

links <- FreeFlowLinks(highways.shp, vehiclesPerHour = AADT / 24, emissionFactor = 1.0)
summary(links)

dat <- as.data.frame(links)

#plot(links)