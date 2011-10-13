context("S3-Meteorology")

SF.isc <- system.file("extdata", "BayArea", "Meteorology", "met_5801.isc", package="Rcaline")

test_that("San Francisco meteorology", {
	expect_true(file.exists(SF.isc))
})

test_that("Pasquill() constructor", {
	x <- Pasquill(1:4)
	expect_that(length(x), equals(4))
})

#test_that("ISCFile() constructor", {
#	isc <- ISCFile(oakland.asc)
#	expect_that(nrow(isc$records), equals(366 * 24))
#})

test_that("Meteorology() constructor", {
	met <- Meteorology(SF.isc)
	expect_true(inherits(met, "Meteorology"))
	expect_that(nrow(met), equals(366 * 24))
})
