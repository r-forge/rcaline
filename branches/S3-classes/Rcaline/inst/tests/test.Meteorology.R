context("S3-Meteorology")
require(Rcaline)

fn <- system.file("extdata", "WestOakland", "OaklandSTP-2000.ASC", package="Rcaline")

test_that("West Oakland meteorology", {
	expect_true(file.exists(fn))
})

test_that("Pasquill() constructor", {
	x <- Pasquill(1:4)
	expect_that(length(x), equals(4))
})

test_that("ISCFile() constructor", {
	isc <- ISCFile(fn)
	expect_that(nrow(isc$records), equals(366 * 24))
})

test_that("Meteorology() constructor", {
	met <- Meteorology(fn)
	expect_that(nrow(met$records), equals(366 * 24))
})
