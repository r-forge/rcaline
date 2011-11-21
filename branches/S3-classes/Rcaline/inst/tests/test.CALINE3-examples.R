context("CALINE3-examples")

data(CALINE3, package="Rcaline")

rounded <- function(x) round(matrix(x), digits=1)

test_that("Case 1: single link", {
	expect_that(SingleLink, is_a('Caline3Model'))
	pred <- predict(SingleLink)
	ambient <- 3.0
	expect_equivalent(
		rounded(pred) + ambient, 
		matrix(c(7.6)))
})

test_that("Case 2: multiple links", {	
	expect_that(MultiLink, is_a('Caline3Model'))
	pred <- predict(MultiLink)
	ambient <- 3.0
	expect_equivalent(
		rounded(pred) + ambient,
		matrix(c(6.1, 10.7, 4.4, 8.3)))
})