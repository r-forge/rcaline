#' Check if a package is installed.
#'
#' @param pkgname name of the package
#' @return logical
#' @export
is.installed <- function(pkgname) pkgname %in% .packages(TRUE)

#' Convert a numeric vector to 32-bit (4-byte) precision.
#'
#' @param x numeric vector
#' @export
real4 <- function(x) as.single(x)

#' Get the "units" attribute on a numeric vector.
#'
#' @param x numeric vector
#' @keywords units
#' @name units
#' @export
units <- function(x) attr(x, "units")

#' Set the "units" attribute on a numeric vector.
#'
#' @usage units(x) <- value
#'
#' @param value replacement value
#'
#' @keywords units
#' @rdname units
#' @export
`units<-` <- function(x, value) {
	attr(x, "units") <- value
	return(x)
}

.kiloFormatter <- function(x) x / 1000
easting <- function(...) scale_x_continuous("Easting (km)", formatter=.kiloFormatter, ...)
northing <- function(...) scale_y_continuous("Northing (km)", formatter=.kiloFormatter, ...)

#' Take a random sample of rows from a data.frame.
#'
#' @param x a data.frame
#' @param p the fraction of rows to sample (between 0 and 1)
#' @keywords sample data.frame
#' @return a subset of the original data.frame
#' @export
sample.data.frame <- function(x, p) x[sample(1:nrow(x), p * nrow(x)),]
