.onLoad <- function(libname=NULL, pkgname="CALINE3") {
	try(library.dynam('CALINE3', pkgname, libname))
}
.onAttach <- function(libname, pkgname) {}
.onUnload <- function(libpath) library.dynam.unload('CALINE3', libpath)

#' CALINE3 provides an interface to a Fortran implementation of CALINE3.
#'
#' @name CALINE3
#' @docType package
#'
#' @title CALINE3
#' @author David Holstius \email{david.holstius@@berkeley.edu}
#'
#' @references 
#' Benson, P. (1979) CALINE-3: A versatile dispersion model for predicting air pollution levels near highways and urban streets. Federal Highway Authority report FHWA/CA/TL-79/23, California DOT, Sacramento (1979).
#'
#' @keywords package disperson CALINE CALINE3 traffic
#'
NA
