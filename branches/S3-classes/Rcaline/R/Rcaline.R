.onLoad <- function(lib.loc=NULL, package="Rcaline") {
	library.dynam('Rcaline', package, lib.loc)
}
.onUnload <- function(lib.loc=NULL) library.dynam.unload('Rcaline', lib.loc)
.onAttach <- function(lib.loc, package) {}
