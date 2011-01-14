.packageName <- 'Rcaline'
.packageVersion <- '0.8'
.onLoad <- function(lib.loc, package) library.dynam('Rcaline', package, lib.loc)
.onUnload <- function(lib.loc) library.dynam.unload('Rcaline', lib.loc)
.onAttach <- function(lib.loc, package) {}
