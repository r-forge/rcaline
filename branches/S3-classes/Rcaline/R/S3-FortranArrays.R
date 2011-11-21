#' Construct a FortranArrays object.
#'
#' @param as.real function to convert numeric arguments to appropriate type (single or double) 
#' @param ... named arguments
#'
#' @return a list object
#' @example FortranArrays(XR=1:3, YR=1:3, ZR=1.8)
#' @export
FortranArrays <- function(as.real=as.single, ...) {
	args <- data.frame(...)
	#args <- as.list(match.call())[-1]
	print(args)
	types <- lapply(args, typeof)
	obj <- args
	class(obj) <- "FortranArrays"
    return(obj)
}

as.Fortran <- function(x, ...) UseMethod("as.Fortran", x)
setGeneric("as.Fortran")

#as.Fortran.data.frame <- function(x, ...) {
#	return(cbind(apply(x, 1, real4)))
#}