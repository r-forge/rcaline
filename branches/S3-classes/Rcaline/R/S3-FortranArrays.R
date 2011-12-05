#' Construct a FortranArrays object.
#'
#' @param as.real function to convert numeric arguments to appropriate type (single or double) 
#' @param ... named arguments
#'
#' @return a list object
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
