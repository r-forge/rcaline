#' predict.Caline3Model
#'
#' @param object a \code{\link{Caline3Model}} object
#' @param .parallel logical; attempt to use the \code{foreach} package to exploit multiple cores or hosts?
#'
#' @return matrix of predicted values
#'
#' @keywords predict model
#' @S3method predict Caline3Model
#' @importFrom stats predict
#' @rdname Caline3Model-methods
#' @export
predict.Caline3Model <- function(object, .parallel=TRUE) {
	
	stopifnot(inherits(object, 'Caline3Model'))
	lnk <- links(object)
	met <- meteorology(object)
	rcp <- receptors(object)
	ter <- terrain(object)
	pol <- pollutant(object)
	
	# Default to sequential processing
	if(missing(.parallel)) 
		.parallel <- FALSE
	
	# Disallow parallel processing in Mac OS X "R.app" GUI
	if(.Platform$GUI %in% c("AQUA") && .parallel==TRUE) {
		warning("Using R.app precludes safe use of multicore. Try xterm instead?")
		.parallel = FALSE
		registerDoSEQ()
	}
	
	# This should be quick
	# TODO: split by receptors or meteorology, whichever is greater
	NR <- nrow(as.data.frame(rcp))
	stopifnot(NR > 0)
	NM <- nrow(as.data.frame(met))
	NL <- nrow(as.data.frame(lnk))
	
	if(.parallel == TRUE) {
	
		require(foreach)
		require(multicore)
		n.cores <- multicore:::detectCores() - 1
		message('Using ', n.cores, ' cores')
		require(doMC)
		registerDoMC(cores=n.cores)
	
		if(NR > NM) {
			# Split by rcp
			k <- sort(rep(1:n.cores, length.out=NR))
			jobs <- suppressWarnings(split(rcp, k))
			pred <- foreach(rcp=iter(jobs), .combine=rbind) %dopar% 
				run.CALINE3(lnk, met, rcp, ter, pol)
		} else {
			# Split by pred conditions
			k <- sort(rep(1:n.cores, length.out=NM))
			jobs <- suppressWarnings(split(met, k))
			pred <- foreach(met=iter(jobs), .combine=cbind) %dopar% 
				run.CALINE3(lnk, met, rcp, ter, pol)
		}
		
	} else {
		pred <- run.CALINE3(lnk, met, rcp, ter, pol)
	}	

	attr(pred, "model") <- object
	return(pred)
}

#' Visualization
#'
#' Plot model elements using ggplot2.
#'
#' @keywords ggplot ggplot2
#' @importFrom ggplot2 ggplot
#' @S3method ggplot Caline3Model
#' @rdname Caline3Model-methods
#' @export
ggplot.Caline3Model <- function(data, ...) {
	receptor.data <- as.data.frame(data$receptors)
	link.data <- as.data.frame(data$links)
	receptor.layer <- geom_point(aes(x=x, y=y), pch=3, alpha=0.5, data=receptor.data)
	link.layer <- geom_segment(aes(x=XL1, y=YL1, xend=XL2, yend=YL2), data=link.data)
	base.layer <- ggplot() + coord_equal() + easting() + northing()
	return(base.layer + link.layer + receptor.layer)
}