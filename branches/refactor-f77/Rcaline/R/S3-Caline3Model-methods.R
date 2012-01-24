#' predict.Caline3Model
#'
#' @param object a \code{\link{Caline3Model}} object
#' @param units one of 'ppm', 'ug/m3', or 'mg/m3'
#'
#' @return matrix of predicted values at each receptor under each meteorological condition
#'
#' @keywords predict model
#' @S3method predict Caline3Model
#' @importFrom stats predict
#' @export
predict.Caline3Model <- function(object, units='ppm') {
	
	require(CALINE3)
	
	stopifnot(inherits(object, 'Caline3Model'))
	model <- object
	rcp <- receptors(model)
	lnk <- links(model)
	met <- meteorology(model)
	ter <- terrain(model)
	pol <- pollutant(model)
	params <- list(
		ATIM = as.single(60.0),
		Z0 = as.single(ter$surfaceRoughness),
		VS = as.single(pol$settlingVelocity),
		VD = as.single(pol$depositionVelocity)
	)
		
	# Initialize the full matrix
	NR <- nrow(as.data.frame(rcp))
	NL <- nrow(as.data.frame(lnk))
	NM <- nrow(as.data.frame(met))
    pred <- matrix(NA, nrow=NR, ncol=NM, dimnames=list(rownames(rcp), rownames(met)))	

	# Compute only the conditions (columns) for which wind speed >= 1.0
	non.calm <- with(met, which(windSpeed >= 1.0))
	for(i in 1:length(non.calm)) {
		hour <- non.calm[i]
		args <- c(as.Fortran(rcp), as.Fortran(lnk), as.Fortran(met[hour,]), params) 
		contributions <- do.call("CALINE3.array", args)
		pred[,hour] <- rowSums(contributions)
	}

	# Convert values to user-specified units
    if(units == 'ppm') {
		pred <- pred * 0.0245 / pol$molecularWeight
	} else if (units == 'mg/m3') {
	      pred <- pred * 1.0e3
	} else {
	      # Default is ug/m3
	}

    # Tag the result as an 'HourlyConcentrations' object,
	# with the originating model, and with the user-specified units
	class(pred) <- c('HourlyConcentrations', 'matrix')
	attr(pred, 'model') <- model
	attr(pred, 'units') <- units

	return(pred)
}

#' Visualization
#'
#' Plot model elements using ggplot2.
#'
#' @param data a Caline3Model object
#'
#' @keywords ggplot ggplot2
#' @importFrom ggplot2 ggplot
#' @S3method ggplot Caline3Model
#' @export
ggplot.Caline3Model <- function(data, ...) {
	receptor.data <- as.data.frame(data$receptors)
	link.data <- as.data.frame(data$links)
	receptor.layer <- geom_point(aes(x=x, y=y), pch=3, alpha=0.5, data=receptor.data)
	link.layer <- geom_segment(aes(x=XL1, y=YL1, xend=XL2, yend=YL2), data=link.data)
	base.layer <- ggplot() + coord_equal() + easting() + northing()
	return(base.layer + link.layer + receptor.layer)
}