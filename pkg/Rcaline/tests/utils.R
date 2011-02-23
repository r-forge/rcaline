is.installed <- function(pkg) is.element(pkg, installed.packages()[,1])

if(is.installed('maptools') && is.installed('spatstat')) {
	shpfile <- system.file("extdata", "WestOakland", "highways.shp", package = "Rcaline")
	link.data <- Rcaline:::import.shapefile(shpfile) # private, undocumented function
}
