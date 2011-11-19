#' San Francisco data
#'
#' @name SanFrancisco
#' @docType data
#' @author David Holstius \email{david.holstius@@berkeley.edu}
#' @references BAAQMD
#' @keywords data datasets

.read.layer <- function(file, ...) {
      suppressPackageStartupMessages(require(rgdal))
      dsn <- dirname(file)
      layer <- gsub(".shp$", "", basename(file))
      spobj <- rgdal::readOGR(dsn, layer, ...)
      return(spobj)
}

# Read shapefiles from extdata directory.
.extdata <- file.path(getwd(), '..', 'inst', 'extdata')
STRte_BAAQMD_v2.shp <- .read.layer(file.path(.extdata, 'BayArea', 'Roadways', 'STRte_BAAQMD_v2.shp'))
SF_county.shp <- .read.layer(file.path(.extdata, 'BayArea', 'Counties', 'SF_county.shp'))

# Transform geometry to the same coordinate system.
.datum <- CRS("+proj=utm +zone=11 +datum=WGS84")
STRte_BAAQMD_v2.shp <- spTransform(STRte_BAAQMD_v2.shp, .datum)
SF_county.shp <- spTransform(SF_county.shp, .datum)

# Select only those roadways that intersect the region of interest
# Region of interest: SF county + 2000 m buffer
suppressPackageStartupMessages(require(rgeos))
.region <- gBuffer(SF_county.shp, width=2000)
.selection <- as.logical(gIntersects(.region, STRte_BAAQMD_v2.shp, byid=TRUE))
STRte_BAAQMD_v2.shp <- STRte_BAAQMD_v2.shp[.selection,]

# Bit of a hack
source(file.path('..','R','S3-Meteorology.R'))
met_5801.isc <- ISCFile(file.path(.extdata, 'BayArea', 'Meteorology', 'met_5801.isc'))