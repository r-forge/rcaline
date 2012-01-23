#' San Francisco data
#'
#' @name SanFrancisco
#' @docType data
#' @author David Holstius \email{david.holstius@@berkeley.edu}
#' @references Data provided by the Bay Area Air Quality Management District (BAAQMD)
#' @keywords data datasets

.read.layer <- function(file, ...) {
    suppressPackageStartupMessages(require(rgdal))
    dsn <- normalizePath(dirname(file))
    layer <- gsub(".shp$", "", basename(file))
    message('Reading ', layer, ' from ', dsn)
    stopifnot(file.exists(dsn))
    spobj <- rgdal::readOGR(dsn, layer, ...)
    return(spobj)
}

# Read shapefiles from extdata directory.
.extdata <- file.path(getwd(), '..', 'inst', 'extdata')
STRte_BAAQMD_v3 <- .read.layer(file.path(.extdata, 'BayArea', 'Roadways', 'STRte_BAAQMD_v3'))
SF_county <- .read.layer(file.path(.extdata, 'BayArea', 'Counties', 'SF_county.shp'))

# Transform geometry to the same coordinate system.
.datum <- CRS("+proj=utm +zone=11 +datum=WGS84")
STRte_BAAQMD_v3 <- spTransform(STRte_BAAQMD_v3, .datum)
SF_county <- spTransform(SF_county, .datum)

# Select only those roadways that intersect the region of interest
# Region of interest: SF county + 2000 m buffer
suppressPackageStartupMessages(require(rgeos))
.region <- gBuffer(SF_county, width=2000)
.selection <- as.logical(gIntersects(.region, STRte_BAAQMD_v3, byid=TRUE))
STRte_BAAQMD_v3 <- STRte_BAAQMD_v3[.selection,]

# Bit of a hack
source(file.path('..','R','S3-Meteorology.R'))
met_5801 <- ISCFile(file.path(.extdata, 'BayArea', 'Meteorology', 'met_5801.isc'))