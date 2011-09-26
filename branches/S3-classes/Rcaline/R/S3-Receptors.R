ReceptorGrid <- function(links, height=1.8, resolution=100.0, maxDistance=1000.0) {
      require(rgeos)
      buffer <- suppressWarnings(gBuffer(links$polylines, width=maxDistance))
      locations <- spsample(buffer, cellsize = c(500, 500), type = "regular")
      coordnames(locations) <- c("x", "y")
      attributes <- data.frame(z = rep(1.8, length(locations)))
      spdf <- SpatialPointsDataFrame(locations, data=attributes)
      return(spdf)
}