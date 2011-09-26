ReceptorGrid <- function(links, height=1.8, resolution=100.0, maxDistance=1000.0) {
      require(rgeos)
      buf <- suppressWarnings(rgeos::gBuffer(links$polylines, width=maxDistance))
      xy <- spsample(buf, cellsize = c(resolution, resolution), type = "regular")
      coordnames(xy) <- c("x", "y")
      z <- data.frame(z = rep(1.8, length(xy)))
      spdf <- SpatialPointsDataFrame(xy, data=z)
      return(spdf)
}