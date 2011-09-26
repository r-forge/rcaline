Pollutant <- function(name, molecularWeight, settlingVelocity=0.0, depositionVelocity=0.0) {
      obj <- list(
            name = name,
            molecularWeight = molecularWeight,
            settlingVelocity = settlingVelocity,
            depositionVelocity = depositionVelocity
      )
      class(obj) <- "Pollutant"
      return(obj)
}