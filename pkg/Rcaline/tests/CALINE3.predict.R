library(Rcaline)
data(CALINE3.examples)
lapply(
	list(ExampleOne, ExampleTwo, ExampleThree, ExampleFour), 
	function(envir)	{
		with(envir, CALINE3.predict(
			receptors, links, meteorology,
			averaging.time,	surface.roughness,
			settling.velocity, deposition.velocity,
			background.concentration)
		)
	}
)
