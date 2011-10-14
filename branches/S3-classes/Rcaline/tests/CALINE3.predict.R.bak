library(Rcaline)
data(CALINE3.examples)
lapply(
	list(ExampleOne, ExampleTwo, ExampleThree, ExampleFour), 
	function(envir)	{
		with(envir, background.concentration + CALINE3.predict(receptors, links, meteorology, surface.roughness=surface.roughness)
		)
	}
)
