#A method for obtaining 2 year seasonal volume forecasts


- m = length of years leading up 
- p = length of years after
- get length m state from historical (store magnitudes)
- find all the length m transitions from paleo that start with historic state
	- possibly from a block of paleo
- get magnitude and state of subsequent p years from paleo
- 
- f(x\_future | x\_past, S\_past)