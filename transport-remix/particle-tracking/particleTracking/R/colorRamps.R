colorRamps <- function(){
		# color ramp functions 
	cr <<- colorRampPalette( 
		c(gray(.9), rgb(.7,.7,.7), rgb(0,0,1), rep(rgb(0,1,0),2)) )
	crw <<- colorRampPalette( 
		c('white', gray(.9), rgb(.7,.7,.7), rgb(0,0,1), rep(rgb(0,1,0),2)) )
}