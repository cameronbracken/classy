colorRamps <- function(){
		# color ramp functions 
	cr <<- colorRampPalette( 
		c(gray(.9), rgb(.7,.7,.7), rgb(0,0,1), rep(rgb(0,1,0),2)) )
	crw <<- colorRampPalette( 
		c('white', gray(.9), rgb(.7,.7,.7), rgb(0,0,1), rep(rgb(0,1,0),2)) )
}

makePdf2SwfMovie <- function(name){
	silence <- 
		system(paste('pdf2swf -l -B alternate_simple_viewer.swf ',
			name,'.pdf',sep=''),intern=T)
	silence <- 
		system(paste('swfcombine --dummy -r 7 ',name,'.swf -o ',
			name,'.swf',sep=''))
}