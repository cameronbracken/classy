#!/usr/bin/env Rscript

nm <- 100
dopdf <- TRUE

for(nmovie in 1:nm){
	
	source('multi_vortex.R')
	if(dopdf)
        file.rename('multi_vortex.pdf',paste('multi_vortex_',sprintf('%03d',nmovie),'.pdf',sep=''))
    else
        file.rename('multi_vortex_oseen_nodiff_polyc_.mp4',
		    paste('multi_vortex_oseen_nodiff_polyc_',sprintf('%03d',nmovie),'.mp4',sep=''))
	
}
