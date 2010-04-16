#!/usr/bin/env Rscript

nm <- 100

for(nmovie in 1:nm){
	
	source('multi_vortex.R')
	file.rename('multi_vortex_oseen_nodiff_polyc_.mp4',
		paste('multi_vortex_oseen_nodiff_polyc_',sprintf('%03d',nmovie),'.mp4',sep=''))
	
}