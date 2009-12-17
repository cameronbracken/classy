
if(!file.exists('output/flows.Rdata'))source('readData.R')
load('output/flows.Rdata')
load('data/pcs.Rdata')
source('lib.R')

sst <- sspectrum(pac$pc[1,])
pdsi <- sspectrum(usa$pc[1,])

sst.plot <- ggplot(sst,aes(x=freq,y=spec)) + 
geom_ribbon(aes(ymin = lcl, ymax = ucl),fill='#AAAAAA99') + 
geom_line(color='dodgerblue') + 
geom_line(aes(y=sig)) +
theme_bw() +
xlab("Freq. [cy/yr]") + ylab("Spectrum")

pdsi.plot <- ggplot(pdsi,aes(x=freq,y=spec)) + 
geom_ribbon(aes(ymin = lcl, ymax = ucl),fill='#AAAAAA99') + 
geom_line(color='dodgerblue') + 
geom_line(aes(y=sig)) +
theme_bw() +
xlab("Freq. [cy/yr]") + ylab("Spectrum")

save(sst,pdsi,sst.plot,pdsi.plot, file='output/4.Rdata')
