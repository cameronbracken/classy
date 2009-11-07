source('lib.R')

sst <- as.matrix(read.table('data/kaplan-sst-wy-1925-2003-revised.txt'))
sst <- t(matrix(sst[,3],nrow=length(unique(sst[,1])),byrow=T))
sst.lat <- as.matrix(read.table('data/kaplan-sst-wy-1925-2003-II.txt'))
sst.lon <- sst.lat[,2]; sst.lat <- sst.lat[,1]
sst.lon[sst.lon < 0] <- sst.lon[sst.lon < 0] + 360

pdsi <- as.matrix(read.table('data/pdsi-wy-1925-2003.txt'))
pdsi <- t(matrix(pdsi[,3],nrow=length(unique(pdsi[,1])),byrow=T))
pdsi.lat <- as.matrix(read.table('data/pdsi-wy-1925-2003-II.txt'))
pdsi.lon <- pdsi.lat[,2]; pdsi.lat <- pdsi.lat[,1]
pdsi.lon[pdsi.lon < 0] <- pdsi.lon[pdsi.lon < 0] + 360

pacific <- sst.lat > -20 & sst.lon >= 120 & sst.lon <= 280
atlantic <- sst.lat > -20 & sst.lat < 70 & sst.lon >= 250 & sst.lon <= 360
states <- pdsi.lat > 15 & pdsi.lat < 60 & pdsi.lon >= 230 & pdsi.lon <= 295

lon.pac <- sst.lon[pacific]
lat.pac <- sst.lat[pacific]
sst.pac <- sst[,pacific]
lon.atl <- sst.lon[atlantic]
lat.atl <- sst.lat[atlantic]
sst.atl <- sst[,atlantic]
lon.usa <- pdsi.lon[states]
lat.usa <- pdsi.lat[states]
sst.usa <- pdsi[,states]

pac <- my.pca(sst.pac)
atl <- my.pca(sst.atl)
usa <- my.pca(sst.usa)

save(lat, lon, lat.pac, lon.pac, lat.atl, lon.atl, lat.usa, lon.usa, pac, atl,
	usa, file='output/1.Rdata')
