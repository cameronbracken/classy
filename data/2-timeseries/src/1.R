	#read support functions
source('lib.R')

	#read data (ac-ft)
x <- read.table('data/Leesferry-mon-data.txt')
	# creat timeseries and convert to cms
x <- ts(as.vector(t(x[,-1])),frequency=12)*0.000469050

