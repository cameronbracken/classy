######################################
# Packages
######################################


######################################
# Data
######################################
if(!file.exists('data/ts.Rdata')) source('read-and-save.R')
load('data/ts.Rdata')

######################################
# Funtions
######################################
source('lib.R')