library(stringr)
library(plyr)
library(reshape)
library(vegan)

setwd("../..")
sapply(list.files("r"), function(file)source(paste("r/", file, sep="")))
metadata <- loadMetaData()
load("data/bugdata.rdata")
