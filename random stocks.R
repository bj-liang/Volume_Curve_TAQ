closeAllConnections()
rm(list=ls())

library(data.table)
setwd("C:\\Users\\benny\\OneDrive\\Desktop\\github\\Volume_Curve_TAQ")

C <- fread("CRSP.gz", header=TRUE,
           colClasses=c(date='character',
                        PRC='numeric',SHROUT='numeric'))
C$date <- as.Date(as.character(C$date),format='%Y%m%d') 

C$PRC[ C$PRC <= 0 ] <- as.numeric(NA)
C$SHROUT <- 1000 * C$SHROUT #shares outstanding

C$mktcap = C$PRC * C$SHROUT

orderedMktcap = C[order(-mktcap)]

set.seed(2020)

randomCompanies=sample(1:1000,10)
orderedMktcap[randomCompanies,]$TICKER