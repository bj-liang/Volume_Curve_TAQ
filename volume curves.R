closeAllConnections()
rm(list=ls())
library(data.table)
setwd("C:\\Users\\benny\\OneDrive\\Desktop\\github\\Volume_Curve_TAQ")

#The data downloaded only includes the ten stocks previously selected in May 2019
C <- fread("TAQ.gz", header=TRUE,
           colClasses=c(DATE='character',
                        PRICE='numeric',SIZE='numeric'))
C$DATE <- as.Date(as.character(C$DATE),format='%Y%m%d') 
copy = C #Just a way for me to check data

library(chron)
C$TIME_M <- chron(times=C$TIME_M)

C <- C[C$TIME_M - chron(times='09:30:00') >= 0 & C$TIME_M - chron(times='16:00:00') <= 0] # filter only trading hours
C <- C[order(TIME_M)] # order the quotes by time

C$SYM_ROOT <- factor(C$SYM_ROOT) #not sure how to use for loops yet

ETR = C[SYM_ROOT == "ETR"]
BBDO = C[SYM_ROOT == "BBDO"]
CPB = C[SYM_ROOT == "CPB"]
BR = C[SYM_ROOT == "BR"]
IRM = C[SYM_ROOT == "IRM"]
HAL = C[SYM_ROOT == "HAL"]
OKE = C[SYM_ROOT == "OKE"]
KMB = C[SYM_ROOT == "KMB"]
HCA = C[SYM_ROOT == "HCA"]
MTN = C[SYM_ROOT == "MTN"]


sum_bins <- function(binNumb, df) {
  fifteenBin <- seq(from=chron(times='09:30:00'), to=chron(times='16:00:00'), by=chron(times='00:15:00'))
  t <- fifteenBin[binNumb]
  minutes <- df[df$TIME >= t & df$TIME < t+chron(times='00:15:00')]
  sum(minutes$SIZE) #multiply by 100... but doesn't matter for plot purposes
}

averageConsolidatedCurve = function(stockSYM) {
  dirtyOneDays <- as.Date(seq(from=chron(dates='2019-05-01', format = c(dates = "y-m-d")), to=chron(dates='2019-05-31', format = c(dates = "y-m-d"))))
  tradingDays = dirtyOneDays[-c(4,5,11,12,18,19,25,26,27)]
  dailyVolumeCurve = c()
  consolidation = vector(mode="numeric", length=26)
  dailySnapshot = data.frame()
  for (d in tradingDays) {
    dailySnapshot = stockSYM[DATE == d]
    dailyVolumeCurve = c()
    for (i in c(1:26)){
      dailyVolumeCurve = c(dailyVolumeCurve, sum_bins(i, dailySnapshot))
    }
    consolidation = consolidation + dailyVolumeCurve
  }
  tradingDaysinMay = 23
  consolidation/23
}

ACC_ETR = averageConsolidatedCurve(ETR)
ACC_BBDO = averageConsolidatedCurve(BBDO)
ACC_CPB = averageConsolidatedCurve(CPB)
ACC_BR = averageConsolidatedCurve(BR)
ACC_IRM = averageConsolidatedCurve(IRM)
ACC_HAL = averageConsolidatedCurve(HAL)
ACC_OKE = averageConsolidatedCurve(OKE)
ACC_KMB = averageConsolidatedCurve(KMB)
ACC_HCA = averageConsolidatedCurve(HCA)
ACC_MTN = averageConsolidatedCurve(MTN)

GIGANTIC_ACC = (ACC_ETR/sum(ACC_ETR)+ACC_BBDO/sum(ACC_BBDO)+ACC_CPB/sum(ACC_CPB)
                +ACC_BR/sum(ACC_BR)+ACC_IRM/sum(ACC_IRM)+ACC_HAL/sum(ACC_HAL)
                +ACC_OKE/sum(ACC_OKE)+ACC_KMB/sum(ACC_KMB)+ACC_HCA/sum(ACC_HCA)
                +ACC_MTN/sum(ACC_MTN))
GIGANTIC_ACC = GIGANTIC_ACC/sum(GIGANTIC_ACC)
#Need to plot the normalized curve or they make no sense in context with giganticACC
pdf("ETRConsolidatedVolCurve.pdf")
plot(ACC_ETR, type = "b", xlab = "bins", ylab = "Volume Liquidated (ETR)",
     col = "green")
dev.off()
pdf("11VolCurves.pdf")
plot(GIGANTIC_ACC, type = "b", xlab = "bins", ylab = "Volume Liquidated (ETR)",ylim = c(0,0.4),
     col = "black")
lines(ACC_ETR/sum(ACC_ETR), col = "chocolate")
lines(ACC_BBDO/sum(ACC_BBDO), col = "deepskyblue4")
lines(ACC_CPB/sum(ACC_CPB), col = "orange")
lines(ACC_BR/sum(ACC_BR), col = "yellow")
lines(ACC_IRM/sum(ACC_IRM), col = "hotpink")
lines(ACC_HAL/sum(ACC_HAL), col = "olivedrab")
lines(ACC_OKE/sum(ACC_OKE), col = "purple1")
lines(ACC_KMB/sum(ACC_KMB), col = "red1")
lines(ACC_HCA/sum(ACC_HCA), col = "tan1")
lines(ACC_MTN/sum(ACC_MTN), col = "plum1")
#shifted up to better see shape
lines(GIGANTIC_ACC+0.05, col = "black")
dev.off()