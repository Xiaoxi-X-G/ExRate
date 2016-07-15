rm(list = ls())
library(Quandl)					# Quandl package
library(reshape2)				# Package for reshaping data
library(ggplot2)

# Build vector of currencies
curr <- c("AUD","CNY")

StartDate <- "2008-01-01"

AUD_RateHighLow <- Quandl(paste("CURRFX/",curr[1],"USD",sep="") 
                          ,start_date= StartDate,end_date= as.character(Sys.Date()))
CNY_RateHighLow <- Quandl(paste("CURRFX/",curr[2],"USD",sep="") 
                          ,start_date= StartDate,end_date= as.character(Sys.Date()))

coeff <- mean(AUD_RateHighLow$Rate)/mean(CNY_RateHighLow$Rate)

HistoryAUDCNY <- data.frame(Date = AUD_RateHighLow$Date, 
                            AUD = AUD_RateHighLow$Rate,
                            CNY = coeff*CNY_RateHighLow$Rate)
HistoryAUDCNY <- HistoryAUDCNY[order(HistoryAUDCNY$Date), ]

HistoryAUDCNY.melt <- melt(HistoryAUDCNY, id="Date")
ggplot(HistoryAUDCNY.melt, aes(x = Date, y = value, color = variable, linetype = variable)) + 
  geom_line(lwd = 0.5) + geom_point(size = 0.5)


##
x <- as.character(HistoryAUDCNY$Date)
plot(HistoryAUDCNY$AUD,type="l",col="red",ylab="", xaxt="n")
axis(1, at=1:length(x), labels=x)
par(new = T)
plot(HistoryAUDCNY$CNY,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("CNY",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("AUD","CNY"))