rm(list = ls())
library(quantmod)
library(forecast)
library(xts)
  
NoDay <- 365*5

getQuote("SPY", what=yahooQF("Last Trade (Price Only)"))

AudUsd.temp <- getFX("AUD/USD", src='oanda', from=Sys.Date()-NoDay, to = Sys.Date(), auto.assign = FALSE)
CnyUsd.temp <- getFX("CNY/USD", src='oanda', from=Sys.Date()-NoDay, to = Sys.Date(), auto.assign = FALSE)

AudUsd <- data.frame(Dates = time(AudUsd.temp), Values = as.numeric(AudUsd.temp))  
CnyUsd <- data.frame(Dates = time(CnyUsd.temp), Values = as.numeric(CnyUsd.temp))   
  


##
plot()

##
days <- as.factor(weekdays(AudUsd$Dates))
xreg1 <- model.matrix(~days)[, 2:5]  # Exogenous variable
colnames(xreg1) <- c("ExV1","ExV2","ExV3","ExV4")

fit1 <- auto.arima(AudUsd$Values)
plot(forecast(fit1, h=14))

fit2 <- arima(AudUsd$Values, order = c(2, 1, 2), xreg = xreg1[c(1:(length(AudUsd$Values))),])
plot(forecast(fit2, h=14, xreg = xreg1[c((length(AudUsd$Values)-6-7) : length(AudUsd$Values)),]))






#######################
require(jsonlite)
require(httr)

url.name = 'http://query.yahooapis.com/v1/public/yql?q=select * from geo.places where text="sunnyvale, ca"'
  #"http://query.yahooapis.com/v1/public/yql?q=select * from yahoo.finance.xchange where pair in ("USDEUR", "USDJPY", "USDBGN", "USDCZK", "USDDKK", "USDGBP", "USDHUF", "USDLTL", "USDLVL", "USDPLN", "USDRON", "USDSEK", "USDCHF", "USDNOK", "USDHRK", "USDRUB", "USDTRY", "USDAUD", "USDBRL", "USDCAD", "USDCNY", "USDHKD", "USDIDR", "USDILS", "USDINR", "USDKRW", "USDMXN", "USDMYR", "USDNZD", "USDPHP", "USDSGD", "USDTHB", "USDZAR", "USDISK")&env=store://datatables.org/alltableswithkeys"
url.get = GET(url.name)

################
library(TFX)
sess <- ConnectTrueFX("AUD/USD", username = "master275", password = "gxx198831")
isActive(sess)
His <- QueryTrueFX(sess, pretty = TRUE, reconnect = TRUE )

for (i in 1:600){
  His <- rbind(His, QueryTrueFX(sess, pretty = TRUE, reconnect = TRUE ))
  Sys.sleep(1)
  plot(tail(His$Bid.Price,n=50), type = "l", main = date())
}

