#-------------------------------------------------------------
#   title: "MMC Stock Analysis"                              -
# author: "Salomon Gilles"                                   -
# date: "July 21, 2016"                                      -
#-------------------------------------------------------------

## @knitr mmcFunctions
library(tseries)
MMCData <- function(paramUrl, paramStock, paramStart, paramEnd)
{
  #stockdata
  conUrl <- url(paramUrl)
  
  if(!inherits(try(open(conUrl), silent = TRUE), "try-error")) {
    
    stockdata <- get.hist.quote(instrument = paramStock, start = paramStart, end = paramEnd, quote = "Close", quiet=TRUE)
    
    close(conUrl)
  }
  
  stockdata
}

LogStockData <- function(paramdata)
{
  logstockdata <- log(lag(paramdata)) - log(paramdata)  
}


StockVolatility <- function(paramweighting, paramLogdata)
{
  var = 0
  lam = 0
  varlist <- c()
  
  for (logdata in paramLogdata) {
    lam = lam * (1 - 1/paramweighting) + 1
    var = (1 - 1/lam) * var + (1/lam) * logdata^2
    varlist <- c(varlist, var)
  }
  
  sqrt(varlist)
}


PlotOriginaldata <- function(paramOriginaldata)
{
  plot(paramOriginaldata, type="l", col = "black", xlab = "time series", ylab = "price", main = "MMC - Raw Data")    
}


PlotLogdata <- function(paramLogdata)
{
  plot(paramLogdata, type="l", col = "red", xlab = "time series", ylab = "price", main = "MMC - log transformed Data")       
}