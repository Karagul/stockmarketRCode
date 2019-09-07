#technical Indicators
library(TTR)

tech_ind <- function(tempHolder){

  colnames(tempHolder) <- c("Date","Open","High","Low","Close","Volume")
  #
  
  #https://www.oipapio.com/question-12219593
  #rownames(tempHolder) <- tempHolder$date 
  
  bbands <- c()
  adx <- c()
  ema <- c()
  sma <- c()
  macd <- c()
  rsi <- c()
  stochOsc <- c()
  
  #ARK breaks this with too many values at the same value
  
  bbands <- BBands( tempHolder[,c("High","Low","Close")] )
  adx <- ADX( tempHolder[,c("High","Low","Close")] )
  ema <- data.frame(EMA(tempHolder[,"Close"], n=20)[,drop=FALSE])
  sma <- data.frame(SMA(tempHolder[,"Close"], n=20)[,drop=FALSE])
  
  # MACD
  macd <- MACD( tempHolder[,"Close"] )
  
  # RSI
  rsi <- RSI(tempHolder[,"Close"])
  
  # Stochastics
  stochOsc <- stoch(tempHolder[,c("High","Low","Close")])
  
  #View(tempHolder$Date)
  rownames(tempHolder) <- tempHolder$Date
  
  eod <<- tempHolder[which(tempHolder$Date=="2016-10-14"),,drop=F]
  xts_holder <- as.xts(tempHolder)
  
  nrow(tempHolder)
  
  tech_ind <- cbind(lister,tempHolder,bbands,adx,ema,sma,macd,rsi,stochOsc)
  
  return(tech_ind)
}

