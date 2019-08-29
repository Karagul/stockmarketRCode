psqluser="postgres"
psqlpassword="Read1234"
psqlport=5432
psqldbname="readyloop"
psqlhost="192.168.3.103"

library(Rserve);
library(mondate);
require(Rserve);
library(TTR);
library(quantmod);
#https://stackoverflow.com/questions/4297231/converting-a-data-frame-to-xts
#tidyquant deprecated
#library(timetk);
#source("xtsanalytics-master/R/mget_symbols.R")
#requires sprint which doesn't install easily
#library(sprint)
#library(data.table)
library(mondate);
require(stringi)
require(reshape2) #did you install this package?
library(plyr)

#todayIs <- as.Date(as.POSIXlt(as.Date(Sys.Date())))
#redundant, but included for usefulness
#end_date <-as.Date(mondate(as.Date(todayIs)) - iterator)

days=252/4
weeks=52/4
months=12/4

#grab a year earlier just in case I haven't reran it in a while (hence x12 vs x8)

require(RPostgreSQL) # did you install this package?
require(DBI)
pg = dbDriver("PostgreSQL")
conn = dbConnect(drv=pg
                 ,user="postgres"
                 ,password="Read1234"
                 ,host="192.168.3.103"
                 ,port=5432
                 ,dbname="readyloop"
)


#max date is already stored on db creation
end_date = dbGetQuery(conn,"select max(max) from qs_max_date")
#2 years = 365*2 = 730
#4 years = 1461 (+1 day for leap year)

#have to reference $max else it returns a data.frame of a unix timetsamp vs a dereferenced string date
end_date = end_date$max
start_date = (end_date - 1461)

dbDisconnect(conn)

print(paste("End Date: ",end_date))

conn = dbConnect(drv=pg, user=psqluser, password=psqlpassword, host=psqlhost, port=psqlport, dbname=psqldbname)

#OHLCV and indices
qry=paste0("SELECT * FROM custom_calendar WHERE date BETWEEN '", start_date, "' AND '",end_date,"' ORDER by date")
ccal<-dbGetQuery(conn,qry)

qry1=paste0("SELECT date,symbol,open,high,low,adj_close AS close,volume  FROM eod_indices WHERE date BETWEEN '", start_date, "' AND '",end_date,"'")
qry2=paste0("SELECT timestamp AS date,symbol,open,high,low,close,volume FROM mv_qs_facts WHERE timestamp BETWEEN '", start_date, "' AND '",end_date,"'")
queryAggregate=paste(qry1,'UNION',qry2)
eodwNA<-dbGetQuery(conn,queryAggregate)

dbDisconnect(conn)

colnames(eodwNA)

eodOutside<-na.omit(eodwNA)

start_dateInLoop = mondate(start_date)
end_dateInLoop = mondate(start_dateInLoop) + 24

eod <<- eodOutside[which(eodOutside$date>=as.Date(start_dateInLoop) & eodOutside$date <= as.Date(end_dateInLoop)),,drop=F]

tdays<-ccal[which(ccal$trading==1 & ccal$date >= as.Date(start_dateInLoop) & ccal$date <=as.Date(end_dateInLoop)),,drop=F]
#short sighted on holidays
wdays<-tdays[which(tdays$dow=="Fri"),,drop=F]
mdays<-tdays[which(tdays$eom==1),,drop=F]

pct<-table(eod$symbol)/(nrow(tdays))

#filter out symbols that were bound together (example shp and gst)
selected_symbols_daily<-names(pct)[which(pct>=0.99 & pct<=1)]

eod_completewNA<-eod[which(eod$symbol %in% selected_symbols_daily),,drop=F]

#melted_eod_completewNA <- melt(eod_completewNA, measure.vars = c("open","high","low","close","volume"))

#https://seananderson.ca/2013/10/19/reshape/
#https://t1.daumcdn.net/cfile/tistory/25177F4E5863D58A0C
#melted_eod_completewNA <- melt(eod_completewNA, measure.vars = c("open","high","low","close","volume"))
melted_eod_completewNA_close <- melt(eod_completewNA, measure.vars = c("close"))
#melted_eod_completewNA[which(melted_eod_completewNA$symbol=="ACAD" & melted_eod_completewNA$date == "2015-08-24"),,drop=F]

#how to filter
melted_eod_completewNA[which(melted_eod_completewNA$symbol=="ACAD" & melted_eod_completewNA$date == "2015-08-24" & (melted_eod_completewNA$variable=="high" | melted_eod_completewNA$variable=="low" | melted_eod_completewNA$variable=="close")),,drop=F]

#https://stackoverflow.com/questions/25143428/why-cant-one-have-several-value-var-in-dcast
#eod_pvtwNA<-dcast(melted_eod_completewNA, date ~ symbol + variable, value.var="value" ,mean)

#eod_pvtwNA_close<-dcast(melted_eod_completewNA_close, date ~ symbol + variable, value.var="value" ,mean)
eod_pvtwNA_close<-dcast(melted_eod_completewNA_close, date ~ symbol, value.var="value" ,mean)

#some symbols had all 1's, so I need to drop those columns
#https://stackoverflow.com/questions/22196078/count-unique-values-for-every-column
#pct_uniques <- apply(eod_pvtwNA_close, 2, function(x) length(unique(x)))

#https://masterr.org/r/how-to-find-consecutive-repeats-in-r/
#https://stackoverflow.com/questions/19998836/r-count-consecutive-occurrences-of-values-in-a-single-column
repeats <- c()
repeats <- apply(eod_pvtwNA_close, 2, function(x) max(rle(x)$lengths))

length(names(repeats))
#90% repeats was 5.5
repeated_symbols_daily<-names(repeats)[which(repeats>=8)]

eod_completewNA_woutRepeats<-eod_completewNA[which(!eod_completewNA$symbol %in% repeated_symbols_daily),,drop=F]

#pct2 <- 1-(sapply(eod_pvtwNA, function(x) sum(is.na(x)))/nrow(eod_pvtwNA))

list_symbols <- unique(eod_completewNA_woutRepeats$symbol)

#x <- mget_symbols(list_symbols, startdate=start_date,src="database",filepath="eodwNA.csv")


#colnames(eod_completewNA_woutRepeats)

eod_completewNA_techInd <- c()
for(lister in list_symbols)
{
  tempHolder <- c()
  tempHolder <- eod_completewNA_woutRepeats[which(eod_completewNA_woutRepeats$symbol==lister),,drop=F][,-2]
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
  
  View(tempHolder$Date)
  rownames(tempHolder) <- tempHolder$Date
  
  eod <<- tempHolder[which(tempHolder$Date=="2016-10-14"),,drop=F]
  xts_holder <- as.xts(tempHolder)
  
  nrow(tempHolder)
  

  tech_ind <- cbind(lister,tempHolder,bbands,adx,ema,sma,macd,rsi,stochOsc)
  
  colnames(tech_ind)[1] <- "Symbol"
  
  if(is.null(eod_completewNA_techInd))
  {
    eod_completewNA_techInd <- tech_ind
  }
  if(!is.null(eod_completewNA_techInd))
  {
    colnames(tech_ind)
    eod_completewNA_techInd <- rbind(eod_completewNA_techInd,tech_ind)
    
  }
  #print(nrow(eod_completewNA_techInd))
}
View(eod_completewNA_woutRepeats$symbol)
View(unique(eod_completewNA_techInd$Symbol))



View(colnames(eod_pvtwNA))