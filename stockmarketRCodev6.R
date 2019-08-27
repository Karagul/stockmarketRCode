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
#library(data.table)
library(mondate);
require(stringi)
require(reshape2) #did you install this package?

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

#eodwNA_xts <- as.xts(eodwNA)
colnames(eodwNA)

#symbolNames <- c()
#symbolNames <- unique(eodwNA$symbol)

#fwrite(eodwNA,"eodwNA.csv")

#https://rdrr.io/github/jeanmarcgp/xtsanalytics/man/mget_symbols.html

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

#https://stackoverflow.com/questions/35371004/error-using-dcast-with-multiple-value-var?noredirect=1&lq=1
#"date"   "open"   "high"   "low"    "close"  "volume"


head(eod_completewNA[1:5])

list_symbols <- unique(eod_completewNA$symbol)

for(lister in list_symbols)
{
  tempHolder <- c()
  tempHolder <- eod_completewNA[which(eod_completewNA$symbol==lister),,drop=F][,-2]
  colnames(tempHolder)
  class(tempHolder$date)
  #https://www.oipapio.com/question-12219593
  rownames(tempHolder) <- tempHolder$date 
  xts_holder <- as.xts(tempHolder,date_col = date)
  colnames(xts_holder)
  colnames(tempHolder)
  class(tempHolder)

  bbands <- BBands( tempHolder[,c("high","low","close")] )
  adx <- ADX( tempHolder[,c("high","low","close")] )
  ema <- EMA(tempHolder[,"close"], n=20)
  sma <- SMA(tempHolder[,"close"], n=20)
  
  # MACD
  macd <- MACD( tempHolder[,"close"] )
  
  # RSI
  rsi <- RSI(tempHolder[,"close"])
  
  # Stochastics
  stochOsc <- stoch(tempHolder[,c("high","low","close")])
  
}

#melted_eod_completewNA <- melt(eod_completewNA, measure.vars = c("open","high","low","close","volume"))

#https://seananderson.ca/2013/10/19/reshape/
#https://t1.daumcdn.net/cfile/tistory/25177F4E5863D58A0C
melted_eod_completewNA <- melt(eod_completewNA, measure.vars = c("open","high","low","close","volume"))

melted_eod_completewNA[which(melted_eod_completewNA$symbol=="ACAD" & melted_eod_completewNA$date == "2015-08-24"),,drop=F]

rownames(melted_eod_completewNA) <- melted_eod_completewNA$date

colnames(melted_eod_completewNA)

#how to filter
melted_eod_completewNA[which(melted_eod_completewNA$symbol=="ACAD" & melted_eod_completewNA$date == "2015-08-24" & (melted_eod_completewNA$variable=="high" | melted_eod_completewNA$variable=="low" | melted_eod_completewNA$variable=="close")),,drop=F]

#https://stackoverflow.com/questions/25143428/why-cant-one-have-several-value-var-in-dcast
eod_pvtwNA<-dcast(melted_eod_completewNA, date ~ symbol + variable, value.var="value" ,mean)

colnames(eod_pvtwNA)
#https://stackoverflow.com/questions/4297231/converting-a-data-frame-to-xts
colnames(eod_completewNA[,-1:-2])

xts_melted_eod_completewNA <- as.xts(eod_completewNA[,-1:-2],date_col = eod_completewNA[,1])

head(xts_melted_eod_completewNA)

colnames(eod_completewNA)
melted_eod_completewNA_bbands <- BBands( eod_completewNA[,c("high","low","close")] )

colnames(melted_eod_completewNA_bbands)
nrow(eod_completewNA)
head(melted_eod_completewNA_bbands)

#value.var (value) not found in input

colnames(eod_completewNA)

colnames(eod_pvtwNA)



#head(eod_pvtwNA)

testWide <- dcast(eod_completewNA, formula = date ~ symbol, value.var = c('open','high','low','close','volume'))

pct2 <- 1-(sapply(eod_pvtwNA, function(x) sum(is.na(x)))/nrow(eod_pvtwNA))

View(colnames(eod_pvtwNA))