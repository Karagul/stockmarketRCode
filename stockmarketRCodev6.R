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
library(timetk);
library(xtsanalytics)


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

qry1=paste0("SELECT symbol,date,open,high,low,adj_close AS close,volume  FROM eod_indices WHERE date BETWEEN '", start_date, "' AND '",end_date,"'")
qry2=paste0("SELECT symbol,timestamp AS date,open,high,low,close,volume FROM mv_qs_facts WHERE timestamp BETWEEN '", start_date, "' AND '",end_date,"'")
queryAggregate=paste(qry1,'UNION',qry2)
eodwNA<-dbGetQuery(conn,queryAggregate)

dbDisconnect(conn)

#eodwNA_xts <- as.xts(eodwNA)
colnames(eodwNA)

#https://rdrr.io/github/jeanmarcgp/xtsanalytics/man/mget_symbols.html


eodwNA_xts <- timetk::tk_xts(eodwNA, date_col = date)
colnames(eodwNA_xts)

bbands <- BBands( [eodwNA_xts,c("High","Low","Close")] )
