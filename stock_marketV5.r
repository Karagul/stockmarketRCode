
#dp<-read.csv('c:/test/share/quantshare/quotes.csv',header = FALSE) # no arguments

psqluser="postgres"
psqlpassword="Read1234"
psqlport=5432
psqldbname="readyloop"
psqlhost="192.168.3.103"

library(Rserve);
library(mondate);
require(Rserve);

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
#4 years = 1461

#have to reference $max else it returns a data.frame of a unix timetsamp vs a dereferenced string date
end_date = end_date$max
start_date = (end_date - 1461)

dbDisconnect(conn)

iterator=0
class(start_date)
#class(end_date)

print(paste("End Date: ",end_date))

conn = dbConnect(drv=pg, user=psqluser, password=psqlpassword, host=psqlhost, port=psqlport, dbname=psqldbname)
#eod prices and indices
qry=paste0("SELECT * FROM custom_calendar WHERE date BETWEEN '", start_date, "' AND '",end_date,"' ORDER by date")
ccal<-dbGetQuery(conn,qry)

qry1=paste0("SELECT symbol,date,adj_close FROM eod_indices WHERE date BETWEEN '", start_date, "' AND '",end_date,"'")
#qry2=paste0("SELECT symbol,timestamp,adjusted_close FROM nasdaq_facts WHERE timestamp BETWEEN '",start_date,"' AND '",end_date,"'")
#qryQSCount=("select symbol from mv_qs_symbols")
qry2=paste0("SELECT symbol,timestamp,adjusted_close FROM etf_bond_facts WHERE timestamp BETWEEN '", start_date, "' AND '",end_date,"'")
qry3=paste0("SELECT symbol,timestamp,adjusted_close FROM nasdaq_facts WHERE timestamp BETWEEN '", start_date, "' AND '",end_date,"'")
qry4=paste0("SELECT symbol,timestamp,adjusted_close FROM other_facts WHERE timestamp BETWEEN '", start_date, "' AND '",end_date,"'")
#qry5=paste0("SELECT symbol,timestamp,close FROM qs_facts WHERE timestamp BETWEEN '", start_date, "' AND '",end_date,"'")
qry5=paste0("SELECT symbol,timestamp,close FROM mv_qs_facts WHERE timestamp BETWEEN '", start_date, "' AND '",end_date,"'")
#eodwNA<-dbGetQuery(conn,paste(qry1,'UNION',qry2,'UNION',qry3,'UNION',qry4))
eodwNA<-dbGetQuery(conn,paste(qry1,'UNION',qry5))
#QSSymbols<-dbGetQuery(conn,paste(qryQSCount))
#QSSymbolCount<-nrow(QSSymbols)

#10% of symbols
#width=round(QSSymbolCount*.1)  
#symbolKeySubset <- symbolKeySubset[sample(, as.numeric(round(QSSymbolCount*.1)))]
#symbolKeySubset <- c(sample(1:(as.numeric(QSSymbolCount)), as.numeric(round(QSSymbolCount*.1)), replace=F))

#symbolKeySubset <- sample_n(QSSymbols, as.numeric(round(QSSymbolCount*.1)))

#https://stackoverflow.com/questions/33634713/rpostgresql-import-dataframe-into-a-table
#dbWriteTable(conn, "symbolKeySubset", symbolKeySubset, row.names=FALSE, append=TRUE)
#system("c:/users/user/documents/alphaadvantageapi/stockmarketr/stockmarketrcode/runSubsetQuery.bat", intern = TRUE)

#require(dplyr)

#qryMVQSSv=paste0("create view v_qs_sample as SELECT mv_qs_facts.symbol, mv_qs_facts.timestamp as date, mv_qs_facts.close as adj_close FROM mv_qs_facts INNER JOIN mv_qs_symbol_subset ON mv_qs_facts.symbol=mv_qs_symbol_subset.symbol;")

#qryMVQSS=paste0("select * from v_qs_sample")

#eodwNA<-dbGetQuery(conn,paste(qryMVQSS,'UNION',qry1))

dbDisconnect(conn)

eodOutside<-na.omit(eodwNA)

#if file exists, will overwrite with file, else error
#eodOutside<-na.omit(dp[which(dp$date>=start_date & dp$date <= end_date),,drop=F])

nrow(eodOutside)

table(eodOutside$symbol)

#View(table(eodOutside$symbol))

#scores<-c()
iterator=0
#for (iterator in seq(1, 24, by=1))
{
  print(paste("iterator", iterator))
}

end_dateInLoop <- 0
start_dateInLoop <- 0


iterator=0
#won't work with 730 days which is 2 years
#3 months, not years
for (iterator in seq(0, 24, by=1))
{
  
  start_dateInLoop = mondate(start_date) + iterator
  end_dateInLoop = mondate(start_dateInLoop) + 24
  
  #typeof(start_dateInLoop)
  #typeof(start_date)
  
  eod <<- eodOutside[which(eodOutside$date>=as.Date(start_dateInLoop) & eodOutside$date <= as.Date(end_dateInLoop)),,drop=F]
  
  #set # of years back here.

  #nrow(eod)
  
  #problem is null records are loaded
  #table(eod$symbol=='AGT')
  
  #https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
  #Delete rows with blank values in one particular column
  #eod[!(is.na(eod$adj_close) | eod$adj_close==""), ]
  
  #https://jangorecki.gitlab.io/data.table/library/data.table/html/na.omit.data.table.html
  #https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame
  
  
  #table(is.na(eod))
  #table(is.na(eod$adj_close))
  #table(is.na(eod_new$adj_close))
  
  #remove.na(eod)
  
  #eod<-dbGetQuery(conn,paste(qry))
  
  #Explore
  #head(ccal)
  #tail(ccal)
  #nrow(ccal)
  
  #head(eod)
  #tail(eod)
  #nrow(eod)
  
  #head(eod[which(eod$symbol=='SP500TR'),])
  
  #tail(eod)
  
  # Use Calendar --------------------------------------------------------
  
  tdays<-ccal[which(ccal$trading==1 & ccal$date >= as.Date(start_dateInLoop) & ccal$date <=as.Date(end_dateInLoop)),,drop=F]
  #short sighted on holidays
  wdays<-tdays[which(tdays$dow=="Fri"),,drop=F]
  mdays<-tdays[which(tdays$eom==1),,drop=F]
  #head(tdays)
  #nrow(tdays)-1
  
  # Completeness ----------------------------------------------------------
  # Percentage of completeness
  
  #table(eod$symbol)
  
  #View(table(eod$symbol))
  
  #hist(table(eod$symbol))
  
  #max(table(eod$symbol))
  #length(table(eod$symbol))
  
  #filter
  #testing<-eod[which(eod$symbol=='YLCO'),,]
  #eom_ret
  #View(testing)
  
  #hist(table(eod$symbol))
  
  pct<-table(eod$symbol)/(nrow(tdays))
  #hist(pct)
  #tail(pct,50)
  #pct<-table(eod$symbol)/max(table(eod$symbol))
  
  #filter out symbols that were bound together (example shp and gst)
  selected_symbols_daily<-names(pct)[which(pct>=0.99 & pct<=1)]
  length(selected_symbols_daily)
  
  eod_completewNA<-eod[which(eod$symbol %in% selected_symbols_daily),,drop=F]
  table(eod_completewNA$symbol)
  max(table(eod_completewNA$symbol))
  
  #temp<-c()
  #temp<-eod_complete$date
  #typeof(temp)
  #eod_complete[which(eod_complete$date=="2017-12-31")]
  
  #=='2017-12-30']
  #data.frame(eod_complete)[date>=as.Date('2017-12-29') & date<=as.Date('2017-12-31')]
  
  #check
  head(eod_completewNA)
  tail(eod_completewNA)
  nrow(eod_completewNA)
  
  #YOUR TURN: perform all these operations for monthly data
  #Create eom and eom_complete
  #Hint: which(ccal$trading==1 & ccal$eom==1)
  
  # Transform (Pivot) -------------------------------------------------------
  #library(reshape2)
  #install.packages("stringi",type="win.binary")
  require(stringi)
  require(reshape2) #did you install this package?
  eod_pvtwNA<-dcast(eod_completewNA, date ~ symbol,value.var='adj_close',fun.aggregate = mean, fill=NULL)
  
  #eod_pvtwNA_allDays<-dcast(eod, date ~ symbol,value.var='adj_close',fun.aggregate = mean, fill=NULL)
  #View(eod_pvtwNA_allDays)
  
  #eod_pvtwNA_allDays
  
  #check
  #View((eod_pvt[c('date','SP500TR')])) #first 10 rows and first 5 columns 
  #View((eod_ret[c('SP500TR')]))
  #ncol(eod_pvtwNA) # column count
  #nrow(eod_pvtwNA)
  
  #still a problem, 451/809
  #table(is.na(eod_pvt$YLCO))
  
  #https://sebastiansauer.github.io/sum-isna/
  pct2 <- 1-(sapply(eod_pvtwNA, function(x) sum(is.na(x)))/nrow(eod_pvtwNA))
  
  #length(eod_complete)
  length(table(eod$symbol))
  length(pct)
  length(eod_pvtwNA)
  length(pct2)
  #tail(pct,50)
  
  selected_symbols_daily2<-names(pct2)[which(pct2>=0.99)]
  length(selected_symbols_daily2)
  selected_symbols_daily3<-selected_symbols_daily[match(selected_symbols_daily2,selected_symbols_daily)]
  
  length(selected_symbols_daily3)
  
  eod_complete<-eod[which(eod$symbol %in% selected_symbols_daily3),,drop=F]
  
  eod_pvt<-dcast(eod_complete, date ~ symbol,value.var='adj_close',fun.aggregate = mean, fill=NULL)
  
  #tail(eod_pvt[,1:2])
  tail(tdays)
  # Merge with Calendar -----------------------------------------------------
  #fixes it here
  eod_pvt_complete<-merge.data.frame(x=tdays[,'date',drop=F],y=eod_pvt,by='date',all.x=T)
  eow_pvt_complete<-merge.data.frame(x=wdays[,'date',drop=F],y=eod_pvt,by='date',all.x=T)
  eom_pvt_complete<-merge.data.frame(x=mdays[,'date',drop=F],y=eod_pvt,by='date',all.x=T)
  
  table(is.na(eod_pvt))
  table(is.na(eod_pvt_complete))
  
  tail(eod_pvt_complete[,1:2]) #cuts off 2017-12-29, but re-appears if I rerun it, but eom_ret didn't have it before I reran it... which tells me it changes inbetween here and eom_ret
  #eom_pvt_complete[which(eom_pvt_complete$date=)]
  
  #which[eom_pvt_complete$date=='2017-12-29']
  #check
  #tail(eod_pvt_complete[,1:5]) #first 10 rows and first 5 columns 
  #ncol(eod_pvt_complete)
  #nrow(eod_pvt_complete)
  
  #use dates as row names and remove the date column
  rownames(eod_pvt_complete)<-eod_pvt_complete$date
  rownames(eom_pvt_complete)<-eom_pvt_complete$date
  rownames(eow_pvt_complete)<-eow_pvt_complete$date
  eod_pvt_complete$date<-NULL
  eom_pvt_complete$date<-NULL
  eow_pvt_complete$date<-NULL
  
  #re-check
  tail(eod_pvt_complete[,1:2]) #cuts off 2017-12-29...
  eod_pvt_complete[1:10,1:5] #first 10 rows and first 5 columns 
  #ncol(eod_pvt_complete)
  #nrow(eod_pvt_complete)
  #table(is.na(eod_pvt_complete))
  
  # Missing Data Imputation -----------------------------------------------------
  # We can replace a few missing (NA or NaN) data items with previous data
  # Let's say no more than 3 in a row...
  #strips it here
  require(zoo)
  #eod_pvt_complete<-na.locf(eod_pvt_complete,na.rm=F,fromLast=F,maxgap=3)
  eod_pvt_complete<-na.locf(eod_pvt_complete)
  #eow_pvt_complete<-na.locf(eow_pvt_complete,na.rm=F,fromLast=F,maxgap=3)
  eow_pvt_complete<-na.locf(eow_pvt_complete)
  #eom_pvt_complete<-na.locf(eom_pvt_complete,na.rm=F,fromLast=F,maxgap=3)
  eom_pvt_complete<-na.locf(eom_pvt_complete)
  #View(((eod_pvt_complete[c('SP500TR')])))
  #cut it here, see the true#  noo!!!
  
  #re-check, if years is set -3, then agt has 364 non na's, but 808 na's... something is getting past the filter here.
  #table(is.na(data.frame(eod_pvt_complete$AGT)))
  
  #colnames(eod_ret)
  
  tail(eod_pvt_complete[,1:2]) #cuts off 2017-12-29...
  eom_pvt_complete[1:10,1:5] #first 10 rows and first 5 columns 
  ncol(eod_pvt_complete)
  nrow(eod_pvt_complete)
  table(is.na(eod_pvt_complete))
  
  # Calculating Returns -----------------------------------------------------
  require(PerformanceAnalytics)
  eod_ret<-CalculateReturns(eod_pvt_complete)
  eow_ret<-CalculateReturns(eow_pvt_complete)
  eom_ret<-CalculateReturns(eom_pvt_complete)
  tail(eom_pvt_complete[,1:2]) #cuts off 2017-12-29...
  #check
  tail(eod_ret[,1:2])
  
  table(is.na(eod_pvt_complete))
  table(is.na(eod_ret))
  
  #check
  eod_ret[1:10,1:5] #first 10 rows and first 5 columns 
  ncol(eod_ret)
  nrow(eod_ret)
  #View(eod_ret)
  
  #remove the first row
  eod_ret<-tail(eod_ret,-1) #use tail with a negative value
  eow_ret<-tail(eow_ret,-1) #use tail with a negative value
  eom_ret<-tail(eom_ret,-1) #use tail with a negative value
  #check
  eod_ret[1:10,1:5] #first 10 rows and first 5 columns 
  tail(eom_ret[,1:2])
  eom_ret[,1:2]
  ncol(eod_ret)
  nrow(eod_ret)
  
  # YOUR TURN: calculate eom_ret (monthly returns)
  
  # Check for extreme returns -------------------------------------------
  # There is colSums, colMeans but no colMax so we need to create it
  colMax <- function(data) sapply(data, max, na.rm = TRUE)
  # Apply it
  max_daily_ret<-colMax(eod_ret)
  max_weekly_ret<-colMax(eow_ret)
  max_monthly_ret<-colMax(eom_ret)
  
  max_daily_ret[1:10] #first 10 max returns
  # And proceed just like we did with percentage (completeness)
  selected_symbols_daily3<-names(max_daily_ret)[which(max_daily_ret<=1.00)]
  selected_symbols_monthly=selected_symbols_daily3
  selected_symbols_weekly=selected_symbols_daily3
  #length(selected_symbols_daily)
  
  #subset eod_ret
  eod_ret<-eod_ret[,which(colnames(eod_ret) %in% selected_symbols_daily3)]
  eow_ret<-eow_ret[,which(colnames(eow_ret) %in% selected_symbols_daily3)]
  eom_ret<-eom_ret[,which(colnames(eom_ret) %in% selected_symbols_daily3)]
  
  library(imputeTS)
  
  eod_ret <- na.replace(eod_ret, 0)
  table(is.na(eod_ret))
  
  # eod_ret[, colSums(eod_ret) != 0]
  
  sum(eod_ret == 0)
  
  apply(eod_ret, 2, function(c)sum(c==0)/(days*2))
  #ncol(eod_ret)
  #ncol(eod_ret[,!apply(eod_ret, 2, function(c)sum(c==0)/(days*2))>.66])
  #nrow(eod_ret[,!apply(eod_ret, 2, function(c)sum(c==0)/(days*2))>.66])
  ncol(eod_ret)
  eod_ret <- eod_ret[,!apply(eod_ret, 2, function(c)sum(c==0)/(days*2))>.10]
  
  #eod on purpose
  eow_ret <- eow_ret[,!apply(eod_ret, 2, function(c)sum(c==0)/(days*2))>.10]
  eom_ret <- eom_ret[,!apply(eod_ret, 2, function(c)sum(c==0)/(days*2))>.10]
  ncol(eod_ret)
  #nrow(eod_ret["XSD"])
  
  #apply(eod_ret, 2, function(c)sum(c!=0))
  
  # x[,!(colSums(abs(x)) == 0)] 
  
  #y2 <- na.replace(y, 0)  
  
  #(eod_ret[!complete.cases(eod_pvt), ][1:5])
  
  #check
  tail(eom_ret[,1:2])
  eod_ret[1:10,1:5] #first 10 rows and first 5 columns 
  ncol(eod_ret)
  nrow(eod_ret)
  
  #YOUR TURN: subset eom_ret data
  
  # Export data from R to CSV -----------------------------------------------
  #write.csv(eod_ret,paste0('plots/',end_date,'_eod_ret.csv')
  #write.csv(eom_ret,paste0('plots/',end_date,'_eom_ret.csv')
  
  # You can actually open this file in Excel!
  
  
  # Tabular Return Data Analytics -------------------------------------------
  
  # We will select 'SP500TR' and c('AEGN','AAON','AMSC','ALCO','AGNC','AREX','ABCB','ABMD','ACTG','ADTN','AAPL','AAL')
  # We need to convert data frames to xts (extensible time series)
  source("colSortAndFilter.R")
  
  
  #all but last 3 mo's
  eod_ret_training<-head(eod_ret,-days)
  #eod_ret_training<-(eod_ret)
  #View(eod_ret_training[1:4])
  #View(eod_ret_testing[1:4])
  #eow_ret_training<-head(eow_ret,-weeks)
  eow_ret_training<-head(eow_ret,-weeks)
  #eom_ret_training<-head(eom_ret,-months)
  eom_ret_training<-head(eom_ret,-months)
  
  eod_ret_testing<-tail(eod_ret,days)
  eow_ret_testing<-tail(eow_ret,weeks)
  eom_ret_testing<-tail(eom_ret,months)
  
  #beta's derived using linear regression model between benchmark and sassy assets
  
  #I had to re-derive these, which means later
  
  #[RA4LM]: Returns Assets 4 Linear Model
  RA4LM<-as.xts(eod_ret[,-which(names(eod_ret) == "SP500TR")]) #colSortAndFilter.R
  RB4LM<-as.xts(eod_ret[,'SP500TR',drop=F]) #benchmark
  
  #[,-which(names(tail(eod_ret,days)) == "SP500TR")]) #colSortAndFilter.R
  #RATestingPre<-as.xts(eod_ret_training[-which(names(eod_ret) == "SP500TR")]) #colSortAndFilter.R
  #RBTestingPre<-as.xts(eod_ret_training[-which(names(eod_ret) == "SP500TR")]) #colSortAndFilter.R
  
  #head(Rb,-days)
  
  x <- RB4LM
  nrow(x)  
  
  #tail(x)  
  
  y <- RA4LM
  
  #replace na with 0
  #https://stackoverflow.com/questions/8161836/how-do-i-replace-na-values-with-zeros-in-an-r-dataframe
  
  library(imputeTS)
  
  x2 <- na.replace(x, 0)
  y2 <- na.replace(y, 0)
  
  nrow(y)
  
  #head
  #xtr<-head(x2,-days)
  xtr<-head(x2)
  #ytr<-head(y2,-days)
  ytr<-head(y2)
  
  nrow(xtr)
  nrow(ytr)
  
  #tail
  xtst<-tail(x2,days)
  ytst<-tail(y2,days)
  
  linearModTotal <- lm(y2~x2)
  linearModTraining <- lm(ytr~xtr)
  
  linearModTesting <- lm(ytst~xtst)
  
  #print(linearModTotal)
  
  #training capm beta's, sorted because no follow up ops outside of my algorithm rely on it's order (simple filter operation)
  
  #trainingBetaSorted[,]
  
  #t20Beta
  
  #Return.cumulative(eod_ret_training$TNA)
  trainingBetaSorted <- colSortMax(linearModTraining$coefficients)
  testingBetaSorted <- colSortMax(linearModTesting$coefficients)
  
  trainingAvgSorted <- colSortAvg(eod_ret_training)
  testingAvgSorted <- colSortAvg(eod_ret_testing)
  
  trainingCumRetSorted <- colSortMax(Return.cumulative(eod_ret_training))
  testingCumRetSorted <- colSortMax(Return.cumulative(eod_ret_testing))
  
  totalBetaSorted <- colSortMax(linearModTotal$coefficients)
  
  CR_Ra_training <- colSortMax(Return.cumulative(eod_ret_training))
  avg_Ra_training <- colSortAvg(eod_ret_training)
  
  CR_Ra_testing <- colSortMax(Return.cumulative(eod_ret_testing))
  avg_Ra_testing <- colSortAvg(eod_ret_testing)
  
  CR_RaW_training <- colSortMax(Return.cumulative(eow_ret_training))
  avg_RaW_training <- colSortAvg(eow_ret_training)
  
  CR_RaM_training <- colSortMax(Return.cumulative(eom_ret_training))
  avg_RaM_training <- colSortAvg(eom_ret_training)
  
  #top 20 by cumulative return
  t20Beta<-c()
  b20Beta<-c()
  
  t20CR_Ra<-c()
  t20CR_RaW<-c()
  t20CR_RaM<-c()
  
  #t20AVGR_Ra<-c()
  #t20AVGR_RaW<-c()
  #t20AVGR_RaM<-c()
  
  #bottom 20 
  b20CR_Ra<-c()
  b20CR_RaW<-c()
  b20CR_RaM<-c()
  
  b20AVGR_Ra<-c()
  b20AVGR_RaW<-c()
  b20AVGR_RaM<-c()
  
  #bottom 20 
  
  #chart.Boxplot(eod_ret[t20CR])
  #top/bottom 2.5%
  setPercent=round(length(colnames(eod_pvt_complete))*.0015625,0)
  if(setPercent<=4)
  {setPercent=5}
  
  #goal should be hold based on beta's, but not shorts
  #t20Beta<-trainingBetaSorted[,1:setPercent]
  
  #colnames(data.frame(Ra_training)[trainingBetas$colname])[1:setPercent]
  
  #eod_ret[,basedOnBetas]
  #basedOnBetas
  #list_Ra
  #write.csv(eod_ret[,basedOnBetas],"plots/Opt_Ret_WBetas.csv")
  
  #need to rename eod to training?  no, doesn't need to be eod_ret_training because of the filter applied via []
  #why am I setting a percent here if these are not sorted... oh, the CR_Ra_training is sorted... doesn't hurt though.
  
  #doesn't give column names
  #(t(head(CR_Ra_training,setPercent)))
  
  #if shorting, negative beta's doesn't do any good.
  #note, tails isn't necessarily in reverse order, just so happens I'm only grabbing the last few #'s...
  t20Beta<-head(colnames(data.frame((eod_ret_training)[trainingBetaSorted$colname])),setPercent)
  
  #filter to non negative
  #length(ratr[which(all_r[]>=Lhinge & all_r[]<=Uhinge)])/length(all_r)  
  #head(eod[which(eod$symbol=='SP500TR'),])
  
  #View((eod_ret_training)[trainingBetaSorted[which(trainingBetaSorted$mean>=0),]$colname])
  b20Beta<-tail(colnames(data.frame((eod_ret_training)[trainingBetaSorted[which(trainingBetaSorted$mean>=0),]$colname])),setPercent)
  #b20Beta<-tail(colnames(data.frame((eod_ret_training)[trainingBetaSorted$colname])),setPercent)
  
  t20Avg<-head(colnames(data.frame((eod_ret_training)[trainingAvgSorted$colname])),setPercent)
  b20Avg<-tail(colnames(data.frame((eod_ret_training)[trainingAvgSorted$colname])),setPercent)
  
  t20Cum<-head(colnames(data.frame((eod_ret_training)[trainingCumRetSorted$colname])),setPercent)
  b20Cum<-tail(colnames(data.frame((eod_ret_training)[trainingCumRetSorted$colname])),setPercent)
  
  #t20Mix_Ra<-unique(c(t20CR_Ra,t20AVGR_Ra))
  #t20Mix_RaW<-unique(c(t20CR_RaW,t20AVGR_RaW))
  #t20Mix_RaM<-unique(c(t20CR_RaM,t20AVGR_RaM))
  
  #b20Mix_Ra<-unique(c(b20CR_Ra,b20AVGR_Ra))
  #b20Mix_RaW<-unique(c(b20CR_RaW,b20AVGR_RaW))
  #b20Mix_RaM<-unique(c(b20CR_RaM,b20AVGR_RaM))
  
  list_Ra<-c()
  #list_Ra<-c(t20Mix_Ra,b20Mix_Ra)
  #list_Ra<-c(t20Beta,b20Beta)
  list_upper<-c()
  list_lower<-c()
  #list_upper<-c(t20Beta)
  #list_lower<-c(b20Beta)
  list_lower<-c(b20Cum)
  list_upper<-c(t20Cum)
  #View(b20Beta)
  list_Ra<-c(list_upper,list_lower)
  #list_Ra<-c(t20Beta)
  #list_Ra<-c(basedOnBetas,b20Mix_Ra)
  #list_Ra<-basedOnBetas
  #eod_ret[,list_Ra]
  
  write.csv(eod_ret_training[c(list_upper,list_lower)],paste0("plots/",end_date,"_listUpperLowerTrainingReturns.csv"))
  write.csv(eod_ret_testing[c(list_upper,list_lower)],paste0("plots/",end_date,"_listUpperLowerTestingReturns.csv"))
  
  #list_RaW<-c(t20Mix_RaW,b20Mix_RaW)
  #list_RaM<-c(t20Mix_RaM,b20Mix_RaM)
  
  list_RaW<-c(list_upper,list_lower)
  list_RaM<-c(list_upper,list_lower)
  #eod_ret[,list_Ra]
  
  #based on top/bottom 20
  Ra<-as.xts(eod_ret[list_Ra]) #colSortAndFilter.R
  RaW<-as.xts(eow_ret[list_Ra]) #colSortAndFilter.R
  RaM<-as.xts(eom_ret[list_Ra]) #colSortAndFilter.R
  
  #check
  #Ra$AGT
  
  # remove NA's and nulls
  #table(is.na(Ra))
  #table(Ra[,colnames(Ra)])
  #tail(Ra$HDV)
  #table(is.na(Ra$HDV))
  #65/1106
  
  #still necessary to augment list with 0's for calculation.  As long as I know it's done as late in the game as possible.
  Ra[is.na(Ra)] <- 0
  RaW[is.na(RaW)] <- 0
  RaM[is.na(RaM)] <- 0
  
  #check
  #Ra$AGT
  
  tail(eom_ret[,1:2])
  tail(RaM)
  #check
  #tail(RaM[,1:2])
  
  Rb<-as.xts(eod_ret[,'SP500TR',drop=F]) #benchmark
  RbW<-as.xts(eow_ret[,'SP500TR',drop=F]) #benchmark
  RbM<-as.xts(eom_ret[,'SP500TR',drop=F]) #benchmark
  
  #check
  #tail(RbM)
  
  head(Ra)
  tail(eod$AGT)
  tail(Ra$AGT)
  head(Rb)
  
  # And now we can use the analytical package...
  
  # Stats #expensive
  #table.Stats(Ra)
  
  # Distributions #expensive
  #table.Distributions(Ra)
  
  # Returns #expensive
  #table.AnnualizedReturns(cbind(Rb,Ra),scale=252) # note for monthly use scale=12
  
  # Accumulate Returns
  acc_Ra<-Return.cumulative(Ra)
  acc_RaW<-Return.cumulative(RaW)
  acc_RaM<-Return.cumulative(RaM)
  
  acc_Rb<-Return.cumulative(Rb)
  acc_RbW<-Return.cumulative(RbW)
  acc_RbM<-Return.cumulative(RbM)
  
  # Capital Assets Pricing Model #expensive
  #table.CAPM(Ra,Rb)
  
  # YOUR TURN: try other tabular analyses
  
  # YOUR TURN: try other charts
  
  # MV Portfolio Optimization -----------------------------------------------
  #trick: re-apply same list/weights to a portfolio that covers twice the distance to determine future success over similar distance (going to want a bandwidth)
  # withold the last 252 trading days
  #Ra_training<-head(Ra,-days)
  Ra_training<-(Ra)
  #Rb_training<-head(Rb,-days)
  Rb_training<-(Rb)
  
  #all but 13 weeks
  #RaW_training<-head(RaW,-weeks)
  RaW_training<-(RaW)
  #RbW_training<-head(RbW,-weeks)
  RbW_training<-(RbW)
  
  
  #all but 3 months
  RaM_training<-(RaM)
  #RaM_training<-head(RaM,-months)
  RbM_training<-(RbM)
  #RbM_training<-head(RbM,-months)
  
  # use the last 21 trading days for testing
  Ra_testing<-tail(Ra,days)
  Rb_testing<-tail(Rb,days)
  
  # use last 13 weeks
  RaW_testing<-tail(RaW,weeks)
  RbW_testing<-tail(RbW,weeks)
  
  #View(Ra_testing[,1:4])
  #View(Rb_testing)
  
  # use last 3 months
  RaM_testing<-tail(RaM,months)
  RbM_testing<-tail(RbM,months)
  
  write.csv(Rb_training,"plots/rb_Training.csv")
  write.csv(Rb_testing,"plots/rb_Testing.csv")
  
  write.csv(eod_ret_training[t20Beta],paste0("plots/",end_date,"_Training_T20B.csv"))
  write.csv(eod_ret_testing[t20Beta],paste0("plots/",end_date,"_Testing_T20B.csv"))
  
  write.csv(eod_ret_training[b20Beta],paste0("plots/",end_date,"_Training_B20B.csv"))
  write.csv(eod_ret_testing[b20Beta],paste0("plots/",end_date,"_Testing_B20B.csv"))
  
  write.csv(eod_ret_training[t20Avg],paste0("plots/",end_date,"_Training_T20A.csv"))
  write.csv(eod_ret_testing[t20Avg],paste0("plots/",end_date,"_Testing_T20A.csv"))
  
  write.csv(eod_ret_training[b20Avg],paste0("plots/",end_date,"_Training_B20A.csv"))
  write.csv(eod_ret_testing[b20Avg],paste0("plots/",end_date,"_Testing_B20A.csv"))
  
  write.csv(trainingBetaSorted,paste0("plots/",end_date,"_trainingBetaSorted.csv"))
  write.csv(testingBetaSorted,paste0("plots/",end_date,"_testingBetaSorted.csv"))
  
  write.csv(trainingAvgSorted,paste0("plots/",end_date,"_trainingAvgSorted.csv"))
  write.csv(testingAvgSorted,paste0("plots/",end_date,"_testingAvgSorted.csv"))
  
  write.csv(trainingCumRetSorted,paste0("plots/",end_date,"_trainingCumRetSorted.csv"))
  write.csv(testingCumRetSorted,paste0("plots/",end_date,"_testingCumRetSorted.csv"))
  
  #list stuff
  
  acc_Ra_training<-Return.cumulative(Ra_training)
  
  acc_Ra_testing<-Return.cumulative(Ra_testing)
  
  #Return profiling
  #all returns in one list (for classification)
  
  #allr<-eod_ret[colnames(eod_complete)]
  
  #allr[is.na(allr)] <- 0
  
  #training
  ratr<-data.frame(stack(((data.frame(Ra_training)))))$values
  brtr<-data.frame(stack(((data.frame(Rb_training)))))$values
  
  #testing
  rate<-data.frame(stack(((data.frame(Ra_testing)))))$values
  brte<-data.frame(stack(((data.frame(Rb_testing)))))$values
  
  #all but SP500TR
  all_r<-(c(ratr,rate))
  
  summary(all_r)
  
  length(rate)
  length(ratr)
  
  length(rate) + length(ratr)
  
  length(all_r)
  
  #Ra[is.na(Ra)] <- 0
  #RaW[is.na(RaW)] <- 0
  #RaM[is.na(RaM)] <- 0
  
  #returns assets training
  # IQR used for rounding determination and subsequent classification of returns
  
  chart.Boxplot(all_r,xlab="all returns training")
  quantile(all_r)
  summary(all_r)
  write.csv(all_r,paste0("plots/",end_date,"_all_r.csv"))
  
  length(all_r)
  
  
  IQR=quantile(all_r)[3]-quantile(all_r)[2]
  Lhinge<-c()
  Lhinge=quantile(all_r)[2]-IQR*1.5
  
  Uhinge<-c()
  Uhinge=quantile(all_r)[4]+IQR*1.5
  
  #length(ratr[which(all_r[]>=Lhinge & all_r[]<=Uhinge)])/length(all_r)      
  IQRRange=Uhinge-Lhinge
  
  HingeRange=Uhinge-Lhinge
  
  #percent captured within LHinge and UHinge of Ra_training
  length(ratr[which(all_r[]>=Lhinge & all_r[]<=Uhinge)])/length(all_r)
  
  Lhinge+IQR*1/8
  
  breaks=c(quantile(all_r)[1],Lhinge,Lhinge+HingeRange*1/8,Lhinge+HingeRange*2/8,Lhinge+HingeRange*3/8,Lhinge+HingeRange*4/8,Lhinge+HingeRange*5/8,Lhinge+HingeRange*6/8,Lhinge+HingeRange*7/8,Uhinge,quantile(all_r)[5])  
  
  hist(ratr,breaks)
  
  hist(all_r,breaks)
  
  #returns benchmark training
  chart.Boxplot(brtr,xlab="S&P500TR training")
  
  #returns assets
  
  #example, outliers are defined as 1Q-IQR*1.5 and 3Q*+IQR*1.5, why not set up a histogram that captures this.
  
  hcr<-data.frame(stack(((data.frame(Ra_training)[list_upper]))))$values
  hist(hcr,breaks)
  boxplot(hcr)
  
  #disable if not using negative
  lcr<-data.frame(stack(((data.frame(Ra_training)[list_lower]))))$values
  hist(lcr)
  hist(hcr, breaks)
  
  mean(hcr)
  mean(lcr)
  
  hcrT20Testing<-data.frame(stack(tail((data.frame(Ra_testing)[list_upper]))))$values
  
  #disable if not using negative weights
  lcrT20Testing<-data.frame(stack(tail((data.frame(Ra_testing)[list_lower]))))$values
  
  mean(hcrT20Testing)
  mean(lcrT20Testing)
  # Graphical Return Data Analytics -----------------------------------------
  
  #long portfolio
  hist(hcr,breaks)
  
  #short portfolio
  #hist(lcr,breaks)
  
  #long testing
  hist(hcrT20Testing,breaks)
  
  #short testing
  hist(lcrT20Testing,breaks)
  
  # Cumulative returns chart
  chart.CumReturns(Ra,legend.loc = 'topleft')
  chart.CumReturns(Rb,legend.loc = 'topleft')
  
  colnames(eod_ret_training)
  
  #head(eow_ret,tdays/2)
  
  #Box plots
  chart.Boxplot(cbind(Rb_training,Ra_training))
  chart.Boxplot(Rb_training)
  
  chart.Drawdown(Ra,legend.loc = 'bottomleft')
  chart.Drawdown(Ra_testing,legend.loc = 'bottomleft')
  
  #boxplot(hcr,Rb_training,lcr)
  
  summary(hcr)
  StdDev(hcr)
  
  #S&P500
  summary(Rb_training)
  StdDev(Rb_training)
  
  #summary(lcr)
  #StdDev(lcr)
  
  #quantile(hcr,c(0,.05,.5,.95,1))
  sum(acc_Ra_training[,list_upper])
  
  #quantile(lcr,c(0,.05,.5,.95,1))
  #need to disable if not using negatives
  #sum(acc_Ra_training[,b20Beta])
  
  #quantile(hcrT20Testing,c(0,.05,.5,.95,1))
  sum(acc_Ra_testing[,list_upper])
  
  #quantile(lcrT20Testing,c(0,.05,.5,.95,1))
  
  #need to disable if not using negatives
  #sum(acc_Ra_testing[,b20Beta])
  
  #plot.new()
  
  #best/worst over testing period
  summary(hcrT20Testing)
  StdDev(hcrT20Testing)
  
  #S&P500
  summary(Rb_testing)
  StdDev(Rb_testing)
  
  #summary(lcrT20Testing)
  #StdDev(lcrT20Testing)
  
  #boxplot(hcrT20Testing,Rb_testing)
  boxplot(hcrT20Testing,Rb_testing,lcrT20Testing)
  
  t.test(hcr,lcr)
  t.test(hcrT20Testing,lcrT20Testing)
  ttest<-t.test(hcr,lcr)
  names(ttest)
  ttest$statistic
  
  summary(hcrT20Testing)
  summary(lcrT20Testing)
  
  #test training
  sum(Return.cumulative(eod_ret_training[list_upper]))
  sum(Return.cumulative(eod_ret_training[list_lower]))
  
  #test testing
  sum(Return.cumulative(eod_ret_testing[list_upper]))
  sum(Return.cumulative(eod_ret_testing[list_lower]))
  
  #Error
  (sum(Return.cumulative(eod_ret_training[list_upper]))-sum(Return.cumulative(eod_ret_training[list_lower])))
  
  #Error 2
  (sum(Return.cumulative(eod_ret_testing[list_upper]))-sum(Return.cumulative(eod_ret_testing[list_lower])))
  
  #test testing
  summary(Rb_testing)
  summary(Rb_training)
  
  #optimize the MV (Markowitz 1950s) portfolio weights based on training
  table.AnnualizedReturns(Rb_training)
  mar<-mean(Rb_training) #we need daily minimum acceptabe return
  marW<-mean(RbW_training) #we need daily minimum acceptable return
  marM<-mean(RbM_training) #we need daily minimum acceptable return
  
  require(PortfolioAnalytics)
  require(ROI) # make sure to install it
  require(ROI.plugin.quadprog)  # make sure to install it
  pspec<-portfolio.spec(assets=colnames(Ra_training[,list_Ra]))
  
  pspec<-add.objective(portfolio=pspec,type="risk",name='StdDev')
  pspec<-add.constraint(portfolio=pspec,type="full_investment")
  pspec<-add.constraint(portfolio=pspec,type="return",return_target=mar)
  
  pspecW<-portfolio.spec(assets=colnames(RaW_training))
  pspecW<-add.objective(portfolio=pspecW,type="risk",name='StdDev')
  pspecW<-add.constraint(portfolio=pspecW,type="full_investment")
  pspecW<-add.constraint(portfolio=pspecW,type="return",return_target=marW)
  
  pspecM<-portfolio.spec(assets=colnames(RaM_training))
  pspecM<-add.objective(portfolio=pspecM,type="risk",name='StdDev')
  pspecM<-add.constraint(portfolio=pspecM,type="full_investment")
  pspecM<-add.constraint(portfolio=pspecM,type="return",return_target=marM)
  
  #optimize portfolio
  opt_p<-optimize.portfolio(R=Ra_training,portfolio=pspec,optimize_method = 'ROI')
  opt_pW<-optimize.portfolio(R=RaW_training,portfolio=pspecW,optimize_method = 'ROI')
  #opt_p=opt_pW
  opt_pM<-optimize.portfolio(R=RaM_training,portfolio=pspecM,optimize_method = 'ROI')
  
  #extract weights
  opt_w<-opt_p$weights
  #opt_wW<-opt_pW$weights
  opt_wW<-opt_w
  #opt_wM<-opt_pM$weights
  opt_wM<-opt_w
  
  #length(t20Mix_Ra)
  #length(b20Mix_Ra)
  
  #length(t20Mix_RaW)
  #length(b20Mix_RaW)
  
  #length(t20Mix_RaM)
  #length(b20Mix_RaM)
  
  #keep this above the print statements
  print(paste("iterator", iterator))
  
  #reporting
  
  #finally got the scatterplot working!
  probs=c(0,.1,.25,.5,.75,.9,1)
  
  all_profile<-quantile(all_r,probs, na.rm =T, names = F, type = 7)
  
  firstSixth = (all_profile[2]-all_profile[1])*(.1-.0)
  secondSixth = (all_profile[3]-all_profile[2])*(.25-.1)
  thirdSixth = (all_profile[4]-all_profile[3])*(.5-.25)
  fourthSixth = (all_profile[5]-all_profile[4])*(.75-.5)
  fifthSixth = (all_profile[6]-all_profile[5])*(.9-.75)
  sixthSixth = (all_profile[7]-all_profile[6])*(1-.9)
  
  sevenNumModSdev=firstSixth+secondSixth+thirdSixth+fourthSixth+fifthSixth+sixthSixth
  #weight=2
  print(paste("start: ", as.Date(start_dateInLoop), "end: ", as.Date(end_dateInLoop), "Lag", iterator))
  chart.CumReturns(eod_ret_training[c(t20Cum,"SP500TR")],legend.loc = 'topleft')
  chart.CumReturns(eod_ret_testing[c(t20Cum,"SP500TR")],legend.loc = 'topleft')
  
  
  #sp500
  #chart.CumReturns(Ra_testing)
  #chart.CumReturns(Ra_testing)
  #sp500
  #chart.CumReturns(Rb_testing)
  
  
  print(paste(c("train portfolio",round(sum(Return.cumulative(eod_ret_training[t20Cum])),3))))
  print(paste(c("train SP500",round(sum(Return.cumulative(eod_ret_training["SP500TR"])),3))))
  
  print(paste(c("test portfolio",round(sum(Return.cumulative(eod_ret_testing[t20Cum])),3))))
  print(paste(c("test SP500",round(sum(Return.cumulative(eod_ret_testing["SP500TR"])),3))))
  
  for (weight in 2:3)
  {
    print(paste("Weights", positive, "vs", negative))
    positive=weight
    negative=abs((positive)-1)*-1
    
    opt_w<-c()
    #length(list_Ra)
    #opt_w[1:length(t20Mix_Ra)]<-positive/length(t20Mix_Ra)
    opt_w[1:length(list_lower)]<-positive/length(list_upper)
    #opt_w[1:length(list_Ra)]=1/length(list_Ra)
    
    #use with negative
    opt_w[(length(list_upper)+1):(length(list_lower)+length(list_upper))]<-negative/length(list_lower)
    
    sum(opt_w)
    
    #apply weights to test returns
    Rp<-Rb_testing # easier to apply the existing structure
    RpW<-RbW_testing # easier to apply the existing structure
    RpM<-RbM_testing # easier to apply the existing structure
    #define new column that is the dot product of the two vectors
    Rp$ptf<-Ra_testing %*% opt_w
    RpW$ptf<-RaW_testing %*% opt_w
    RpM$ptf<-RaM_testing %*% opt_w
    
    #check
    head(Rp)
    tail(Rp)
    
    #Compare basic metrics
    #table.AnnualizedReturns(Rp) expensive
    
    Return.cumulative(Rp)
    Return.cumulative(RpW)
    Return.cumulative(RpM)
    
    #b20AVGR_Ra is b20AVGR_Ra_training, I just didn't name b20AVGR_Ra correctly, but the [filter] in b20AVGR_Ra specifies names sourced from training data.    
    #what is the average
    
    t20Beta <- head(trainingBetaSorted,setPercent)[,1]
    b20Beta <- tail(trainingBetaSorted,setPercent)[,1]
    
    #class(t20Beta)
    
    #class(t20AVGR_Ra)
    
    #need to extract these as names for this next step.
    
    #graph cumulative returns by best/worst beta's before and after
    #chart.CumReturns(eod_ret_training[t20Beta])
    chart.CumReturns(eod_ret_training[unlist(t20Beta)])
    
    #as.character(t20Beta)
    #typeof(t20Beta)
    #typeof(b20Avg)
    chart.CumReturns(eod_ret_testing[unlist(t20Beta)])
    
    chart.CumReturns(eod_ret_training[unlist(b20Beta)])
    chart.CumReturns(eod_ret_testing[unlist(b20Beta)])
    
    #graph cumulative returns by best/worst avg's before and after    
    chart.CumReturns(eod_ret_training[unlist(t20Avg)])
    chart.CumReturns(eod_ret_testing[unlist(t20Avg)])
    
    chart.CumReturns(eod_ret_training[unlist(b20Avg)])
    chart.CumReturns(eod_ret_testing[unlist(b20Avg)])
    
    #compare accumulated returns via means of best/worst beta's
    
    #good returns, before after, do predictions hold?
    mean_acc_training_beta_t20 <- mean(Return.cumulative(eod_ret_training[unlist(t20Beta)]))
    mean_acc_testing_beta_t20 <- mean(Return.cumulative(eod_ret_testing[unlist(t20Beta)]))
    
    #bad returns, before after, do predictions hold?
    mean_acc_training_beta_b20 <- mean(Return.cumulative(eod_ret_training[unlist(b20Beta)]))
    mean_acc_testing_beta_b20 <- mean(Return.cumulative(eod_ret_testing[unlist(b20Beta)]))
    
    #compare accumulated returns via means of best/worst average cumulative return
    
    #good returns, before after, do predictions hold?
    mean_acc_training_t20 <- mean(Return.cumulative(eod_ret_training[unlist(t20Cum)]))
    mean_acc_testing_t20 <- mean(Return.cumulative(eod_ret_testing[unlist(t20Cum)]))
    
    #bad returns, before after, do predictions hold?
    mean_acc_training_b20 <- mean(Return.cumulative(eod_ret_training[unlist(b20Cum)]))
    mean_acc_testing_b20 <- mean(Return.cumulative(eod_ret_testing[unlist(b20Cum)]))
    
    print (paste("Beta: CumRet", "mean_acc tr_t20 te_t20 tr_b20 te_b20", round(mean_acc_training_t20,3) , round(mean_acc_testing_t20,3) , round(mean_acc_training_b20,3), round(mean_acc_testing_b20,3)))
    
    #compare accumulated returns via means of best/worst avg return
    
    #good returns, before after, do predictions hold?
    mean_acc_training_t20 <- mean(Return.cumulative(eod_ret_training[unlist(t20Avg)]))
    mean_acc_testing_t20 <- mean(Return.cumulative(eod_ret_testing[unlist(t20Avg)]))
    
    #bad returns, before after, do predictions hold?
    mean_acc_training_b20 <- mean(Return.cumulative(eod_ret_training[unlist(b20Avg)]))
    mean_acc_testing_b20 <- mean(Return.cumulative(eod_ret_testing[unlist(b20Avg)]))
    
    print (paste("Beta: CumRet", "mean_acc tr_t20 te_t20 tr_b20 te_b20", round(mean_acc_training_t20,3) , round(mean_acc_testing_t20,3) , round(mean_acc_training_b20,3), round(mean_acc_testing_b20,3)))
    
    #compare accumulated returns, avg
    
    #good returns, before after, do predictions hold?
    mean_acc_training_t20 <- mean(Return.cumulative(eod_ret_training[unlist(t20Avg)]))
    mean_acc_testing_t20 <- mean(Return.cumulative(eod_ret_testing[unlist(t20Avg)]))
    
    #bad returns, before after, do predictions hold?
    mean_acc_training_b20 <- mean(Return.cumulative(eod_ret_training[unlist(b20Avg)]))
    mean_acc_testing_b20 <- mean(Return.cumulative(eod_ret_testing[unlist(b20Avg)]))
    
    print (paste("Avg: CumRet: tr_t20 te_t20 tr_b20 te_b20", round(mean_acc_training_t20,3) , round(mean_acc_testing_t20,3) , round(mean_acc_training_b20,3), round(mean_acc_testing_b20,3)))
    
    #chart.CumReturns(Ra_training[,b20AVGR_Ra])
    
    #boxplot(data.frame(stack(((data.frame(eod_ret_training[t20AVGR_Ra])))))$values, horizontal = 1)
    #boxplot(data.frame(stack(((data.frame(eod_ret_testing[t20AVGR_Ra])))))$values, horizontal = 1)
    
    print (paste("Avg: CumRet: tr_t20 te_t20 tr_b20 te_b20", round(mean_acc_training_t20,3) , round(mean_acc_testing_t20,3) , round(mean_acc_training_b20,3), round(mean_acc_testing_b20,3)))

    #uses lists
    #chart.CumReturns(Ra_training,legend.loc = 'topleft')
    
    # Chart Hypothetical Portfolio Returns ------------------------------------
    
    #jpeg(paste0(end_date,"rplot.jpg"))
    #dev.off()
    
    #chart.CumReturns(Ra_training[,t20Mix_Ra])
    #chart.CumReturns(Ra_testing[,t20Mix_Ra])
    
    #chart.CumReturns(Rp,legend.loc = 'topleft')
    
    #chart.CumReturns(RpW,legend.loc = 'topleft')
    #chart.CumReturns(RpM,legend.loc = 'topleft')
    #View(eom_ret[,list_Ra])
    
    #table(is.na(eom_ret[,list_Ra]))
    
    # End of Part 3c
    # End of Stock Market Case Study 
    
    #don't use without shorts
    #scores<-rbind(scores,Return.cumulative(Rp$ptf))
    print(paste( "The return is", round(Return.cumulative(Rp$ptf),3)))
    
  }
  
  #reporting
  
  #testing
  for (name in c(list_upper,list_lower))
  {
    
    mean(eod_ret_training[,name])
    
    #beta correlation plot training
    jpeg(paste0("plots/",end_date,name,"_betaTrainPlot.jpg"))
    x=eod_ret_training$SP500TR; y=eod_ret_training[,name]
    plot(x, y, ylab=paste("training",name),xlab="SP500TR")
    #text(x, y, paste(round(x, 2), round(y, 3), sep=", "), cex=1.1) # for (x, y)   
    dev.off()
    
    #... testing
    jpeg(paste0("plots/",end_date,name,"_betaTestPlot.jpg"))
    x=eod_ret_testing$SP500TR; y=eod_ret_testing[,name]
    plot(x, y, ylab=paste("testing",name),xlab="SP500TR")
    #text(x, y, paste(round(x, 2), round(y, 3), sep=", "), cex=1.1) # for (x, y)   
    dev.off()
    
    #Return probability plot training
    jpeg(paste0("plots/",end_date,name,"_ProbTrainPlot.jpg"))
    profile<-quantile(eod_ret_training[,name],probs, na.rm =T, names = F, type = 7)
    x = probs; y = profile
    plot(x, y, ylab=name,type="o",xlab="Training Return Probability")
    text(x, y, paste(round(x, 2), round(y, 3), sep=", "), cex=1.1) # for (x, y)   
    dev.off()
    
    #... testing
    jpeg(paste0("plots/",end_date,name,"_ProbTestPlot.jpg"))
    profile<-quantile(eod_ret_testing[,name],probs, na.rm =T, names = F, type = 7)
    x = probs; y = profile
    plot(x, y, ylab=name,type="o",xlab="Testing Return Probability")
    text(x, y, paste(round(x, 2), round(y, 3), sep=", "), cex=1.1) # for (x, y)   
    dev.off()
    
    #Return Box Plot training
    jpeg(paste0("plots/",end_date,name,"_training_retBoxPlot.jpg"))
    boxplot(eod_ret_training[,name],horizontal=1,ylab=name,xlab="training")
    dev.off()
    
    #... testing
    jpeg(paste0("plots/",end_date,name,"_testing_retBoxPlot.jpg"))
    boxplot(eod_ret_testing[,name],horizontal=1,ylab=name,xlab="testing")
    dev.off()
    
    #Return Density Plot training
    jpeg(paste0("plots/",end_date,name,"_training_retDensPlot.jpg"))
    d <- density(eod_ret_training[,name],na.rm=T)
    plot(d,ylab=name)
    dev.off()
    
    #Return Density Plot training
    jpeg(paste0("plots/",end_date,name,"_testing_retDensPlot.jpg"))
    d <- density(eod_ret_testing[,name],na.rm=T)
    plot(d,ylab=name)
    dev.off()
    
    #... testing
    jpeg(paste0("plots/",end_date,name,"_testing_retDensPlot.jpg"))
    d <- density(eod_ret_testing[,name],na.rm=T)
    plot(d,ylab=name)
    dev.off()
    
    #Histogram, does it need to show mean?
    #training
    jpeg(paste0("plots/",end_date,name,"_trainingHistPlot.jpg"))
    hist(eod_ret_training[,name],breaks,ylab=name)
    dev.off()
    
    #...testing
    jpeg(paste0("plots/",end_date,name,"_testingHistPlot.jpg"))
    hist(eod_ret_testing[,name],breaks,ylab=name)
    dev.off()
    
    #turn into gif, yes, run bat file, then delete! :
    
    command <- paste0("convert -delay 100 -loop 0 ","plots/",end_date,name,"*.jpg ","plots/",end_date,"_",name,".gif")
    system(command, intern = TRUE)
    #when using erase, states command not found
    command <- paste0("rm ","plots/",end_date,name,"*.jpg -f")
    system(command, intern = TRUE)
    
  }
  
  jpeg(paste0("plots/",end_date,"_retAllProbPlot.jpg"))
  x = probs; y = all_profile;
  plot(x,y,type="o",xlab="Return All Probability")
  text(x, y, paste(round(x, 2), round(y, 3), sep=", "), cex=1.1) # for (x, y)  
  dev.off()
  
  #upper training/testing profile Boxplot
  jpeg(paste0("plots/",end_date,"_training_upper_BoxPlot.jpg"))
  training_upper<-data.frame(stack(((data.frame(Ra_training[,list_upper])))))$values
  boxplot(training_upper,horizontal=1,xlab="training upper")
  dev.off()
  
  jpeg(paste0("plots/",end_date,"_testing_upper_BoxPlot.jpg"))
  testing_upper<-data.frame(stack(((data.frame(Ra_testing[,list_upper])))))$value
  boxplot(testing_upper,horizontal=1,xlab="testing upper")
  dev.off()
  
  #lower training/testing profile Boxplot
  jpeg(paste0("plots/",end_date,"_training_lower_BoxPlot.jpg"))
  training_lower<-data.frame(stack(((data.frame(Ra_training[,list_lower])))))$values
  boxplot(training_lower,horizontal=1,xlab="training lower")
  dev.off()
  
  jpeg(paste0("plots/",end_date,"_testing_lower_BoxPlot.jpg"))
  testing_lower<-data.frame(stack(((data.frame(Ra_testing[,list_lower])))))$value
  boxplot(testing_lower,horizontal=1,xlab="testing lower")
  dev.off()
  
  #17
  jpeg(paste0("plots/",end_date,"_retTrainingCumUpperPlot.jpg"))
  d<-chart.CumReturns(eod_ret_training[list_upper])
  #View(eod_ret_training[list_upper])
  plot(d)
  dev.off()
  #15
  jpeg(paste0("plots/",end_date,"_retTestingCumUpperPlot.jpg"))
  d<-chart.CumReturns(eod_ret_testing[list_upper])
  plot(d)
  dev.off()
  #16
  jpeg(paste0("plots/",end_date,"_retTrainingCumLowerPlot.jpg"))
  d<-chart.CumReturns(eod_ret_training[list_lower])
  plot(d)
  dev.off()
  #14
  jpeg(paste0("plots/",end_date,"_retTestingCumLowerPlot.jpg"))
  d<-chart.CumReturns(eod_ret_testing[list_lower])
  plot(d)
  dev.off()
  
  opt_wp<-optimize.portfolio(R=Ra_training,portfolio=pspec,optimize_method = 'ROI')
  
  opt_wpw<-opt_wp$weights
  
  #sum(combinedOpt)
  
  positive=2
  negative=-1
  
  opt_w<-c()
  #length(list_Ra)
  #opt_w[1:length(t20Mix_Ra)]<-positive/length(t20Mix_Ra)
  opt_w[1:length(list_upper)]<-positive/length(list_upper)
  #opt_w[1:length(t20Mix_Ra)]<-.5/length(t20Mix_Ra)
  #opt_w[1:length(list_Ra)]=1/length(list_Ra)
  
  mean_acc_training_upper_t20 <- mean(Return.cumulative(eod_ret_training[list_upper]))
  mean_acc_training_lower_t20 <- mean(Return.cumulative(eod_ret_training[list_lower]))
  
  mean_acc_testing_upper_t20 <- mean(Return.cumulative(eod_ret_testing[list_upper]))
  mean_acc_testing_lower_t20 <- mean(Return.cumulative(eod_ret_testing[list_lower]))
  
  mean_training_upper_t20 <- sum(eod_ret_training[list_upper])/(length(eod_ret_training[list_upper])*count(eod_ret_training[list_upper]))
  mean_training_lower_t20 <- sum(eod_ret_training[list_lower])/(length(eod_ret_training[list_lower])*count(eod_ret_training[list_lower]))
  
  mean_testing_upper_t20 <- sum(eod_ret_testing[list_upper])/(length(eod_ret_testing[list_upper])*count(eod_ret_testing[list_upper]))
  mean_testing_lower_t20 <- sum(eod_ret_testing[list_lower])/(length(eod_ret_testing[list_lower])*count(eod_ret_testing[list_lower]))
  
  mean(all_r)
  
  #enable for negative
  opt_p<-optimize.portfolio(R=Ra_training,portfolio=pspec,optimize_method = 'ROI')
  opt_w<-opt_p$weights
  #opt_w[(length(list_upper)+1):(length(list_lower)+length(list_upper))]<-negative/length(list_lower)
  
  upper_profile_training<-(data.frame(stack(eod_ret_training[list_upper]))$values)
  lower_profile_training<-(data.frame(stack(eod_ret_training[list_lower]))$values)
  
  upper_profile_testing<-(data.frame(stack(eod_ret_testing[list_upper]))$values)
  lower_profile_testing<-(data.frame(stack(eod_ret_testing[list_lower]))$values)
  
  #Return Probability Lower Training
  jpeg(paste0("plots/",end_date,"_lower_training_retProbPlot.jpg"))
  profile<-c()
  profile<-quantile(lower_profile_training,probs, na.rm =T, names = F, type = 7)
  x = probs; y = profile
  plot(x=probs,y=profile,type="o",xlab="Return Probability",ylab="lower training")
  text(x, y, paste(round(x, 2), round(y, 3), sep=", "), cex=1.1) # for (x, y)  
  dev.off()
  
  #... testing
  jpeg(paste0("plots/",end_date,"_lower_testing_retProbPlot.jpg"))
  profile<-c()
  profile<-quantile(lower_profile_testing,probs, na.rm =T, names = F, type = 7)
  x = probs; y = profile
  plot(x=probs,y=profile,type="o",xlab="Return Probability",ylab="lower testing")
  text(x, y, paste(round(x, 2), round(y, 3), sep=", "), cex=1.1) # for (x, y)  
  dev.off()
  
  #... upper training
  jpeg(paste0("plots/",end_date,"_upper_training_retProbPlot.jpg"))
  profile<-c()
  profile<-quantile(upper_profile_training,probs, na.rm =T, names = F, type = 7)
  x = probs; y = profile
  plot(x=probs,y=profile,type="o",xlab="Return Probability",ylab="upper training")
  text(x, y, paste(round(x, 2), round(y, 3), sep=", "), cex=1.1) # for (x, y)  
  dev.off()
  
  #... testing
  jpeg(paste0("plots/",end_date,"_upper_testing_retProbPlot.jpg"))
  profile<-c()
  profile<-quantile(upper_profile_testing,probs, na.rm =T, names = F, type = 7)
  x = probs; y = profile
  plot(x=probs,y=profile,type="o",xlab="Return Probability",ylab="upper testing")
  text(x, y, paste(round(x, 2), round(y, 3), sep=", "), cex=1.1) # for (x, y)
  dev.off()
  
  #Density plot all returns - SP500TR
  jpeg(paste0("plots/",end_date,"_allDensPlot.jpg"))
  d <- density(all_r,na.rm=T)
  plot(d)
  dev.off()
  
  #density plot returns training upper
  jpeg(paste0("plots/",end_date,"_upper_training_densPlot.jpg"))
  d <- density(upper_profile_training,na.rm=T)
  plot(d)
  dev.off()
  
  # ...testing upper
  jpeg(paste0("plots/",end_date,"_upper_testing_densPlot.jpg"))
  d <- density(upper_profile_testing,na.rm=T)
  plot(d)
  dev.off()
  
  # ...lower training
  jpeg(paste0("plots/",end_date,"_lower_training_densPlot.jpg"))
  d <- density(lower_profile_training,na.rm=T)
  plot(d)
  dev.off()
  
  #... lower testing
  jpeg(paste0("plots/",end_date,"_lower_testing_densPlot.jpg"))
  d <- density(lower_profile_testing,na.rm=T)
  plot(d)
  dev.off()
  
  #Return probability Plot SP500TR training
  jpeg(paste0("plots/",end_date,"_SP500TR_training_retProbPlot.jpg"))
  profile<-c()
  profile<-quantile(eod_ret_training$SP500TR,probs, na.rm =T, names = F, type = 7)
  x = probs; y = profile
  plot(x=probs,y=profile,type="o",xlab="Return Probability",ylab="SP500TR training")
  text(x, y, paste(round(x, 2), round(y, 3), sep=", "), cex=1.1) # for (x, y)
  dev.off()
  
  #... testing
  jpeg(paste0("plots/",end_date,"_SP500TR_testing_retProbPlot.jpg"))
  profile<-c()
  profile<-quantile(eod_ret_testing$SP500TR,probs, na.rm =T, names = F, type = 7)
  x = probs; y = profile
  plot(x=probs,y=profile,type="o",xlab="Return Probability",ylab="SP500TR testing")
  text(x, y, paste(round(x, 2), round(y, 3), sep=", "), cex=1.1) # for (x, y)
  dev.off()
  
  #boxplot Returns Training SP500TR
  jpeg(paste0("plots/",end_date,"_SP500TR_training_retBoxPlot.jpg"))
  boxplot(eod_ret_training$SP500TR,horizontal=1,xlab="SP500TR training")
  dev.off()
  
  #... testing SP500TR
  jpeg(paste0("plots/",end_date,"_SP500TR_testing_retBoxPlot.jpg"))
  boxplot(eod_ret_testing$SP500TR,horizontal=1,xlab="SP500TR testing")
  dev.off()
  
  #Return Density Plot training SP500TR
  jpeg(paste0("plots/",end_date,"_SP500TR_training_retDensPlot.jpg"))
  d <- density(eod_ret_training$SP500TR,na.rm=T)
  plot(d,ylab=name)
  dev.off()
  
  #Return Density Plot testing SP500TR
  jpeg(paste0("plots/",end_date,"_SP500TR_testing_retDensPlot.jpg"))
  d <- density(eod_ret_testing$SP500TR,na.rm=T)
  plot(d,ylab=name)
  dev.off()
  
  #Cumulative Return SP500TR Training
  jpeg(paste0("plots/",end_date,"_SP500TR_retTrainingPlot.jpg"))
  d<-chart.CumReturns(as.xts(eod_ret_training['SP500TR',drop=F]))
  plot(d)
  dev.off()
  
  #...testing
  jpeg(paste0("plots/",end_date,"_SP500TR_retTestingPlot.jpg"))
  d<-chart.CumReturns(as.xts(eod_ret_testing['SP500TR',drop=F]))
  plot(d)
  dev.off()
  
  #buffer flush for any pending writes  
  #for(i in 1:length(dev.list())) { dev.off() }
  
  combinedOpt=(opt_w+opt_wpw)/2
  
  sum(combinedOpt)
  
  #just beta's
  Rp$ptf<-c()
  Rp$ptf<-Ra_testing %*% opt_w
  RpW$ptf<-RaW_testing %*% opt_w
  RpM$ptf<-RaM_testing %*% opt_w
  
  print (paste("Beta: CumRet", "tr_t20 te_t20 tr_b20 te_b20", round(mean_acc_training_beta_t20,3) , round(mean_acc_testing_beta_t20,3) , round(mean_acc_training_beta_b20,3), round(mean_acc_testing_beta_b20,3)))
  
  #only use with negative weights
  print(paste("Markowitz Profile ret", round(Return.cumulative(Rp$ptf),3)))
  
  #zip up reports
  
  command <- paste0("convert -delay 100 -loop 0 plots/*lot.jpg ","plots/",end_date,"_aggregate_Plots.gif")
  system(command, intern = TRUE)
  #when using erase, states command not found
  command <- paste0("mv ","plots/",end_date,"_aggregate_Plots.gif ","plots/",end_date,"_aggregate_Plots.bak")
  system(command, intern = TRUE)
  command <- paste0("rm ","plots/*Plot.jpg -f")
  system(command, intern = TRUE)
  
  
  command <- paste0("7z a plots/",end_date,"_ReturnAggregates.zip plots/*.csv")
  system(command, intern = TRUE)
  #when using erase, states command not found
  command <- paste0("rm ","plots/*.csv -f")
  system(command, intern = TRUE)
  
  command <- paste0("7z a plots/",end_date,"_ReturnUpperLowerHighlights.zip plots/*.gif")
  system(command, intern = TRUE)
  #when using erase, states command not found
  command <- paste0("rm ","plots/*.gif -f")
  system(command, intern = TRUE)
  command <- paste0("cp plots/",end_date,"_aggregate_Plots.bak plots/",end_date,"_aggregate_Plots.gif")
  system(command, intern = TRUE)
  #command <- paste0("rm plots/*.bak -f")
  #system(command, intern = TRUE)
  
  
}
command <- paste0("cp plots/*.bak plots/*.gif")
system(command, intern = TRUE)
#command <- paste0("rm -f plots/*.bak")
#system(command, intern = TRUE)
