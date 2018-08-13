
#dp<-read.csv('d:/quantshare/quotes.csv') # no arguments

todayIs <- as.Date(as.POSIXlt(as.Date(Sys.Date())))

require(RPostgreSQL) # did you install this package?
require(DBI)
pg = dbDriver("PostgreSQL")
conn = dbConnect(drv=pg
                 ,user="readyloop"
                 ,password="read123"
                 ,host="localhost"
                 ,port=5432
                 ,dbname="readyloop"
)

#custom calendar
qry='SELECT * FROM custom_calendar ORDER by date'
ccal<-dbGetQuery(conn,qry)
#eod prices and indices
#qry1="SELECT symbol,date,adjusted_close FROM eod_indices WHERE date BETWEEN '2011-12-30' AND '2017-12-31'"
qry1=paste0("SELECT symbol,date,adj_close FROM eod_indices WHERE date BETWEEN '1999-12-30' AND '",todayIs,"'")
#qry2=paste0("SELECT symbol,timestamp,adjusted_close FROM nasdaq_facts WHERE timestamp BETWEEN '",start_date,"' AND '",end_date,"'")
qry2=paste0("SELECT symbol,timestamp,adjusted_close FROM etf_bond_facts WHERE timestamp BETWEEN '1999-12-30' AND '",todayIs,"'")
qry3=paste0("SELECT symbol,timestamp,adjusted_close FROM nasdaq_facts WHERE timestamp BETWEEN '1999-12-30' AND '",todayIs,"'")
qry4=paste0("SELECT symbol,timestamp,adjusted_close FROM other_facts WHERE timestamp BETWEEN '1999-12-30' AND '",todayIs,"'")
eodwNA<-dbGetQuery(conn,paste(qry1,'UNION',qry2,'UNION',qry3,'UNION',qry4))
dbDisconnect(conn)

eodOutside<-na.omit(eodwNA)

#scores<-c()
iterator=0
for (iterator in seq(0, 151, by=30))
{
  
  #set # of years back here.
  library(mondate)
  end_date<-as.Date(mondate(as.Date(todayIs)) - iterator)
  
  start_date<- as.Date(mondate(end_date)-60)
  
  days=252/4
  weeks=52/4
  months=12/4
  
  eod<-eodOutside[which(eodOutside$date>=start_date & eodOutside$date <= end_date),,drop=F]
  
  
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
  head(ccal)
  tail(ccal)
  nrow(ccal)
  
  head(eod)
  tail(eod)
  nrow(eod)
  
  head(eod[which(eod$symbol=='SP500TR'),])
  
  tail(eod)
  
  # Use Calendar --------------------------------------------------------
  
  tdays<-ccal[which(ccal$trading==1 & ccal$date >= start_date & ccal$date <=end_date),,drop=F]
  wdays<-tdays[which(tdays$dow=="Fri"),,drop=F]
  mdays<-tdays[which(tdays$eom==1),,drop=F]
  head(tdays)
  nrow(tdays)-1
  
  # Completeness ----------------------------------------------------------
  # Percentage of completeness
  
  table(eod$symbol)
  
  
  #filter
  #testing<-eod[which(eod$symbol=='YLCO'),,]
  #eom_ret
  #View(testing)
  
  pct<-table(eod$symbol)/(nrow(tdays)-1)
  #tail(pct,50)
  #pct<-table(eod$symbol)/max(table(eod$symbol))
  selected_symbols_daily<-names(pct)[which(pct>=0.99)]
  length(selected_symbols_daily)
  
  eod_completewNA<-eod[which(eod$symbol %in% selected_symbols_daily),,drop=F]
  
  
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
  
  require(reshape2) #did you install this package?
  eod_pvtwNA<-dcast(eod_completewNA, date ~ symbol,value.var='adj_close',fun.aggregate = mean, fill=NULL)
  #check
  #View((eod_pvt[c('date','SP500TR')])) #first 10 rows and first 5 columns 
  #View((eod_ret[c('SP500TR')]))
  ncol(eod_pvtwNA) # column count
  nrow(eod_pvtwNA)
  
  #still a problem, 451/809
  #table(is.na(eod_pvt$YLCO))
  
  #https://sebastiansauer.github.io/sum-isna/
  pct2<-1-(sapply(eod_pvtwNA, function(x) sum(is.na(x)))/nrow(eod_pvtwNA))
  
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
  
  
  #eod_complete<-eod[which(eod$symbol %in% selected_symbols_daily2),,drop=F]  
  
  
  #table(is.na(eod_pvt))
  # YOUR TURN: Perform the same set of tasks for monthly prices (create eom_pvt)
  
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
  eod_pvt_complete<-na.locf(eod_pvt_complete,na.rm=F,fromLast=F,maxgap=3)
  eow_pvt_complete<-na.locf(eow_pvt_complete,na.rm=F,fromLast=F,maxgap=3)
  eom_pvt_complete<-na.locf(eom_pvt_complete,na.rm=F,fromLast=F,maxgap=3)
  #View(((eod_pvt_complete[c('SP500TR')])))
  #cut it here, see the true#  noo!!!
  
  #re-check, if years is set -3, then agt has 364 non na's, but 808 na's... something is getting past the filter here.
  table(is.na(data.frame(eod_pvt_complete$AGT)))
  
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
  length(selected_symbols_daily)
  
  #subset eod_ret
  eod_ret<-eod_ret[,which(colnames(eod_ret) %in% selected_symbols_daily3)]
  eow_ret<-eow_ret[,which(colnames(eow_ret) %in% selected_symbols_daily3)]
  eom_ret<-eom_ret[,which(colnames(eom_ret) %in% selected_symbols_daily3)]
  
  (eod_ret[!complete.cases(eod_pvt), ][1:5])
  
  #check
  tail(eom_ret[,1:2])
  eod_ret[1:10,1:5] #first 10 rows and first 5 columns 
  ncol(eod_ret)
  nrow(eod_ret)
  
  #YOUR TURN: subset eom_ret data
  
  # Export data from R to CSV -----------------------------------------------
  #write.csv(eod_ret,paste0('C:/Test/',end_year,'eod_ret.csv')
  #write.csv(eom_ret,paste0('C:/Test/',end_year,'eom_ret.csv')
  
  # You can actually open this file in Excel!
  
  
  # Tabular Return Data Analytics -------------------------------------------
  
  # We will select 'SP500TR' and c('AEGN','AAON','AMSC','ALCO','AGNC','AREX','ABCB','ABMD','ACTG','ADTN','AAPL','AAL')
  # We need to convert data frames to xts (extensible time series)
  source("C:/Users/user/Documents/alphaAdvantageApi/stockmarketR/stockmarketRCode/colSortAndFilter.R")
  
  eod_ret_training<-head(eod_ret,-days)
  #View(eod_ret_training[,1:4])
  #View(eod_ret_testing[,1:4])
  eow_ret_training<-head(eow_ret,-weeks)
  eom_ret_training<-head(eom_ret,-months)
  
  eod_ret_testing<-tail(eod_ret,days)
  eow_ret_testing<-tail(eow_ret,weeks)
  eom_ret_testing<-tail(eom_ret,months)
  
  CR_Ra_training <- colSortMax(Return.cumulative(eod_ret_training))
  avg_Ra_training <- colSortAvg(eod_ret_training)
  
  CR_RaW_training <- colSortMax(Return.cumulative(eow_ret_training))
  avg_RaW_training <- colSortAvg(eow_ret_training)
  
  CR_RaM_training <- colSortMax(Return.cumulative(eom_ret_training))
  avg_RaM_training <- colSortAvg(eom_ret_training)
  
  #top 20 by cumulative return
  t20CR_Ra<-c()
  t20CR_RaW<-c()
  t20CR_RaM<-c()
  
  t20AVGR_Ra<-c()
  t20AVGR_RaW<-c()
  t20AVGR_RaM<-c()
  
  #bottom 20 
  b20CR_Ra<-c()
  b20CR_RaW<-c()
  b20CR_RaM<-c()
  
  b20AVGR_Ra<-c()
  b20AVGR_RaW<-c()
  b20AVGR_RaM<-c()
  
  #bottom 20 
  
  #chart.Boxplot(eod_ret[t20CR])
  setPercent=round(length(colnames(eod_pvt_complete))*.025,0)
  
  t20CR_Ra<-colnames(data.frame(eod_ret)[CR_Ra_training$colname])[1:setPercent]
  t20CR_RaW<-colnames(data.frame(eow_ret)[CR_RaW_training$colname])[1:setPercent]
  t20CR_RaM<-colnames(data.frame(eom_ret)[CR_RaM_training$colname])[1:setPercent]
  
  b20CR_Ra<-colnames(data.frame(eod_ret)[CR_Ra_training$colname])[(length(CR_Ra_training$colname)-setPercent):length(CR_Ra_training$colname)]
  b20CR_RaW<-colnames(data.frame(eow_ret)[CR_RaW_training$colname])[(length(CR_RaW_training$colname)-setPercent):length(CR_RaW_training$colname)]
  b20CR_RaM<-colnames(data.frame(eom_ret)[CR_RaM_training$colname])[(length(CR_RaM_training$colname)-setPercent):length(CR_RaM_training$colname)]
  
  #top 20 by average return
  #I should only be feeding in one variable to avoid input errors, which means I need to refactor this code.
  t20AVGR_Ra<-colnames(data.frame(eod_ret)[avg_Ra_training$colname])[1:setPercent]
  t20AVGR_RaW<-colnames(data.frame(eow_ret)[avg_RaW_training$colname])[1:setPercent]
  t20AVGR_RaM<-colnames(data.frame(eom_ret)[avg_RaM_training$colname])[1:setPercent]
  
  #bottom 20
  b20AVGR_Ra<-colnames(data.frame(eod_ret)[avg_Ra_training$colname])[(length(avg_Ra_training$colname)-setPercent):length(avg_Ra_training$colname)]
  b20AVGR_RaW<-colnames(data.frame(eow_ret)[avg_RaW_training$colname])[(length(avg_RaW_training$colname)-setPercent):length(avg_RaW_training$colname)]
  b20AVGR_RaM<-colnames(data.frame(eom_ret)[avg_RaM_training$colname])[(length(avg_RaM_training$colname)-setPercent):length(avg_RaM_training$colname)]
  #export dataframe in the order specified in the summary
  
  t20Mix_Ra<-unique(c(t20CR_Ra,t20AVGR_Ra))
  t20Mix_RaW<-unique(c(t20CR_RaW,t20AVGR_RaW))
  t20Mix_RaM<-unique(c(t20CR_RaM,t20AVGR_RaM))
  
  b20Mix_Ra<-unique(c(b20CR_Ra,b20AVGR_Ra))
  b20Mix_RaW<-unique(c(b20CR_RaW,b20AVGR_RaW))
  b20Mix_RaM<-unique(c(b20CR_RaM,b20AVGR_RaM))
  
  list_Ra<-c(t20Mix_Ra,b20Mix_Ra)
  
  list_RaW<-c(t20Mix_RaW,b20Mix_RaW)
  list_RaM<-c(t20Mix_RaM,b20Mix_RaM)
  
  #Ra<-as.xts(eod_ret[,c('AEGN','AAON','AMSC','ALCO','AGNC','AREX','ABCB','ABMD','ACTG','ADTN','AAPL','AAL'),drop=F])
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
  Ra_training<-head(Ra,-days)
  Rb_training<-head(Rb,-days)
  
  #all but 13 weeks
  RaW_training<-head(RaW,-weeks)
  RbW_training<-head(RbW,-weeks)
  
  #all but 3 months
  RaM_training<-head(RaM,-months)
  RbM_training<-head(RbM,-months)
  
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
  
  #list stuff
  
  acc_Ra_training<-Return.cumulative(Ra_training)
  
  acc_Ra_testing<-Return.cumulative(Ra_testing)
  
  #all returns in one list (for classification)
  
  #allr<-eod_ret[colnames(eod_complete)]
  
  #allr[is.na(allr)] <- 0
  
  #training
  ratr<-data.frame(stack(((data.frame(Ra_training)))))$values
  brtr<-data.frame(stack(((data.frame(Rb_training)))))$values
  
  #testing
  rate<-data.frame(stack(((data.frame(Ra_testing)))))$values
  brte<-data.frame(stack(((data.frame(Rb_testing)))))$values
  
  alltr<-(c(ratr,rate))
  summary(alltr)
  
  length(rate)
  length(ratr)
  
  length(rate) +length(ratr)
  
  length(alltr)
  
  #Ra[is.na(Ra)] <- 0
  #RaW[is.na(RaW)] <- 0
  #RaM[is.na(RaM)] <- 0
  
  #returns assets training
    # IQR used for rounding determination and subsequent classification of returns
    
  chart.Boxplot(alltr)
    quantile(alltr)
    summary(alltr)
    write.csv(alltr,"c:/test/alltr.csv")
    
    length(alltr)
    
    
    IQR=quantile(alltr)[3]-quantile(alltr)[2]
    Lhinge<-c()
    Lhinge=quantile(alltr)[2]-IQR*1.5
    Uhinge<-c()
    Uhinge=quantile(alltr)[4]+IQR*1.5
    IQRRange=Uhinge-Lhinge
    
    HingeRange=Uhinge-Lhinge
    
    #percent captured within LHinge and UHinge of Ra_training
    length(ratr[which(alltr[]>=Lhinge & alltr[]<=Uhinge)])/length(alltr)
    
    
    Lhinge+IQR*1/8
  
    breaks=c(quantile(alltr)[1],Lhinge,Lhinge+HingeRange*1/8,Lhinge+HingeRange*2/8,Lhinge+HingeRange*3/8,Lhinge+HingeRange*4/8,Lhinge+HingeRange*5/8,Lhinge+HingeRange*6/8,Lhinge+HingeRange*7/8,Uhinge,quantile(alltr)[5])  

    hist(ratr,breaks)

    hist(alltr,breaks)
    
  
  #returns benchmark training
  chart.Boxplot(brtr)
  
  #returns assets
  
  #example, outliers are defined as 1Q-IQR*1.5 and 3Q*+IQR*1.5, why not set up a histogram that captures this.
  
  
  hcr<-data.frame(stack(((data.frame(Ra_training)[t20Mix_Ra]))))$values
    hist(hcr,breaks)
    boxplot(hcr)
  
  
    lcr<-data.frame(stack(((data.frame(Ra_training)[b20Mix_Ra]))))$values
    hist(lcr)
    hist(hcr, breaks)
    
    mean(hcr)
    mean(lcr)
  
  hcrT20Testing<-data.frame(stack(tail((data.frame(Ra_testing)[t20Mix_Ra]))))$values
  lcrT20Testing<-data.frame(stack(tail((data.frame(Ra_testing)[b20Mix_Ra]))))$values
  
  mean(hcrT20Testing)
  mean(lcrT20Testing)
  # Graphical Return Data Analytics -----------------------------------------
  
  #long portfolio
  hist(hcr,breaks)
  
  #short portfolio
  hist(lcr,breaks)
  
  #long testing
  hist(hcrT20Testing,breaks)
  
  #short testing
  hist(lcrT20Testing,breaks)
  
  
  # Cumulative returns chart
  chart.CumReturns(Ra,legend.loc = 'topleft')
  chart.CumReturns(Rb,legend.loc = 'topleft')
  
  #Box plots
  chart.Boxplot(cbind(Rb_training,Ra_training))
  chart.Boxplot(Rb_training)
  
  chart.Drawdown(Ra,legend.loc = 'bottomleft')
  chart.Drawdown(Ra_testing,legend.loc = 'bottomleft')
  
  boxplot(hcr,Rb_training,lcr)
  
  summary(hcr)
  StdDev(hcr)
  
  
  #S&P500
  summary(Rb_training)
  StdDev(Rb_training)
  
  summary(lcr)
  StdDev(lcr)
  
  #quantile(hcr,c(0,.05,.5,.95,1))
  sum(acc_Ra_training[,t20Mix_Ra])
  
  #quantile(lcr,c(0,.05,.5,.95,1))
  sum(acc_Ra_training[,b20Mix_Ra])
  
  #quantile(hcrT20Testing,c(0,.05,.5,.95,1))
  sum(acc_Ra_testing[,t20Mix_Ra])
  
  #quantile(lcrT20Testing,c(0,.05,.5,.95,1))
  sum(acc_Ra_testing[,b20Mix_Ra])
  
  
  #plot.new()
  
  #best/worst over testing period
  summary(hcrT20Testing)
  StdDev(hcrT20Testing)
  
  #S&P500
  summary(Rb_testing)
  StdDev(Rb_testing)
  
  summary(lcrT20Testing)
  StdDev(lcrT20Testing)
  
  boxplot(hcrT20Testing,Rb_testing,lcrT20Testing)
  
  
  t.test(hcr,lcr)
  t.test(hcrT20Testing,lcrT20Testing)
  ttest<-t.test(hcr,lcr)
  names(ttest)
  ttest$statistic
  
  
  summary(hcrT20Testing)
  summary(Rb_testing)
  summary(Rb_training)
  summary(lcrT20Testing)
  
  #optimize the MV (Markowitz 1950s) portfolio weights based on training
  table.AnnualizedReturns(Rb_training)
  mar<-mean(Rb_training) #we need daily minimum acceptabe return
  marW<-mean(RbW_training) #we need daily minimum acceptable return
  marM<-mean(RbM_training) #we need daily minimum acceptable return
  
  require(PortfolioAnalytics)
  require(ROI) # make sure to install it
  require(ROI.plugin.quadprog)  # make sure to install it
  pspec<-portfolio.spec(assets=colnames(Ra_training))
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
  
  length(t20Mix_Ra)
  length(b20Mix_Ra)
  
  length(t20Mix_RaW)
  length(b20Mix_RaW)
  
  length(t20Mix_RaM)
  length(b20Mix_RaM)
  
  for (weight in 2:5)
  {
    positive=weight
    negative=abs((positive)-1)*-1
    
    length(list_Ra)
    opt_w[1:length(t20Mix_Ra)]<-positive/length(t20Mix_Ra)
    #opt_w[1:length(t20Mix_Ra)]<-.5/length(t20Mix_Ra)
    #opt_w[1:length(list_Ra)]=1/length(list_Ra)
    opt_w[(length(t20Mix_Ra)+1):(length(t20Mix_Ra)+length(b20Mix_Ra))]<-negative/length(b20Mix_Ra)
    #opt_w[(length(t20Mix_Ra)+1):(length(t20Mix_Ra)+length(b20Mix_Ra))]<-.5/length(b20Mix_Ra)
    
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
    
    chart.CumReturns(Ra_training[,t20Mix_Ra])
    chart.CumReturns(Ra_training[,b20Mix_Ra])
    chart.CumReturns(Rb_training)
    chart.CumReturns(Ra_testing)
    
    chart.CumReturns(Rb_testing)
    plot(Rb_testing$SP500TR)
    
    #write.csv(Rb_testing,"c:/test/sp5.csv")
    #write.csv(Ra_testing,"c:/test/rat.csv")
    #write.csv(eod_ret[list_Ra],"c:/test/opt_Returns.csv")
    
    #check
    #View(Ra_testing[,1:2])
    
    # Chart Hypothetical Portfolio Returns ------------------------------------
    
    
    jpeg(paste0(end_date,"rplot.jpg"))
    chart.CumReturns(Rp,legend.loc = 'topleft')
    dev.off()
    chart.CumReturns(RpW,legend.loc = 'topleft')
    chart.CumReturns(RpM,legend.loc = 'topleft')
    #View(eom_ret[,list_Ra])
    
    #table(is.na(eom_ret[,list_Ra]))
    
    # End of Part 3c
    # End of Stock Market Case Study 
      print(paste("start: ", start_date, "end: ", end_date, "Weights", positive, "vs", negative, "The lag month is", iterator, "and the return is", Return.cumulative(Rp$ptf)))      
    #scores<-rbind(scores,Return.cumulative(Rp$ptf))
    
  }
  
}