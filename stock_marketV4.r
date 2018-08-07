# Part 3c

# Stock Market Case in R
rm(list=ls(all=T)) # this just removes everything from memory

# Load CSV Files ----------------------------------------------------------

# Load daily prices from CSV - no parameters needed
dp<-read.csv('C:/Temp/daily_prices_2012_2017.csv') # no arguments

#Explore
head(dp) #first few rows
tail(dp) #last few rows
nrow(dp) #row count

#This is an easy way (csv) but we are not going to use it here
rm(dp) # remove from memory
#We are going to perform most of the transformation tasks in R

# Connect to PostgreSQL ---------------------------------------------------

# Make sure you have created the reader role for our PostgreSQL database
# and granted that role SELECT rights to all tables
# Also, make sure that you have completed (or restored) Part 3b db

require(RPostgreSQL) # did you install this package?
require(DBI)
pg = dbDriver("PostgreSQL")
conn = dbConnect(drv=pg
                 ,user="stockmarketreader"
                 ,password="read123"
                 ,host="localhost"
                 ,port=5432
                 ,dbname="stockmarket"
)

#custom calendar
qry='SELECT * FROM custom_calendar ORDER by date'
ccal<-dbGetQuery(conn,qry)
#eod prices and indices
qry1="SELECT symbol,date,adj_close FROM eod_indices WHERE date BETWEEN '2011-12-30' AND '2017-12-31'"
qry2="SELECT ticker,date,adj_close FROM eod_quotes WHERE date BETWEEN '2011-12-30' AND '2017-12-31'"
eod<-dbGetQuery(conn,paste(qry1,'UNION',qry2))
dbDisconnect(conn)

#Explore
head(ccal)
tail(ccal)
nrow(ccal)

head(eod)
tail(eod)
nrow(eod)

head(eod[which(eod$symbol=='SP500TR'),])

#For monthly we may need one more data item (for 2011-12-30)
#We can add it to the database (INSERT INTO) - but to practice: reinserts each eod run which is not tied directly with the db.
eod_row<-data.frame(symbol='SP500TR',date=as.Date('2011-12-30'),adj_close=2158.94)

eod<-rbind(eod,eod_row)
tail(eod)

# Use Calendar --------------------------------------------------------

tdays<-ccal[which(ccal$trading==1 & ccal$date >= '2011-12-30' & ccal$date <='2017-12-31'),,drop=F]
wdays<-tdays[which(tdays$dow=="Fri"),,drop=F]
mdays<-tdays[which(tdays$eom==1),,drop=F]
head(tdays)
nrow(tdays)-1

# Completeness ----------------------------------------------------------
# Percentage of completeness

#pct<-table(eod$symbol)/(nrow(tdays)-1)
pct<-table(eod$symbol)/max(table(eod$symbol))
selected_symbols_daily<-names(pct)[which(pct>=0.98)]
length(selected_symbols_daily)

eod_complete<-eod[which(eod$symbol %in% selected_symbols_daily),,drop=F]

#temp<-c()
#temp<-eod_complete$date
#typeof(temp)
#eod_complete[which(eod_complete$date=="2017-12-31")]

#=='2017-12-30']
#data.frame(eod_complete)[date>=as.Date('2017-12-29') & date<=as.Date('2017-12-31')]

#check
head(eod_complete)
tail(eod_complete)
nrow(eod_complete)

#YOUR TURN: perform all these operations for monthly data
#Create eom and eom_complete
#Hint: which(ccal$trading==1 & ccal$eom==1)

# Transform (Pivot) -------------------------------------------------------

require(reshape2) #did you install this package?
eod_pvt<-dcast(eod_complete, date ~ symbol,value.var='adj_close',fun.aggregate = mean, fill=NULL)
#check
eod_pvt[1:10,1:5] #first 10 rows and first 5 columns 
ncol(eod_pvt) # column count
nrow(eod_pvt)
#table(is.na(eod_pvt))
# YOUR TURN: Perform the same set of tasks for monthly prices (create eom_pvt)

#tail(eod_pvt[,1:2])

# Merge with Calendar -----------------------------------------------------
eod_pvt_complete<-merge.data.frame(x=tdays[,'date',drop=F],y=eod_pvt,by='date',all.x=T)
eow_pvt_complete<-merge.data.frame(x=wdays[,'date',drop=F],y=eod_pvt,by='date',all.x=T)
eom_pvt_complete<-merge.data.frame(x=mdays[,'date',drop=F],y=eod_pvt,by='date',all.x=T)

tail(eom_pvt_complete[,1:2]) #cuts off 2017-12-29, but re-appears if I rerun it, but eom_ret didn't have it before I reran it... which tells me it changes inbetween here and eom_ret
#eom_pvt_complete[which(eom_pvt_complete$date=)]

#which[eom_pvt_complete$date=='2017-12-29']
#check
#eod_pvt_complete[1:10,1:5] #first 10 rows and first 5 columns 
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
tail(eom_pvt_complete[,1:2]) #cuts off 2017-12-29...
eod_pvt_complete[1:10,1:5] #first 10 rows and first 5 columns 
#ncol(eod_pvt_complete)
#nrow(eod_pvt_complete)
table(is.na(eod_pvt_complete))
# Missing Data Imputation -----------------------------------------------------
# We can replace a few missing (NA or NaN) data items with previous data
# Let's say no more than 3 in a row...
require(zoo)
eod_pvt_complete<-na.locf(eod_pvt_complete,na.rm=T,fromLast=F,maxgap=3)
eow_pvt_complete<-na.locf(eow_pvt_complete,na.rm=T,fromLast=F,maxgap=3)
eom_pvt_complete<-na.locf(eom_pvt_complete,na.rm=T,fromLast=F,maxgap=3)

#cut it here, see the true#  noo!!!

#re-check
tail(eom_pvt_complete[,1:2]) #cuts off 2017-12-29...
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
tail(eom_ret[,1:2])

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
selected_symbols_daily<-names(max_daily_ret)[which(max_daily_ret<=1.00)]
selected_symbols_monthly=selected_symbols_daily
selected_symbols_weekly=selected_symbols_daily
length(selected_symbols_daily)

#subset eod_ret
eod_ret<-eod_ret[,which(colnames(eod_ret) %in% selected_symbols_daily)]
eow_ret<-eow_ret[,which(colnames(eow_ret) %in% selected_symbols_daily)]
eom_ret<-eom_ret[,which(colnames(eom_ret) %in% selected_symbols_daily)]

(eod_ret[!complete.cases(eod_pvt), ][1:5])

#check
tail(eom_ret[,1:2])
eod_ret[1:10,1:5] #first 10 rows and first 5 columns 
ncol(eod_ret)
nrow(eod_ret)

#YOUR TURN: subset eom_ret data

# Export data from R to CSV -----------------------------------------------
#write.csv(eod_ret,'C:/Temp/eod_ret.csv')

# You can actually open this file in Excel!


# Tabular Return Data Analytics -------------------------------------------

# We will select 'SP500TR' and c('AEGN','AAON','AMSC','ALCO','AGNC','AREX','ABCB','ABMD','ACTG','ADTN','AAPL','AAL')
# We need to convert data frames to xts (extensible time series)
source("C:/Users/user/Documents/alphaAdvantageApi/stockmarketR/stockmarketRCode/colSortAndFilter.R")

CR_Ra_training <- colSortMax(Return.cumulative(Ra_training))
avg_Ra_training <- colSortAvg(Ra_training)

CR_RaW_training <- colSortMax(Return.cumulative(RaW_training))
avg_RaW_training <- colSortAvg(RaW_training)

CR_RaM_training <- colSortMax(Return.cumulative(RaM_training))
avg_RaM_training <- colSortAvg(RaM_training)

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
t20CR_Ra<-colnames(data.frame(eod_ret)[CR_Ra_training$colname])[1:20]
t20CR_RaW<-colnames(data.frame(eow_ret)[CR_RaW_training$colname])[1:20]
t20CR_RaM<-colnames(data.frame(eom_ret)[CR_RaM_training$colname])[1:20]

b20CR_Ra<-colnames(data.frame(eod_ret)[CR_Ra_training$colname])[(length(CR_Ra_training$colname)-20):length(CR_Ra_training$colname)]
b20CR_RaW<-colnames(data.frame(eow_ret)[CR_RaW_training$colname])[(length(CR_RaW_training$colname)-20):length(CR_RaW_training$colname)]
b20CR_RaM<-colnames(data.frame(eom_ret)[CR_RaM_training$colname])[(length(CR_RaM_training$colname)-20):length(CR_RaM_training$colname)]

#top 20 by average return
#I should only be feeding in one variable to avoid input errors, which means I need to refactor this code.
t20AVGR_Ra<-colnames(data.frame(eod_ret)[avg_Ra_training$colname])[1:20]
t20AVGR_RaW<-colnames(data.frame(eow_ret)[avg_RaW_training$colname])[1:20]
t20AVGR_RaM<-colnames(data.frame(eom_ret)[avg_RaM_training$colname])[1:20]

#bottom 20
b20AVGR_Ra<-colnames(data.frame(eod_ret)[avg_Ra_training$colname])[(length(avg_Ra_training$colname)-20):length(avg_Ra_training$colname)]
b20AVGR_RaW<-colnames(data.frame(eow_ret)[avg_RaW_training$colname])[(length(avg_RaW_training$colname)-20):length(avg_RaW_training$colname)]
b20AVGR_RaM<-colnames(data.frame(eom_ret)[avg_RaM_training$colname])[(length(avg_RaM_training$colname)-20):length(avg_RaM_training$colname)]
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

# Graphical Return Data Analytics -----------------------------------------

# Cumulative returns chart
chart.CumReturns(Ra,legend.loc = 'topleft')
chart.CumReturns(Rb,legend.loc = 'topleft')

#Box plots
chart.Boxplot(cbind(head(Rb,-63),head(Ra,-63)))

chart.Drawdown(Ra,legend.loc = 'bottomleft')

# YOUR TURN: try other charts

# MV Portfolio Optimization -----------------------------------------------

# withold the last 252 trading days
Ra_training<-head(Ra,-63)
Rb_training<-head(Rb,-63)

#all but 13 weeks
RaW_training<-head(RaW,-13)
RbW_training<-head(RbW,-13)

#all but 3 months
RaM_training<-head(RaM,-3)
RbM_training<-head(RbM,-3)

# use the last 21 trading days for testing
Ra_testing<-tail(Ra,63)
Rb_testing<-tail(Rb,63)

# use last 13 weeks
RaW_testing<-tail(RaW,13)
RbW_testing<-tail(RbW,13)

# use last 3 months
RaM_testing<-tail(RaM,3)
RbM_testing<-tail(RbM,3)

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

#positve/negative weights
positive=2
negative=-1

opt_w[1:length(t20Mix_Ra)]<-positive/length(t20Mix_Ra)
opt_w[(length(t20Mix_Ra)+1):(length(t20Mix_Ra)+length(b20Mix_Ra))]<-negative/length(b20Mix_Ra)

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

# Chart Hypothetical Portfolio Returns ------------------------------------

chart.CumReturns(Rp,legend.loc = 'topleft')
chart.CumReturns(RpW,legend.loc = 'topleft')
chart.CumReturns(RpM,legend.loc = 'topleft')

# End of Part 3c
# End of Stock Market Case Study 