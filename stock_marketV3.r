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
qry1="SELECT symbol,date,adj_close FROM eod_indices WHERE date BETWEEN '2012-12-31' AND '2018-03-27'"
qry2="SELECT ticker,date,adj_close FROM eod_quotes WHERE date BETWEEN '2012-12-31' AND '2018-03-27'"
eod<-dbGetQuery(conn,paste(qry1,'UNION',qry2))
testing<-dbGetQuery(conn,paste(qry))

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
#We can add it to the database (INSERT INTO) - but to practice:
#eod_row<-data.frame(symbol='SP500TR',date=as.Date('2011-12-30'),adj_close=2158.94) head(eod)
eod<-rbind(eod,eod_row)
tail(eod)

# Use Calendar --------------------------------------------------------
#I keep confusing completeness window
tdays<-ccal[which(ccal$trading==1 & ccal$date>='2012-12-31' & ccal$date<='2018-03-27'),,drop=F]
wdays<-tdays[which(tdays$dow=="Fri"),,drop=F]
mdays<-tdays[which(tdays$eom==1),,drop=F]
#check

nrow(tdays)-1
tail(tdays)

# Completeness ----------------------------------------------------------
# Percentage of completeness
max(table(eod$symbol))

#pct<-table(eod$symbol)/(nrow(tdays)-1) #max(pct) #account for day with -1
pct<-table(eod$symbol)/max(table(eod$symbol))
selected_symbols_daily<-names(pct)[which(pct>=0.97)]
length(selected_symbols_daily)
eod_complete<-eod[which(eod$symbol %in% selected_symbols_daily),,drop=F]

#check
head(eod_complete)
tail(eod_complete)
nrow(eod_complete)

#YOUR TURN: perform all these operations for monthly data
#Create eom and eom_complete
#Hint: which(ccal$trading==1 & ccal$eom==1)

# Transform (Pivot) -------------------------------------------------------

require(reshape2) #did you install this package?
eod_pvt<-dcast(eod_complete, date ~ symbol,value.var='adj_close',fun.aggregate = mean, fill=NULL) #no need for extra eod's
#check
tail(eod_pvt[,1:5]) #last (6) rows and first 5 columns
ncol(eod_pvt) # column count
nrow(eod_pvt)

# YOUR TURN: Perform the same set of tasks for monthly prices (create eom_pvt)

# Merge with Calendar -----------------------------------------------------
eod_pvt_complete<-merge.data.frame(x=tdays[,'date',drop=F],y=eod_pvt,by='date',all.x=T)
eom_pvt_complete<-merge.data.frame(x=mdays[,'date',drop=F],y=eod_pvt,by='date',all.x=T)
eow_pvt_complete<-merge.data.frame(x=wdays[,'date',drop=F],y=eod_pvt,by='date',all.x=T)

#check
eod_pvt_complete[1:10,1:5] #first 10 rows and first 5 columns
tail(eod_pvt_complete[,1:5]) #first 10 rows and first 5 columns 
ncol(eod_pvt_complete)
nrow(eod_pvt_complete)

#use dates as row names and remove the date column
rownames(eod_pvt_complete)<-eod_pvt_complete$date
rownames(eom_pvt_complete)<-eom_pvt_complete$date
rownames(eow_pvt_complete)<-eow_pvt_complete$date
eod_pvt_complete$date<-NULL
eom_pvt_complete$date<-NULL
eow_pvt_complete$date<-NULL

#re-check
eod_pvt_complete[1:10,1:5] #first 10 rows and first 5 columns 
ncol(eod_pvt_complete)
nrow(eod_pvt_complete)

# Missing Data Imputation -----------------------------------------------------
# We can replace a few missing (NA or NaN) data items with previous data
# Let's say no more than 3 in a row...
require(zoo)
eod_pvt_complete<-na.locf(eod_pvt_complete,na.rm=F,fromLast=T,maxgap=3)
eom_pvt_complete<-na.locf(eom_pvt_complete,na.rm=F,fromLast=T,maxgap=3)
eow_pvt_complete<-na.locf(eow_pvt_complete,na.rm=F,fromLast=T,maxgap=3)

#re-check
table(is.na(eod_pvt_complete))
eom_pvt_complete[1:10,1:5] #first 10 rows and first 5 columns 
ncol(eod_pvt_complete)
nrow(eod_pvt_complete)

# Calculating Returns -----------------------------------------------------
require(PerformanceAnalytics)
eod_ret<-CalculateReturns(eod_pvt_complete)
eom_ret<-CalculateReturns(eom_pvt_complete)
eow_ret<-CalculateReturns(eow_pvt_complete) 
head(eow_pvt_complete)[1:5]

#check
eod_ret[1:10,1:5] #first 10 rows and first 5 columns 
ncol(eod_ret)
nrow(eod_ret)

#remove the first row (need to do for daily, weekly, monthly to remove 2012-12-31)
eod_ret<-tail(eod_ret,-1) #use tail with a negative value
eom_ret<-tail(eom_ret,-1) #use tail with a negative value
eow_ret<-tail(eow_ret,-1) #use tail with a negative value
#check
eod_ret[1:10,1:5] #first 10 rows and first 5 columns 
tail(eod_ret[,1:2]) # last 6 rows, first 2 columns

ncol(eod_ret)
nrow(eod_ret)

# YOUR TURN: calculate eom_ret (monthly returns)

# Check for extreme returns -------------------------------------------
# There is colSums, colMeans but no colMax so we need to create it
colMax <- function(data) sapply(data, max, na.rm = TRUE)
# Apply it
max_daily_ret<-colMax(eod_ret)
max_monthly_ret<-colMax(eom_ret)
max_weekly_ret<-colMax(eow_ret)
max_daily_ret[1:10] #first 10 max returns
# And proceed just like we did with percentage (completeness)
selected_symbols_daily<-names(max_daily_ret)[which(max_daily_ret<=1.00)]
#selected_symbols_monthly<-names(max_monthly_ret)[which(max_monthly_ret<=1.00)]
#selected_symbols_weekly<-names(max_weekly_ret)[which(max_weekly_ret<=1.00)]
selected_symbols_monthly=selected_symbols_daily
selected_symbols_weekly=selected_symbols_daily
#x<-merge(selected_symbols_daily,selected_symbols_weekly, by.x=by, by=inserect(names(x), names(y)))
#x2<-selected_symbols_daily(x)


#subset eod_ret
eod_ret<-eod_ret[,which(colnames(eod_ret) %in% selected_symbols_daily)]
eow_ret<-eow_ret[,which(colnames(eow_ret) %in% selected_symbols_weekly)]
eom_ret<-eom_ret[,which(colnames(eom_ret) %in% selected_symbols_monthly)]

#check
eow_ret[1:10,1:5] #first 10 rows and first 5 columns 
tail(eod_ret[,1:2],2)
tail(eow_ret[,1:2],2)
head(eom_ret[,1:2])
tail(eom_ret[,1:2],2)

ncol(eod_ret)
nrow(eod_ret)

#YOUR TURN: subset eom_ret data

# Export data from R to CSV -----------------------------------------------
#write.csv(eod_ret,'C:/Test/eod_ret.csv')
#write.csv(eow_ret,'C:/Test/eow_ret.csv')
eom_ret['CLVS']
write.csv(eom_ret,'C:/Test/eom_ret.csv')
write.csv(eow_ret,'C:/Test/eow_ret.csv')
write.csv(eod_ret,'C:/Test/eod_ret.csv')



# You can actually open this file in Excel!


# Tabular Return Data Analytics -------------------------------------------

#symbolsChosen<-c('ABMD','ACAD','ALGN','ALNY','ANIP','ASCMA','AVGO','CALD','CLVS','CORT','CPST','EA','EGY','EXEL','FCSC','FOLD','GNC','GTT','HEAR','HK','HZNP','ICON','IMI','IMMU','INFI','INSY','KEG','LGND','LQDT','MCF','MU','NBIX','NFLX','NVDA','OREX','PFPT','PQ','PRTA','PTX','RAS','REXX','RTRX','SDRL','SHOS','SSI','STMP','TAL','TREE','TSLA','TTWO','UVE','VICL','VSI','VVUS','WLB'),drop=F])
# We will select 'SP500TR' and c('AEGN','AAON','AMSC','ALCO','AGNC','AREX','ABCB','ABMD','ACTG','ADTN','AAPL','AAL'),drop=F])
# We need to convert data frames to xts (extensible time series)
#Ra<-as.xts(eod_ret)
#RaM<-as.xts(eod_ret)
#RaW<-as.xts(eod_ret)
nonSlist<-c('TREE','TAL','NVDA','GTT','NFLX','ABMD','NBIX','TTWO','USCR','TSLA','AVGO','ANIP','NXST','ALGN','NTRI','GTN','STMP','LOV','PLUG','CORT','BCRX','BEAT','IMMU','SRPT','EXEL','NKTR','AMPE','XXII','RTRX','IDRA')
shortList<-c('GMO','EGY','AMRS','SHOS','VSI','HEAR','VICL','PRKR','CPST','SGY','VVUS','REXX','KEG','CYTX','RAS','FCSC','LQDT','GNC','ICON','PTX','SSI','OREX','BAS','HK')
list<-c(nonSlist,shortList)
Ra<-as.xts(eod_ret[,c(list),drop=F]) #based on top 4 and worst 4 avg performers of period.
RaW<-as.xts(eow_ret[,c(list),drop=F]) #based on top 4 and worst 4 avg performers of period.
RaM<-as.xts(eom_ret[,c(list),drop=F]) #based on top 4 and worst 4 avg performers of period.
Rb<-as.xts(eod_ret[,'SP500TR',drop=F]) #benchmark
RbM<-as.xts(eom_ret[,'SP500TR',drop=F]) #benchmark
RbW<-as.xts(eow_ret[,'SP500TR',drop=F]) #benchmark


#head(Ra)
#head(Rb)

# And now we can use the analytical package...

# Stats
#table.Stats(Ra)

# Distributions
#table.Distributions(Ra)

# Returns
#table.AnnualizedReturns(cbind(Rb,Ra),scale=252) # note for monthly use scale=12

# Accumulate Returns
acc_Ra<-Return.cumulative(Ra)
acc_RaM<-Return.cumulative(RaM)
acc_RaW<-Return.cumulative(RaW)
acc_Rb<-Return.cumulative(Rb)
acc_RbM<-Return.cumulative(RbM)
acc_RbW<-Return.cumulative(RbW)

# Capital Assets Pricing Model
#table.CAPM(Ra,Rb)

# YOUR TURN: try other tabular analyses

# Graphical Return Data Analytics -----------------------------------------

# Cumulative returns chart
#chart.CumReturns(Ra,legend.loc = 'topleft')
#chart.CumReturns(Rb,legend.loc = 'topleft')

#Box plots
#chart.Boxplot(cbind(Rb,Ra))

#chart.Drawdown(Ra,legend.loc = 'bottomleft')

# YOUR TURN: try other charts

# MV Portfolio Optimization -----------------------------------------------

# withold the last 5 trading days
Ra_training<-head(Ra,-5)

table(is.na(Ra_training))

table(is.na(RaM_training))

table(is.na(RaW_training))

head(Ra_training)[1:5,1:10]
tail(Ra_training)[,1:10]
Rb_training<-head(Rb,-5)
#all but 3 month
RaM_training<-head(RaM,-3)
RbM_training<-head(RbM,-3)
#all but 4 weeks
RaW_training<-head(RaW,-4)
RbW_training<-head(RbW,-4)

# use the last 5 trading days for testing
Ra_testing<-tail(Ra,5)
Rb_testing<-tail(Rb,5)
# use last 3 months
RaM_testing<-tail(RaM,3)
RbM_testing<-tail(RbM,3)
# use last 4 weeks
RaW_testing<-tail(RaW,4)
RbW_testing<-tail(RbW,4)




#optimize the MV (Markowitz 1950s) portfolio weights based on training
#table.AnnualizedReturns(Rb_training)
mar<-mean(Rb_training) #we need daily minimum acceptabe return
marM<-mean(RbM_training) #we need daily minimum acceptabe return
marW<-mean(RbW_training) #we need daily minimum acceptabe return

require(PortfolioAnalytics)
require(ROI) # make sure to install it
require(ROI.plugin.quadprog)  # make sure to install it
pspec<-portfolio.spec(assets=list)
pspec<-add.objective(portfolio=pspec,type="risk",name='StdDev')
pspec<-add.constraint(portfolio=pspec,type="full_investment")
pspec<-add.constraint(portfolio=pspec,type="return",return_target=mar)

pspecM<-portfolio.spec(assets=list)
pspecM<-add.objective(portfolio=pspecM,type="risk",name='StdDev')
pspecM<-add.constraint(portfolio=pspecM,type="full_investment")
pspecM<-add.constraint(portfolio=pspecM,type="return",return_target=marM)

pspecW<-portfolio.spec(assets=list)
pspecW<-add.objective(portfolio=pspecW,type="risk",name='StdDev')
pspecW<-add.constraint(portfolio=pspecW,type="full_investment")
pspecW<-add.constraint(portfolio=pspecW,type="return",return_target=marW)

#optimize portfolio
opt_p<-optimize.portfolio(R=Ra_training,portfolio=pspec,optimize_method = 'ROI')
opt_pW<-optimize.portfolio(R=RaW_training,portfolio=pspecW,optimize_method = 'ROI')
opt_p=opt_pW
opt_pM<-optimize.portfolio(R=RaM_training,portfolio=pspecM,optimize_method = 'ROI')

#extract weights
opt_w<-opt_p$weights
opt_wW<-opt_pW$weights
opt_wM<-opt_pM$weights

length(nonSlist)
length(shortList)
opt_w[1:length(nonSlist)]=2/length(nonSlist)
opt_w[(length(nonSlist)+1):(length(nonSlist)+length(shortList))]=(-1/length(shortList))
#opt_w[1:30]
#opt_w[31:54]
opt_wM<-opt_w
opt_wW<-opt_w
length(opt_w)

opt_w
sum(opt_w)
#opt_wM[1:6]=.6667
#opt_wM[7:12]=--3333
#opt_wW[1:6]=.6667
#opt_wW[7:12]=--3333
#opt_w[1:6]=.6667
#opt_w[7:12]=--3333
#opt_w[1:12]=.0833
#opt_wM[1:20]=.075
#opt_wM[21:40]=-.025

#apply weights to test returns #applies to Rb Testing then Ra Testing, shortcircuit by applying to Rb<>Training and then ptf<Ra<>Training * weights, but that's applying the weights to itself
Rp<-Rb_testing # easier to apply the existing structure
RpM<-RbM_testing # easier to apply the existing structure
RpW<-RbW_testing # easier to apply the existing structure
#define new column that is the dot product of the two vectors
Rp$ptf<-Ra_testing %*% opt_w
RpM$ptf<-RaM_testing %*% opt_wM
RpW$ptf<-RaW_testing %*% opt_wW

Return.cumulative(Rp)
Return.cumulative(RpW)
Return.cumulative(RpM)

Return.cumulative(RaM_testing)
RpM

#check
head(Rp)
tail(Rp)

#Compare basic metrics
#table.AnnualizedReturns(RpM)


# Chart Hypothetical Portfolio Returns ------------------------------------

#chart.CumReturns(Ra_training,legend.loc = 'topleft')
chart.CumReturns(RaM_training,legend.loc = 'topleft')
chart.CumReturns(Rp,legend.loc = 'topleft')
chart.CumReturns(RpW,legend.loc = 'topleft')
chart.CumReturns(RpM,legend.loc = 'topleft')

# End of Part 3c
# End of Stock Market Case Study 