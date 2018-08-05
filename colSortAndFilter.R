#thank you Calum You!
#https://stackoverflow.com/questions/51642566/r-find-top-n-results-of-column-operation-on-aggregate-operation-per-column-over/51643803#51643803

#would be used on up to the last day and immediately for the next day to tell you what stocks to invest in

colMax <- function(data) sapply(data, max, na.rm = TRUE)

library(tidyverse)

colSortAvg <- function(data) data.frame(data) %>%
  gather(colname, value) %>%
  group_by(colname) %>%
  summarise_at(.vars = vars(value), .funs = funs(mean = mean, sum = sum, max = max)) %>%
  arrange(desc(mean))

colSortMax <- function(data) data.frame(data) %>%
  gather(colname, value) %>%
  group_by(colname) %>%
  summarise_at(.vars = vars(value), .funs = funs(mean = mean, sum = sum, max = max)) %>%
  arrange(desc(max))

CR_Ra_training <- colSortMax(Return.cumulative(head(eod_ret, -63)))
#CR_RaW_training <- colSortMax(Return.cumulative(head(eow_ret, -13)))
#CR_RaM_training <- colSortMax(Return.cumulative(head(eom_ret, -3)))

avg_Ra_training <- colSortAvg(head(eod_ret, -63))
#avg_RaW_training <- colSortAvg(head(eow_ret, -13))
#avg_RaM_training <- colSortAvg(head(eom_ret, -3))

#top 20 by cumulative return

#bottom 20 
b20CR<-c()
b20AVGR<-c()

#bottom 20 

t20CR<-colnames(data.frame(eod_ret)[CR_Ra_training$colname])[1:20]
b20CR<-colnames(data.frame(eod_ret)[CR_Ra_training$colname])[(length(CR_Ra_training$colname)-20):length(CR_Ra_training$colname)]
#top 20 by average return
t20AVGR<-colnames(data.frame(eod_ret)[avg_Ra_training$colname])[1:20]
#bottom 20
b20AVGR<-colnames(data.frame(eod_ret)[avg_Ra_training$colname])[(length(avg_Ra_training$colname)-20):length(avg_Ra_training$colname)]
#export dataframe in the order specified in the summary

t20Mix<-unique(c(t20CR,t20AVGR))
b20Mix<-unique(c(b20CR,b20AVGR))

list<-c(t20Mix,b20Mix)
#good returns
hcr<-data.frame(stack(tail((data.frame(RaMAll)[summary$colname])[,1:20],3)))$values

#bad returns
lcr<-data.frame(stack(tail((data.frame(RaMAll)[summary$colname])[,n20:n],3)))$values

boxplot(hcr,lcr)

summary(hcr)
summary(lcr)

Return.cumulative(hcr)

#sum of individual cumulatives?
sum(Return.cumulative(tail((data.frame(RaMAll)[summary$colname])[,1:20],3)))
Return.cumulative(lcr)

RaM_training()


write.csv(data.frame(RaMAll)[summary$colname],"c:/test/sorted.csv")

write.csv(data.frame(RaMAll)[list],"c:/test/sortedReduced.csv")
