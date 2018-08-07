#thank you Calum You!
#https://stackoverflow.com/questions/51642566/r-find-top-n-results-of-column-operation-on-aggregate-operation-per-column-over/51643803#51643803

#would be used on up to the last day and immediately for the next day to tell you what stocks to invest in

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


#good returns
#hcr<-data.frame(stack(tail((data.frame(Ra)[summary$colname])[,1:20],3)))$values

#bad returns
#lcr<-data.frame(stack(tail((data.frame(Ra)[summary$colname])[,n20:n],3)))$values

#boxplot(hcr,lcr)

#summary(hcr)
#summary(lcr)

#Return.cumulative(hcr)

#sum of individual cumulatives?
#sum(Return.cumulative(tail((data.frame(RaMAll)[summary$colname])[,1:20],3)))
#Return.cumulative(lcr)


#write.csv(data.frame(RaMAll)[summary$colname],"c:/test/sorted.csv")

#write.csv(data.frame(RaMAll)[list],"c:/test/sortedReduced.csv")
