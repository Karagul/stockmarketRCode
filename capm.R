#https://msperlin.github.io/2017-01-19-CalculatingBetas/
library(BatchGetSymbols)

## Loading required package: rvest

## Loading required package: xml2

## 
## Thank you for using BatchGetSymbols! If applicable, please use the following citation in your research report. Thanks! 
## 
## APA:
##  Perlin, M. (2016). BatchGetSymbols: Downloads and Organizes Financial Data for Multiple Tickers. CRAN Package, Available in https://CRAN.R-project.org/package=BatchGetSymbols. 
## 
## BIBTEX:
##  @misc{perlin2016batchgetsymbols,
##   title = {BatchGetSymbols: Downloads and Organizes Financial Data for Multiple Tickers},
##   author = {Marcelo Perlin},
##   year = {2016},
##   journal = {CRAN Package},
##   url = {https://CRAN.R-project.org/package=BatchGetSymbols}
## }
## }

set.seed(100)

ticker.MktIdx <- '^GSPC'
first.date <- as.Date('2015-01-01')
last.date <- as.Date('2017-01-01')

n.chosen.stocks <- 10 # can't be higher than 505

# get random stocks
my.tickers <- c(sample(GetSP500Stocks()$tickers,n.chosen.stocks),
                ticker.MktIdx)

l.out <- BatchGetSymbols(tickers = my.tickers, 
                         first.date = first.date,
                         last.date = last.date)

## 
## Running BatchGetSymbols for:
##    tickers = DUK, CCI, LLY, AAL, HST, IR, SRE, FB, LH, CAH, ^GSPC
##    Downloading data for benchmark ticker
## Downloading Data for DUK from yahoo (1|11) - Good stuff!
## Downloading Data for CCI from yahoo (2|11) - Well done!
## Downloading Data for LLY from yahoo (3|11) - Got it!
## Downloading Data for AAL from yahoo (4|11) - Nice!
## Downloading Data for HST from yahoo (5|11) - Good job!
## Downloading Data for IR from yahoo (6|11) - Good job!
## Downloading Data for SRE from yahoo (7|11) - Got it!
## Downloading Data for FB from yahoo (8|11) - Nice!
## Downloading Data for LH from yahoo (9|11) - Nice!
## Downloading Data for CAH from yahoo (10|11) - Good job!
## Downloading Data for ^GSPC from yahoo (11|11) - Good stuff!

df.stocks <- l.out$df.tickers
print(l.out$df.control)

##    ticker   src download.status total.obs perc.benchmark.dates
## 1     DUK yahoo              OK       504                    1
## 2     CCI yahoo              OK       504                    1
## 3     LLY yahoo              OK       504                    1
## 4     AAL yahoo              OK       504                    1
## 5     HST yahoo              OK       504                    1
## 6      IR yahoo              OK       504                    1
## 7     SRE yahoo              OK       504                    1
## 8      FB yahoo              OK       504                    1
## 9      LH yahoo              OK       504                    1
## 10    CAH yahoo              OK       504                    1
## 11  ^GSPC yahoo              OK       504                    1
##    threshold.decision
## 1                KEEP
## 2                KEEP
## 3                KEEP
## 4                KEEP
## 5                KEEP
## 6                KEEP
## 7                KEEP
## 8                KEEP
## 9                KEEP
## 10               KEEP
## 11               KEEP
library(ggplot2)

p <- ggplot(df.stocks, aes(x=ref.date, y=price.adjusted))
p <- p + geom_line()
p <- p + facet_wrap(~ticker, scales = 'free')
print(p)

calc.ret <- function(p){
  
  my.n <- length(p)
  arit.ret <- c(NA, log(p[2:my.n]/p[1:(my.n-1)]))
  return(arit.ret)
}

list.ret <- tapply(X = df.stocks$price.adjusted, 
                   INDEX = df.stocks$ticker,
                   FUN = calc.ret)

list.ret <- list.ret[unique(df.stocks$ticker)]

df.stocks$ret <- unlist(list.ret)

df.MktIdx <- df.stocks[df.stocks$ticker==ticker.MktIdx, ]

idx <- match(df.stocks$ref.date, df.MktIdx$ref.date)

df.stocks$ret.MktIdx <- df.MktIdx$ret[idx]

# Check unique tickers
unique.tickers <- unique(df.stocks$ticker)

# create a empty vector to store betas
beta.vec <- c()

for (i.ticker in unique.tickers){
  
  # message to prompt
  cat('\nRunning ols for',i.ticker)
  
  # filter the data.frame for stock i.ticker
  df.temp <- df.stocks[df.stocks$ticker==i.ticker, ]
  
  # calculate beta with lm
  my.ols <- lm(data = df.temp, formula = ret ~ ret.MktIdx)
  
  # save beta
  my.beta <- coef(my.ols)[2]
  
  # store beta em beta.vec
  beta.vec <- c(beta.vec, my.beta)
}

## 
## Running ols for DUK
## Running ols for CCI
## Running ols for LLY
## Running ols for AAL
## Running ols for HST
## Running ols for IR
## Running ols for SRE
## Running ols for FB
## Running ols for LH
## Running ols for CAH
## Running ols for ^GSPC

# print result
print(data.frame(unique.tickers,beta.vec))

##    unique.tickers  beta.vec
## 1             DUK 0.4731914
## 2             CCI 0.7045035
## 3             LLY 0.8846485
## 4             AAL 1.3411230
## 5             HST 1.2226485
## 6              IR 1.1176236
## 7             SRE 0.6460290
## 8              FB 1.0819643
## 9              LH 0.8211889
## 10            CAH 0.9816521
## 11          ^GSPC 1.0000000

get.beta <- function(df.temp){
  
  # estimate model
  my.ols <- lm(data=df.temp, formula = ret ~ ret.MktIdx)
  
  # isolate beta
  my.beta <- coef(my.ols)[2]
  
  # return beta
  return(my.beta)
}

# get betas with by
my.l <- by(data = df.stocks, 
           INDICES = df.stocks$ticker, 
           FUN = get.beta)

# my.l is an objetct of class by. To get only its elements, we can unclass it
betas <- unclass(my.l)

# print result
print(data.frame(betas))

##           betas
## ^GSPC 1.0000000
## AAL   1.3411230
## CAH   0.9816521
## CCI   0.7045035
## DUK   0.4731914
## FB    1.0819643
## HST   1.2226485
## IR    1.1176236
## LH    0.8211889
## LLY   0.8846485
## SRE   0.6460290

library(dplyr)

## 
## Attaching package: 'dplyr'

## The following objects are masked from 'package:stats':
## 
##     filter, lag

## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union

beta.tab <- df.stocks %>% 
  group_by(ticker) %>% # group by column ticker
  do(ols.model = lm(data = ., formula = ret ~ret.MktIdx)) %>%   # estimate model
  mutate(beta = coef(ols.model)[2]) # get coefficients

print(beta.tab)

## Source: local data frame [11 x 3]
## Groups: <by row>
## 
## # A tibble: 11 × 3
##    ticker ols.model      beta
##     <chr>    <list>     <dbl>
## 1   ^GSPC  <S3: lm> 1.0000000
## 2     AAL  <S3: lm> 1.3411230
## 3     CAH  <S3: lm> 0.9816521
## 4     CCI  <S3: lm> 0.7045035
## 5     DUK  <S3: lm> 0.4731914
## 6      FB  <S3: lm> 1.0819643
## 7     HST  <S3: lm> 1.2226485
## 8      IR  <S3: lm> 1.1176236
## 9      LH  <S3: lm> 0.8211889
## 10    LLY  <S3: lm> 0.8846485
## 11    SRE  <S3: lm> 0.6460290

set.sample <- function(ref.dates){
  my.n <- length(ref.dates) # get number of dates
  my.n.half <- floor(my.n/2) # get aproximate half of observations
  
  # create grouping variable
  samples.vec <- c(rep('Sample 1', my.n.half ), rep('Sample 2', my.n-my.n.half))
  
  # return
  return(samples.vec)
}

# build group
my.l <- tapply(X = df.stocks$ref.date, 
               INDEX = df.stocks$ticker,
               FUN = set.sample )

# unsort it
my.l <- my.l[my.tickers]

# save it in dataframe
df.stocks$my.sample <- unlist(my.l)

beta.tab <- df.stocks %>% 
  group_by(ticker,my.sample) %>% # group by column ticker
  do(ols.model = lm(data = ., formula = ret ~ret.MktIdx)) %>%   # estimate model
  mutate(beta = coef(ols.model)[2]) # get coefficients

print(beta.tab)

## Source: local data frame [22 x 4]
## Groups: <by row>
## 
## # A tibble: 22 × 4
##    ticker my.sample ols.model      beta
##     <chr>     <chr>    <list>     <dbl>
## 1   ^GSPC  Sample 1  <S3: lm> 1.0000000
## 2   ^GSPC  Sample 2  <S3: lm> 1.0000000
## 3     AAL  Sample 1  <S3: lm> 1.1212908
## 4     AAL  Sample 2  <S3: lm> 1.6462873
## 5     CAH  Sample 1  <S3: lm> 0.9211033
## 6     CAH  Sample 2  <S3: lm> 1.0710157
## 7     CCI  Sample 1  <S3: lm> 0.6844971
## 8     CCI  Sample 2  <S3: lm> 0.7341892
## 9     DUK  Sample 1  <S3: lm> 0.5907584
## 10    DUK  Sample 2  <S3: lm> 0.3064395
## # ... with 12 more rows

library(dplyr)

beta.tab <- df.stocks %>% 
  group_by(ticker) %>% # group by column ticker
  do(ols.model = lm(data = ., formula = ret ~ret.MktIdx)) %>%   # estimate model
  mutate(beta = coef(ols.model)[2],
         beta.tstat = summary(ols.model)[[4]][2,3],
         alpha = coef(ols.model)[1],
         alpha.tstat = summary(ols.model)[[4]][1,3]) # get coefficients

print(beta.tab)

## Source: local data frame [11 x 6]
## Groups: <by row>
## 
## # A tibble: 11 × 6
##    ticker ols.model      beta   beta.tstat         alpha alpha.tstat
##     <chr>    <list>     <dbl>        <dbl>         <dbl>       <dbl>
## 1   ^GSPC  <S3: lm> 1.0000000 1.445949e+16 -2.513794e-19 -0.40203444
## 2     AAL  <S3: lm> 1.3411230 1.359647e+01 -4.710243e-04 -0.52817938
## 3     CAH  <S3: lm> 0.9816521 1.657973e+01 -3.069863e-04 -0.57348160
## 4     CCI  <S3: lm> 0.7045035 1.520931e+01  2.167688e-04  0.51761145
## 5     DUK  <S3: lm> 0.4731914 8.938631e+00 -6.718761e-05 -0.14037962
## 6      FB  <S3: lm> 1.0819643 1.595227e+01  5.802958e-04  0.94632384
## 7     HST  <S3: lm> 1.2226485 1.757965e+01 -4.772003e-04 -0.75890926
## 8      IR  <S3: lm> 1.1176236 2.232841e+01  2.317878e-04  0.51219268
## 9      LH  <S3: lm> 0.8211889 1.523458e+01  1.443500e-04  0.29619986
## 10    LLY  <S3: lm> 0.8846485 1.273551e+01  5.368389e-05  0.08548114
## 11    SRE  <S3: lm> 0.6460290 1.190925e+01 -2.093818e-04 -0.42692546

