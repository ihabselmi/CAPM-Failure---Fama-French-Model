
library(lubridate)
library(ggplot2)
## the hadley packages
library(dplyr)
library(readr)
library(tidyr)
## for linear regression models
library(broom)

## read in the returns to the 25 fama-french portfolios

ffports <- read.csv("~/Desktop/Code Work to display/FFports.csv")
View(ffports)
## clean up the dates
ffports <- ffports %>%
  gather(portfolio, ret, -Date) %>%
  mutate(Date=ymd(paste(Date,"01",sep="")))

## read in the risk factors
## and clean up the Dates

ff <- read.csv("~/Desktop/Code Work to display/FFfactors.csv")
View(ff)
ff <- ff %>% mutate(Date=ymd(paste(Date,"01",sep="")))

## join and get the excess returns
all <- ffports %>% left_join(ff) %>%
  mutate(excess=ret-RF)

## subset the data
subs <- all %>%
  filter(Date >= ymd("1963-01-01"),
         Date <= ymd("2013-12-31"))

## Get Regression coefficients for the first regression
a <- subs %>%
  group_by(portfolio) %>%
  do(tidy(lm(excess ~ Mkt.RF, .)))

## mean excess returns
b <- subs %>%
  group_by(portfolio) %>%
  summarize(meanexcess=mean(excess,na.rm = TRUE))

## join them together
c <- full_join(a,b) %>% filter(term=="Mkt.RF")

## plot everything
ggplot(c) + geom_text(aes(estimate,meanexcess,label=portfolio))

## do it the fama-french way
aFF <- subs %>%
  group_by(portfolio) %>%
  do(tidy(lm(excess ~ Mkt.RF + SMB + HML, .))) %>%
  filter(term != "(Intercept)") %>%
  select(portfolio,term,estimate)

bFF <- subs %>%
  group_by(portfolio) %>%
  summarize(meanexcess=mean(excess,na.rm = TRUE),
            meanMktRF=mean(Mkt.RF,na.rm = TRUE),
            meanHML=mean(HML,na.rm = TRUE),
            meanSMB=mean(SMB,na.rm = TRUE))

## spread the term column into the values
aFF <- spread(aFF,term,estimate)

## join and att the predicted value
cFF <- left_join(aFF,bFF) %>%
  mutate(predval=Mkt.RF*meanMktRF+HML*meanHML+SMB*meanSMB)

## make the plot
ggplot(cFF) + geom_text(aes(predval,meanexcess,label=portfolio))
