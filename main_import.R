
#main import file and exploratory analysis

#libraries needed:
# bigmemory
# biganalytics
# lme4
# dtplyr(?) -> shouldn't be as necessary since ralph processed it already (ty ty)
# ggplot2

library(bigmemory)
library(biganalytics)
library(lme4)
library(dtplyr)
library(ggplot2)

#takes like give or take 4-5 minutes
#x <- read.big.matrix('flights_weather2005.csv', sep = ",",header=TRUE)

colnames(x)
colrange(x,'ArrDelay',na.rm = TRUE)
colrange(x,'Year',na.rm = TRUE) #tbd, combine the year files with dtplyr

#biganalytics usage
bigreg_slr <- biglm.big.matrix(ArrDelay ~ WeatherDelay + DayOfWeek + Year, data = x)

summary(bigreg_slr)

#lme4 usage need to look into

#bigreg_lmm <- lmer(ArrDelay ~ WeatherDelay + DayofWeek,x)
#summary(bigreg_lmm)

#ggplot 2 can look at interesting variables
#need to tidy up data first
#testplot <- ggplot(x, aes(ArrDelay, DayOfWeek)) + geom_point(aes(color = "green"))
