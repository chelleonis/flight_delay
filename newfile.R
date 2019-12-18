
#main import file and exploratory analysis

#libraries needed:
# bigmemory
# biganalytics
# lme4
# dtplyr(?) -> shouldn't be as necessary since ralph processed it already (ty ty)
# ggplot2

#library(doParallel)
#library(doSNOW)
library(data.table)
library(bigmemory)
library(biganalytics)
library(lme4)
library(dplyr)
library(dtplyr)
library(ggplot2)

#takes like give or take 4-5 minutes each, run once
#x_2001 <- read.big.matrix('flights_weather2001.csv', sep = ",",header=TRUE, backingfile = 'airline1.bin')
#x_2002 <- read.big.matrix('flights_weather2002.csv', sep = ",",header=TRUE, backingfile = 'airline2.bin')
#x_2003 <- read.big.matrix('flights_weather2003.csv', sep = ",",header=TRUE, backingfile = 'airline3.bin')
#x_2004 <- read.big.matrix('flights_weather2004.csv', sep = ",",header=TRUE, backingfile = 'airline4.bin')
#x_2005 <- read.big.matrix('flights_weather2005.csv', sep = ",",header=TRUE, backingfile = 'airline5.bin')
#x_2006 <- read.big.matrix('flights_weather2006.csv', sep = ",",header=TRUE, backingfile = 'airline6.bin')
#x_2007 <- read.big.matrix('flights_weather2007.csv', sep = ",",header=TRUE, backingfile = 'airline7.bin')
#x_2008 <- read.big.matrix('flights_weather2008.csv', sep = ",",header=TRUE, backingfile = 'airline8.bin')


#reload files into renvir
x_2001 <- attach.big.matrix(dget('airline1.bin.desc'))
#x_2002 <- attach.big.matrix(dget('airline2.bin.desc'))

#reading using data.table
x_2001dt <- data.table::fread("flights_weather2001.csv")
x_2002dt <- data.table::fread("flights_weather2002.csv")
x_2003dt <- data.table::fread("flights_weather2003.csv")
x_2004dt <- data.table::fread("flights_weather2004.csv")
x_2005dt <- data.table::fread("flights_weather2005.csv")
x_2006dt <- data.table::fread("flights_weather2006.csv")
x_2007dt <- data.table::fread("flights_weather2007.csv")
x_2008dt <- data.table::fread("flights_weather2008.csv")

#shit breaks
#asdf <- list(x_2001dt,x_2002dt, x_2003dt, x_2004dt, x_2005dt, x_2006dt, x_2007dt, x_2008dt)
#x_comb <- do.call("rbind",asdf)

#biganalytics usage
bigreg_slr <- biglm.big.matrix(ArrDelay ~  Year + Rain.x + Fog.x + TEMP.x, data = x_2001)
summary(bigreg_slr)

#lme4 usage need to look into

#bigreg_lmm <- lmer(ArrDelay ~ WeatherDelay + DayofWeek,x)
#summary(bigreg_lmm)

#ggplot 2 can look at interesting variables
#need to tidy up data first
#testplot <- ggplot(x, aes(ArrDelay, DayOfWeek)) + geom_point(aes(color = "green")
