library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(data.table)
library(chron)
library(RColorBrewer)

# https://towardsdatascience.com/time-series-calendar-heatmaps-9f576578fcfe

setwd("~/Biostat 625/Final Project/Weather")
data2001 <- read.csv('2001.csv.bz2')
data2002 <- read.csv('2002.csv.bz2')
data2003 <- read.csv('2003.csv.bz2')
data2004 <- read.csv('2004.csv.bz2')
data2005 <- read.csv('2005.csv.bz2')
data2006 <- read.csv('2006.csv.bz2')
data2007 <- read.csv('2007.csv.bz2')
data2008 <- read.csv('2008.csv.bz2')

data2001 <- data2001[ , c('Year', 'Month', 'DayofMonth', 'DepDelay')]
data2002 <- data2002[ , c('Year', 'Month', 'DayofMonth', 'DepDelay')]
data2003 <- data2003[ , c('Year', 'Month', 'DayofMonth', 'DepDelay')]
data2004 <- data2004[ , c('Year', 'Month', 'DayofMonth', 'DepDelay')]
data2005 <- data2005[ , c('Year', 'Month', 'DayofMonth', 'DepDelay')]
data2006 <- data2006[ , c('Year', 'Month', 'DayofMonth', 'DepDelay')]
data2007 <- data2007[ , c('Year', 'Month', 'DayofMonth', 'DepDelay')]
data2008 <- data2008[ , c('Year', 'Month', 'DayofMonth', 'DepDelay')]

data1 <- rbind(data2001, data2002, data2003, data2004, data2005, data2006, data2007, data2008)
fwrite(data1, 'data1.csv')

data1$Date <- ymd(paste0(data1$Year, '/', data1$Month, '/', data1$DayofMonth))
delay_prop <- data1 %>% group_by(Date) %>% summarise(delay_prop = sum(DepDelay >= 15, na.rm = T) / n()) 
saveRDS(delay_prop, "delay_prop.rds")
delay_prop <- readRDS("delay_prop.rds")


g2r <- c("#5CDA4D", "#FEFEAA", "#FF9563", "#FF0000") 

source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")
calendarHeat(delay_prop$Date[1:1461], delay_prop$delay_prop[1:1461], ncolors=99, color='g2r', varname = "Flight Delay Proportion (2001-2004)")
calendarHeat(delay_prop$Date[1462:2922], delay_prop$delay_prop[1462:2922], ncolors=99, color='g2r', varname = "Flight Delay Proportion (2005-2008)")

