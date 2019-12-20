library(data.table)
library(stringr)
library(openxlsx)

setwd("~/Biostat 625/Final Project/Weather")

# data1 <- fread('https://www1.ncdc.noaa.gov/pub/orders/CDO9571248080690.txt')
# data2 <- fread('https://www1.ncdc.noaa.gov/pub/orders/CDO2625378080692.txt')
# data3 <- fread('https://www1.ncdc.noaa.gov/pub/orders/CDO4333248080694.txt')
# data4 <- fread('https://www1.ncdc.noaa.gov/pub/orders/CDO9723398080696.txt')
# data5 <- fread('https://www1.ncdc.noaa.gov/pub/orders/CDO8978598080698.txt')
# data6 <- fread('https://www1.ncdc.noaa.gov/pub/orders/CDO6217118080700.txt')
# weather_data <- rbind(data1, data2, data3, data4, data5, data6)

weather_data <- fread('weather_data.csv')

station_codes <- as.character(weather_data$`STN---`)
station_codes[station_codes == '702730'] <- 'ANC'
station_codes[station_codes == '722020'] <- 'MIA'
station_codes[station_codes == '722030'] <- 'PBI'
station_codes[station_codes == '722050'] <- 'MCO'
station_codes[station_codes == '722060'] <- 'JAX'
station_codes[station_codes == '722108'] <- 'RSW'
station_codes[station_codes == '722110'] <- 'TPA'
station_codes[station_codes == '722190'] <- 'ATL'
station_codes[station_codes == '722280'] <- 'BHM'
station_codes[station_codes == '722310'] <- 'MSY'
station_codes[station_codes == '722430'] <- 'IAH'
station_codes[station_codes == '722440'] <- 'HOU'
station_codes[station_codes == '722530'] <- 'SAT'
station_codes[station_codes == '722540'] <- 'AUS'
station_codes[station_codes == '722590'] <- 'DFW'
station_codes[station_codes == '722780'] <- 'PHX'
station_codes[station_codes == '722880'] <- 'BUR'
station_codes[station_codes == '722900'] <- 'SAN'
station_codes[station_codes == '722950'] <- 'LAX'
station_codes[station_codes == '722977'] <- 'SNA'
station_codes[station_codes == '723060'] <- 'RDU'
station_codes[station_codes == '723140'] <- 'CLT'
station_codes[station_codes == '723270'] <- 'BNA'
station_codes[station_codes == '723650'] <- 'ABQ'
station_codes[station_codes == '723860'] <- 'LAS'
station_codes[station_codes == '724030'] <- 'IAD'
station_codes[station_codes == '724050'] <- 'DCA'
station_codes[station_codes == '724060'] <- 'BWI'
station_codes[station_codes == '724080'] <- 'PHL'
station_codes[station_codes == '724210'] <- 'CVG'
station_codes[station_codes == '724280'] <- 'CMH'
station_codes[station_codes == '724340'] <- 'STL'
station_codes[station_codes == '724380'] <- 'IND'
station_codes[station_codes == '724460'] <- 'MCI'
station_codes[station_codes == '724839'] <- 'SMF'
station_codes[station_codes == '724930'] <- 'OAK'
station_codes[station_codes == '724940'] <- 'SFO'
station_codes[station_codes == '724945'] <- 'SJC'
station_codes[station_codes == '725020'] <- 'EWR'
station_codes[station_codes == '725030'] <- 'LGA'
station_codes[station_codes == '725080'] <- 'BDL'
station_codes[station_codes == '725090'] <- 'BOS'
station_codes[station_codes == '725200'] <- 'PIT'
station_codes[station_codes == '725240'] <- 'CLE'
station_codes[station_codes == '725280'] <- 'BUF'
station_codes[station_codes == '725300'] <- 'ORD'
station_codes[station_codes == '725340'] <- 'MDW'
station_codes[station_codes == '725370'] <- 'DTW'
station_codes[station_codes == '725500'] <- 'OMA'
station_codes[station_codes == '725650'] <- 'DEN'
station_codes[station_codes == '725720'] <- 'SLC'
station_codes[station_codes == '726400'] <- 'MKE'
station_codes[station_codes == '726580'] <- 'MSP'
station_codes[station_codes == '726980'] <- 'PDX'
station_codes[station_codes == '727930'] <- 'SEA'
station_codes[station_codes == '744860'] <- 'JFK'
station_codes[station_codes == '911820'] <- 'HNL'
station_codes[station_codes == '911900'] <- 'OGG'
weather_data$`STN---` <- station_codes
unique_codes <- unique(station_codes)

weather_data$TEMP[weather_data$TEMP == 9999.9] <- NA
weather_data$DEWP[weather_data$DEWP == 9999.9] <- NA
weather_data$SLP[weather_data$SLP == 9999.9] <- NA
weather_data$STP[weather_data$STP == 9999.9] <- NA
weather_data$VISIB[weather_data$VISIB == 999.9] <- NA
weather_data$WDSP[weather_data$WDSP == 999.9] <- NA
weather_data$MXSPD[weather_data$MXSPD == 999.9] <- NA
weather_data$GUST[weather_data$GUST == 999.9] <- NA
weather_data$MAX <- gsub("*", "", weather_data$MAX, fixed = T)
weather_data$MAX[weather_data$MAX == 9999.9] <- NA
weather_data$MIN <- gsub("*", "", weather_data$MIN, fixed = T)
weather_data$MIN[weather_data$MIN == 9999.9] <- NA
weather_data$PRCP[weather_data$PRCP == 99.99] <- NA
weather_data$PRCP1 <- as.numeric(substr(weather_data$PRCP, 1, nchar(weather_data$PRCP) - 1))
weather_data$PRCP2 <- substr(weather_data$PRCP, nchar(weather_data$PRCP), nchar(weather_data$PRCP))
weather_data$PRCP2[weather_data$PRCP2 == 'A'] <- 4
weather_data$PRCP2[weather_data$PRCP2 == 'B'] <- 2
weather_data$PRCP2[weather_data$PRCP2 == 'C'] <- 1.333
weather_data$PRCP2[weather_data$PRCP2 == 'D'] <- 1
weather_data$PRCP2[weather_data$PRCP2 == 'G'] <- 1
weather_data$PRCP2[weather_data$PRCP2 == 'H'] <- 0
weather_data$PRCP2[weather_data$PRCP2 == 'I'] <- 0
weather_data$PRCP2 <- as.numeric(weather_data$PRCP2)
weather_data$PRCP <- weather_data$PRCP1 * weather_data$PRCP2
weather_data$SNDP[weather_data$SNDP == 999.9] <- 0
weather_data$FRSHTT <- str_pad(weather_data$FRSHTT, width = 6, pad = '0')
weather_data$Fog <- substr(weather_data$FRSHTT, 1, 1)
weather_data$Rain <- substr(weather_data$FRSHTT, 2, 2)
weather_data$Snow <- substr(weather_data$FRSHTT, 3, 3)
weather_data$Hail <- substr(weather_data$FRSHTT, 4, 4)
weather_data$Thunder <- substr(weather_data$FRSHTT, 5, 5)
weather_data$Tornado <- substr(weather_data$FRSHTT, 6, 6)
weather_data$key <- paste0(weather_data$'STN---', weather_data$YEARMODA)
weather_data <- subset(weather_data, select = -c(WBAN, V5, V7, V9, V11, V13, V15, FRSHTT, V23, PRCP1, PRCP2))

flights1987 <- read.csv('1987.csv.bz2')
flights1988 <- read.csv('1988.csv.bz2')
flights1989 <- read.csv('1989.csv.bz2')
flights1990 <- read.csv('1990.csv.bz2')
flights1991 <- read.csv('1991.csv.bz2')
flights1992 <- read.csv('1992.csv.bz2')
flights1993 <- read.csv('1993.csv.bz2')
flights1994 <- read.csv('1994.csv.bz2')
flights1995 <- read.csv('1995.csv.bz2')
flights1996 <- read.csv('1996.csv.bz2')
flights1997 <- read.csv('1997.csv.bz2')
flights1998 <- read.csv('1998.csv.bz2')
flights1999 <- read.csv('1999.csv.bz2')
flights2000 <- read.csv('2000.csv.bz2')
flights2001 <- read.csv('2001.csv.bz2')
flights2002 <- read.csv('2002.csv.bz2')
flights2003 <- read.csv('2003.csv.bz2')
flights2004 <- read.csv('2004.csv.bz2')
flights2005 <- read.csv('2005.csv.bz2')
flights2006 <- read.csv('2006.csv.bz2')
flights2007 <- read.csv('2007.csv.bz2')
flights2008 <- read.csv('2008.csv.bz2')

flights1987 <- flights1987[flights1987$Origin %in% unique_codes & flights1987$Dest %in% unique_codes, ]
flights1988 <- flights1988[flights1988$Origin %in% unique_codes & flights1988$Dest %in% unique_codes, ]
flights1989 <- flights1989[flights1989$Origin %in% unique_codes & flights1989$Dest %in% unique_codes, ]
flights1990 <- flights1990[flights1990$Origin %in% unique_codes & flights1990$Dest %in% unique_codes, ]
flights1991 <- flights1991[flights1991$Origin %in% unique_codes & flights1991$Dest %in% unique_codes, ]
flights1992 <- flights1992[flights1992$Origin %in% unique_codes & flights1992$Dest %in% unique_codes, ]
flights1993 <- flights1993[flights1993$Origin %in% unique_codes & flights1993$Dest %in% unique_codes, ]
flights1994 <- flights1994[flights1994$Origin %in% unique_codes & flights1994$Dest %in% unique_codes, ]
flights1995 <- flights1995[flights1995$Origin %in% unique_codes & flights1995$Dest %in% unique_codes, ]
flights1996 <- flights1996[flights1996$Origin %in% unique_codes & flights1996$Dest %in% unique_codes, ]
flights1997 <- flights1997[flights1997$Origin %in% unique_codes & flights1997$Dest %in% unique_codes, ]
flights1998 <- flights1998[flights1998$Origin %in% unique_codes & flights1998$Dest %in% unique_codes, ]
flights1999 <- flights1999[flights1999$Origin %in% unique_codes & flights1999$Dest %in% unique_codes, ]
flights2000 <- flights2000[flights2000$Origin %in% unique_codes & flights2000$Dest %in% unique_codes, ]
flights2001 <- flights2001[flights2001$Origin %in% unique_codes & flights2001$Dest %in% unique_codes, ]
flights2002 <- flights2002[flights2002$Origin %in% unique_codes & flights2002$Dest %in% unique_codes, ]
flights2003 <- flights2003[flights2003$Origin %in% unique_codes & flights2003$Dest %in% unique_codes, ]
flights2004 <- flights2004[flights2004$Origin %in% unique_codes & flights2004$Dest %in% unique_codes, ]
flights2005 <- flights2005[flights2005$Origin %in% unique_codes & flights2005$Dest %in% unique_codes, ]
flights2006 <- flights2006[flights2006$Origin %in% unique_codes & flights2006$Dest %in% unique_codes, ]
flights2007 <- flights2007[flights2007$Origin %in% unique_codes & flights2007$Dest %in% unique_codes, ]
flights2008 <- flights2008[flights2008$Origin %in% unique_codes & flights2008$Dest %in% unique_codes, ]

flights1987$key <- paste0(flights1987$Origin, flights1987$Year, 
                          str_pad(flights1987$Month, width = 2, pad = '0'),
                          str_pad(flights1987$DayofMonth, width = 2, pad = '0'))
flights1987$key2 <- paste0(flights1987$Dest, flights1987$Year, 
                          str_pad(flights1987$Month, width = 2, pad = '0'),
                          str_pad(flights1987$DayofMonth, width = 2, pad = '0'))
flights1988$key <- paste0(flights1988$Origin, flights1988$Year, 
                                str_pad(flights1988$Month, width = 2, pad = '0'),
                                str_pad(flights1988$DayofMonth, width = 2, pad = '0'))
flights1988$key2 <- paste0(flights1988$Dest, flights1988$Year, 
                           str_pad(flights1988$Month, width = 2, pad = '0'),
                           str_pad(flights1988$DayofMonth, width = 2, pad = '0'))
flights1989$key <- paste0(flights1989$Origin, flights1989$Year, 
                          str_pad(flights1989$Month, width = 2, pad = '0'),
                          str_pad(flights1989$DayofMonth, width = 2, pad = '0'))
flights1989$key2 <- paste0(flights1989$Dest, flights1989$Year, 
                           str_pad(flights1989$Month, width = 2, pad = '0'),
                           str_pad(flights1989$DayofMonth, width = 2, pad = '0'))
flights1990$key <- paste0(flights1990$Origin, flights1990$Year, 
                          str_pad(flights1990$Month, width = 2, pad = '0'),
                          str_pad(flights1990$DayofMonth, width = 2, pad = '0'))
flights1990$key2 <- paste0(flights1990$Dest, flights1990$Year, 
                           str_pad(flights1990$Month, width = 2, pad = '0'),
                           str_pad(flights1990$DayofMonth, width = 2, pad = '0'))
flights1991$key <- paste0(flights1991$Origin, flights1991$Year, 
                          str_pad(flights1991$Month, width = 2, pad = '0'),
                          str_pad(flights1991$DayofMonth, width = 2, pad = '0'))
flights1991$key2 <- paste0(flights1991$Dest, flights1991$Year, 
                           str_pad(flights1991$Month, width = 2, pad = '0'),
                           str_pad(flights1991$DayofMonth, width = 2, pad = '0'))
flights1992$key <- paste0(flights1992$Origin, flights1992$Year, 
                          str_pad(flights1992$Month, width = 2, pad = '0'),
                          str_pad(flights1992$DayofMonth, width = 2, pad = '0'))
flights1992$key2 <- paste0(flights1992$Dest, flights1992$Year, 
                           str_pad(flights1992$Month, width = 2, pad = '0'),
                           str_pad(flights1992$DayofMonth, width = 2, pad = '0'))
flights1993$key <- paste0(flights1993$Origin, flights1993$Year, 
                          str_pad(flights1993$Month, width = 2, pad = '0'),
                          str_pad(flights1993$DayofMonth, width = 2, pad = '0'))
flights1993$key2 <- paste0(flights1993$Dest, flights1993$Year, 
                           str_pad(flights1993$Month, width = 2, pad = '0'),
                           str_pad(flights1993$DayofMonth, width = 2, pad = '0'))
flights1994$key <- paste0(flights1994$Origin, flights1994$Year, 
                          str_pad(flights1994$Month, width = 2, pad = '0'),
                          str_pad(flights1994$DayofMonth, width = 2, pad = '0'))
flights1994$key2 <- paste0(flights1994$Dest, flights1994$Year, 
                           str_pad(flights1994$Month, width = 2, pad = '0'),
                           str_pad(flights1994$DayofMonth, width = 2, pad = '0'))
flights1995$key <- paste0(flights1995$Origin, flights1995$Year, 
                          str_pad(flights1995$Month, width = 2, pad = '0'),
                          str_pad(flights1995$DayofMonth, width = 2, pad = '0'))
flights1995$key2 <- paste0(flights1995$Dest, flights1995$Year, 
                           str_pad(flights1995$Month, width = 2, pad = '0'),
                           str_pad(flights1995$DayofMonth, width = 2, pad = '0'))
flights1996$key <- paste0(flights1996$Origin, flights1996$Year, 
                          str_pad(flights1996$Month, width = 2, pad = '0'),
                          str_pad(flights1996$DayofMonth, width = 2, pad = '0'))
flights1996$key2 <- paste0(flights1996$Dest, flights1996$Year, 
                           str_pad(flights1996$Month, width = 2, pad = '0'),
                           str_pad(flights1996$DayofMonth, width = 2, pad = '0'))
flights1997$key <- paste0(flights1997$Origin, flights1997$Year, 
                          str_pad(flights1997$Month, width = 2, pad = '0'),
                          str_pad(flights1997$DayofMonth, width = 2, pad = '0'))
flights1997$key2 <- paste0(flights1997$Dest, flights1997$Year, 
                           str_pad(flights1997$Month, width = 2, pad = '0'),
                           str_pad(flights1997$DayofMonth, width = 2, pad = '0'))
flights1998$key <- paste0(flights1998$Origin, flights1998$Year, 
                          str_pad(flights1998$Month, width = 2, pad = '0'),
                          str_pad(flights1998$DayofMonth, width = 2, pad = '0'))
flights1998$key2 <- paste0(flights1998$Dest, flights1998$Year, 
                           str_pad(flights1998$Month, width = 2, pad = '0'),
                           str_pad(flights1998$DayofMonth, width = 2, pad = '0'))
flights1999$key <- paste0(flights1999$Origin, flights1999$Year, 
                          str_pad(flights1999$Month, width = 2, pad = '0'),
                          str_pad(flights1999$DayofMonth, width = 2, pad = '0'))
flights1999$key2 <- paste0(flights1999$Dest, flights1999$Year, 
                           str_pad(flights1999$Month, width = 2, pad = '0'),
                           str_pad(flights1999$DayofMonth, width = 2, pad = '0'))
flights2000$key <- paste0(flights2000$Origin, flights2000$Year, 
                          str_pad(flights2000$Month, width = 2, pad = '0'),
                          str_pad(flights2000$DayofMonth, width = 2, pad = '0'))
flights2000$key2 <- paste0(flights2000$Dest, flights2000$Year, 
                           str_pad(flights2000$Month, width = 2, pad = '0'),
                           str_pad(flights2000$DayofMonth, width = 2, pad = '0'))
flights2001$key <- paste0(flights2001$Origin, flights2001$Year, 
                          str_pad(flights2001$Month, width = 2, pad = '0'),
                          str_pad(flights2001$DayofMonth, width = 2, pad = '0'))
flights2001$key2 <- paste0(flights2001$Dest, flights2001$Year, 
                           str_pad(flights2001$Month, width = 2, pad = '0'),
                           str_pad(flights2001$DayofMonth, width = 2, pad = '0'))
flights2002$key <- paste0(flights2002$Origin, flights2002$Year, 
                          str_pad(flights2002$Month, width = 2, pad = '0'),
                          str_pad(flights2002$DayofMonth, width = 2, pad = '0'))
flights2002$key2 <- paste0(flights2002$Dest, flights2002$Year, 
                           str_pad(flights2002$Month, width = 2, pad = '0'),
                           str_pad(flights2002$DayofMonth, width = 2, pad = '0'))
flights2003$key <- paste0(flights2003$Origin, flights2003$Year, 
                          str_pad(flights2003$Month, width = 2, pad = '0'),
                          str_pad(flights2003$DayofMonth, width = 2, pad = '0'))
flights2003$key2 <- paste0(flights2003$Dest, flights2003$Year, 
                           str_pad(flights2003$Month, width = 2, pad = '0'),
                           str_pad(flights2003$DayofMonth, width = 2, pad = '0'))
flights2004$key <- paste0(flights2004$Origin, flights2004$Year, 
                          str_pad(flights2004$Month, width = 2, pad = '0'),
                          str_pad(flights2004$DayofMonth, width = 2, pad = '0'))
flights2004$key2 <- paste0(flights2004$Dest, flights2004$Year, 
                           str_pad(flights2004$Month, width = 2, pad = '0'),
                           str_pad(flights2004$DayofMonth, width = 2, pad = '0'))
flights2005$key <- paste0(flights2005$Origin, flights2005$Year, 
                          str_pad(flights2005$Month, width = 2, pad = '0'),
                          str_pad(flights2005$DayofMonth, width = 2, pad = '0'))
flights2005$key2 <- paste0(flights2005$Dest, flights2005$Year, 
                           str_pad(flights2005$Month, width = 2, pad = '0'),
                           str_pad(flights2005$DayofMonth, width = 2, pad = '0'))
flights2006$key <- paste0(flights2006$Origin, flights2006$Year, 
                          str_pad(flights2006$Month, width = 2, pad = '0'),
                          str_pad(flights2006$DayofMonth, width = 2, pad = '0'))
flights2006$key2 <- paste0(flights2006$Dest, flights2006$Year, 
                           str_pad(flights2006$Month, width = 2, pad = '0'),
                           str_pad(flights2006$DayofMonth, width = 2, pad = '0'))
flights2007$key <- paste0(flights2007$Origin, flights2007$Year, 
                          str_pad(flights2007$Month, width = 2, pad = '0'),
                          str_pad(flights2007$DayofMonth, width = 2, pad = '0'))
flights2007$key2 <- paste0(flights2007$Dest, flights2007$Year, 
                           str_pad(flights2007$Month, width = 2, pad = '0'),
                           str_pad(flights2007$DayofMonth, width = 2, pad = '0'))
flights2008$key <- paste0(flights2008$Origin, flights2008$Year, 
                          str_pad(flights2008$Month, width = 2, pad = '0'),
                          str_pad(flights2008$DayofMonth, width = 2, pad = '0'))
flights2008$key2 <- paste0(flights2008$Dest, flights2008$Year, 
                           str_pad(flights2008$Month, width = 2, pad = '0'),
                           str_pad(flights2008$DayofMonth, width = 2, pad = '0'))

flights_weather1987 <- merge(flights1987, weather_data, by = 'key')
flights_weather1987 <- merge(flights_weather1987, weather_data, by.x = 'key2', by.y = 'key')
flights_weather1988 <- merge(flights1988, weather_data, by = 'key')
flights_weather1988 <- merge(flights_weather1988, weather_data, by.x = 'key2', by.y = 'key')
flights_weather1989 <- merge(flights1989, weather_data, by = 'key')
flights_weather1989 <- merge(flights_weather1989, weather_data, by.x = 'key2', by.y = 'key')
flights_weather1990 <- merge(flights1990, weather_data, by = 'key')
flights_weather1990 <- merge(flights_weather1990, weather_data, by.x = 'key2', by.y = 'key')
flights_weather1991 <- merge(flights1991, weather_data, by = 'key')
flights_weather1991 <- merge(flights_weather1991, weather_data, by.x = 'key2', by.y = 'key')
flights_weather1992 <- merge(flights1992, weather_data, by = 'key')
flights_weather1992 <- merge(flights_weather1992, weather_data, by.x = 'key2', by.y = 'key')
flights_weather1993 <- merge(flights1993, weather_data, by = 'key')
flights_weather1993 <- merge(flights_weather1993, weather_data, by.x = 'key2', by.y = 'key')
flights_weather1994 <- merge(flights1994, weather_data, by = 'key')
flights_weather1994 <- merge(flights_weather1994, weather_data, by.x = 'key2', by.y = 'key')
flights_weather1995 <- merge(flights1995, weather_data, by = 'key')
flights_weather1995 <- merge(flights_weather1995, weather_data, by.x = 'key2', by.y = 'key')
flights_weather1996 <- merge(flights1996, weather_data, by = 'key')
flights_weather1996 <- merge(flights_weather1996, weather_data, by.x = 'key2', by.y = 'key')
flights_weather1997 <- merge(flights1997, weather_data, by = 'key')
flights_weather1997 <- merge(flights_weather1997, weather_data, by.x = 'key2', by.y = 'key')
flights_weather1998 <- merge(flights1998, weather_data, by = 'key')
flights_weather1998 <- merge(flights_weather1998, weather_data, by.x = 'key2', by.y = 'key')
flights_weather1999 <- merge(flights1999, weather_data, by = 'key')
flights_weather1999 <- merge(flights_weather1999, weather_data, by.x = 'key2', by.y = 'key')
flights_weather2000 <- merge(flights2000, weather_data, by = 'key')
flights_weather2000 <- merge(flights_weather2000, weather_data, by.x = 'key2', by.y = 'key')
flights_weather2001 <- merge(flights2001, weather_data, by = 'key')
flights_weather2001 <- merge(flights_weather2001, weather_data, by.x = 'key2', by.y = 'key')
flights_weather2002 <- merge(flights2002, weather_data, by = 'key')
flights_weather2002 <- merge(flights_weather2002, weather_data, by.x = 'key2', by.y = 'key')
flights_weather2003 <- merge(flights2003, weather_data, by = 'key')
flights_weather2003 <- merge(flights_weather2003, weather_data, by.x = 'key2', by.y = 'key')
flights_weather2004 <- merge(flights2004, weather_data, by = 'key')
flights_weather2004 <- merge(flights_weather2004, weather_data, by.x = 'key2', by.y = 'key')
flights_weather2005 <- merge(flights2005, weather_data, by = 'key')
flights_weather2005 <- merge(flights_weather2005, weather_data, by.x = 'key2', by.y = 'key')
flights_weather2006 <- merge(flights2006, weather_data, by = 'key')
flights_weather2006 <- merge(flights_weather2006, weather_data, by.x = 'key2', by.y = 'key')
flights_weather2007 <- merge(flights2007, weather_data, by = 'key')
flights_weather2007 <- merge(flights_weather2007, weather_data, by.x = 'key2', by.y = 'key')
flights_weather2008 <- merge(flights2008, weather_data, by = 'key')
flights_weather2008 <- merge(flights_weather2008, weather_data, by.x = 'key2', by.y = 'key')

# fwrite(weather_data, 'weather_data.csv')

fwrite(flights_weather1987, 'flights_weather1987.csv')
fwrite(flights_weather1988, 'flights_weather1988.csv')
fwrite(flights_weather1989, 'flights_weather1989.csv')
fwrite(flights_weather1990, 'flights_weather1990.csv')
fwrite(flights_weather1991, 'flights_weather1991.csv')
fwrite(flights_weather1992, 'flights_weather1992.csv')
fwrite(flights_weather1993, 'flights_weather1993.csv')
fwrite(flights_weather1994, 'flights_weather1994.csv')
fwrite(flights_weather1995, 'flights_weather1995.csv')
fwrite(flights_weather1996, 'flights_weather1996.csv')
fwrite(flights_weather1997, 'flights_weather1997.csv')
fwrite(flights_weather1998, 'flights_weather1998.csv')
fwrite(flights_weather1999, 'flights_weather1999.csv')
fwrite(flights_weather2000, 'flights_weather2000.csv')
fwrite(flights_weather2001, 'flights_weather2001.csv')
fwrite(flights_weather2002, 'flights_weather2002.csv')
fwrite(flights_weather2003, 'flights_weather2003.csv')
fwrite(flights_weather2004, 'flights_weather2004.csv')
fwrite(flights_weather2005, 'flights_weather2005.csv')
fwrite(flights_weather2006, 'flights_weather2006.csv')
fwrite(flights_weather2007, 'flights_weather2007.csv')
fwrite(flights_weather2008, 'flights_weather2008.csv')
