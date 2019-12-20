#LME usage

library(data.table)
library(lme4)
library(tictoc)

path <- "D:/bios625data"
#fpath <- file.path(path,"flight_weather_cleaned.csv")
#tic("fread 6gb data import")
#flight_data <- data.table::fread(fpath)
#toc()

tic("test for read.csv")
x_test <- read.csv("D:/bios625data/flight_weather_cleaned.csv")
toc()

