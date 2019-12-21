#LME usage

library(data.table)
library(lme4)
library(tictoc)

path <- "D:/bios625data"
#this takes 3 minutes ish
#fpath <- file.path(path,"flight_weather_cleaned.csv")
#tic("fread 6gb data import")
#flight_data <- data.table::fread(fpath)
#toc()

#this takes 40 minutes lol
#tic("test for read.csv")
#x_test <- read.csv("D:/bios625data/flight_weather_cleaned.csv")
#toc()

model_test <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
summary(model_test)

#change parameters as needed
tic("LMM model runtime")

flight_model <- lmer(DepDelay ~ Year + Fog.y + Rain.y + Snow.y + Hail.y + Thunder.y + 
                       TEMP.y + (1 | Year), flight_data)
toc()