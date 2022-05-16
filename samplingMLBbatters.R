

library(baseballr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(boot)

source_url("https://raw.githubusercontent.com/JanLMoffett/baseballAnalysis/master/buildDatesMLB.R")

#pick some random days, stratified by season

#number of dates sampled from each season
nDates <- 5
#sample of dates
set.seed(624); sDates15 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2015"], size = nDates)
set.seed(245); sDates16 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2016"], size = nDates)
set.seed(534); sDates17 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2017"], size = nDates)
set.seed(457); sDates18 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2018"], size = nDates)
set.seed(512); sDates19 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2019"], size = nDates)
set.seed(697); sDates20 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2020"], size = nDates)
set.seed(753); sDates21 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2021"], size = nDates)

sDates <- c(sDates15, sDates16, sDates17, sDates18, sDates19, sDates20, sDates21)

#for each date, filter for balls in play
#bin by launch speed and angle and calculate probability of hit

#sample dataframe
d.yesterday <- statcast_search(start_date = "2022-05-09", end_date = "2022-05-09", player_type = "batter")
d.s1 <- d.yesterday[1,]

for(i in seq_along(sDates)){
  print(i)
  #pull statcast data for that date
  d1 <- statcast_search(start_date = sDates[i], end_date = sDates[i], player_type = "batter")
  d1 <- d1 %>% filter(type == "X")
  
  #add to sample dataframe
  d.s1 <- d.s1 %>% union(d1)
  
}
#take out dummy row
d.s1 <- d.s1[-1,]

#now i have a sample of balls in play
summary(d.s1$launch_angle)
summary(d.s1$launch_speed)

ggplot(d.s1) + napkin + 
  geom_histogram(aes(launch_speed), fill = oj["blue3"]) + 
  geom_vline(xintercept = mean(d.s1$launch_speed, na.rm = T), color = oj["orangeK"]) + 
  geom_vline(xintercept = quantile(d.s1$launch_speed, probs = c(0.05,0.95), na.rm = T), color = oj["orange5"]) +
  geom_vline(xintercept = quantile(d.s1$launch_speed, probs = c(0.05,0.95), na.rm = T), color = oj["orange5"])

ggplot(d.s1) + napkin + 
  geom_density(aes(launch_speed))

ggplot(d.s1) + napkin + 
  geom_histogram(aes(launch_angle))

ggplot(d.s1) + napkin + 
  geom_density(aes(launch_angle))

#i'd like to boostrap samples from the sample of BIP

