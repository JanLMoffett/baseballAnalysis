

library(baseballr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(devtools)

source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/datavizExtras.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/baseballAnalysis/master/buildDatesMLB.R")


#each call requires a start date and end date
#now i have a big vector of dates from the buildDatesMLB file
head(bigDates)
tail(bigDates)

#i can sample from these dates to avoid pulling giant amounts of data
#want to sample from each year separately

#pick some random days
#number of date to sample from each season
nDates <- 10
#samples of dates
set.seed(18); sDates15 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2015"], size = nDates)
set.seed(26); sDates16 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2016"], size = nDates)
set.seed(34); sDates17 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2017"], size = nDates)
set.seed(47); sDates18 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2018"], size = nDates)
set.seed(52); sDates19 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2019"], size = nDates)
set.seed(67); sDates20 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2020"], size = nDates)
set.seed(75); sDates21 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2021"], size = nDates)

sDates <- c(sDates15, sDates16, sDates17, sDates18, sDates19, sDates20, sDates21)

#levels of events var that are hits
hitEvents <- c("single","double","triple","home_run")
#levels of events var that are 'outs'
outEvents <- c("field_out", "fielders_choice_out",
               "force_out", "double_play", 
               "grounded_into_double_play")

#empty data frames to hold results
freqDF <- data.frame("date" = ymd("2022-01-01"),
                     "total" = 99,
                     "totalBIP" = 9,
                     "totalOut" = 9,
                     "totalHit" = 9)
bipDF <- data.frame("date" = ymd("2022-01-01"),
                    "total" = 99,
                    "avgLaunchSpeed" = 99.9,
                    "avgLaunchAngle" = 9.9)
hitDF <- bipDF
singDF <- bipDF
doubDF <- bipDF
tripDF <- bipDF
hrDF <- bipDF

#loop through selected dates, import data and record stats
for(i in seq_along(sDates)){
  #pulling data with baseballr statcast_search function
  d1 <- statcast_search(start_date = ymd(sDates[i]), 
                        end_date = ymd(sDates[i]),
                        player_type = "pitcher")

  #the following functions add one row of sample stats at a time
  #   |   |   |   |   |   |   |   |
  #   V   V   V   V   V   V   V   V
  
  #Frequencies of BIP, outs, and hits among all pitches
  d2 <- d1 %>% 
    mutate(
      isBIP = ifelse(type == "X", 1, 0),
      isOut = ifelse(events %in% outEvents, 1, 0),
      isHit = ifelse(events %in% hitEvents, 1, 0)
      ) %>%
    summarize(
      date = first(game_date),
      total = n(),
      totalBIP = sum(isBIP, na.rm = T),
      totalOut = sum(isOut, na.rm = T),
      totalHit = sum(isHit, na.rm = T)
      )
  
  #avg launch speed and angle of BIP
  d3 <- d1 %>% filter(type == "X") %>%
    summarize(
      date = first(game_date),
      total = n(),
      avgLaunchSpeed = mean(launch_speed, na.rm = T),
      avgLaunchAngle = mean(launch_angle, na.rm = T),
      )
  
  #avg launch speed and angle of hits
  d4 <- d1 %>% filter(events %in% c("single","double","triple","home_run")) %>%
    summarize(
      date = first(game_date),
      total = n(),
      avgLaunchSpeed = mean(launch_speed, na.rm = T),
      avgLaunchAngle = mean(launch_angle, na.rm = T),
    )
  
  #avg launch speed and angle of singles
  d5 <- d1 %>% filter(events == "single") %>%
    summarize(date = first(game_date),
              total = n(),
              avgLaunchSpeed = mean(launch_speed, na.rm = T),
              avgLaunchAngle = mean(launch_angle, na.rm = T),
    )
  
  #avg launch speed and angle of doubles
  d6 <- d1 %>% filter(events == "double") %>%
    summarize(date = first(game_date),
              total = n(),
              avgLaunchSpeed = mean(launch_speed, na.rm = T),
              avgLaunchAngle = mean(launch_angle, na.rm = T),
    )
  
  #avg launch speed and angle of triples
  d7 <- d1 %>% filter(events == "triple") %>%
    summarize(date = first(game_date),
              total = n(),
              avgLaunchSpeed = mean(launch_speed, na.rm = T),
              avgLaunchAngle = mean(launch_angle, na.rm = T),
    )
  
  #avg launch speed and angle of HR
  d8 <- d1 %>% filter(events == "home_run") %>%
    summarize(date = first(game_date),
              total = n(),
              avgLaunchSpeed = mean(launch_speed, na.rm = T),
              avgLaunchAngle = mean(launch_angle, na.rm = T),
    )
  
  #append new stats to dataframes
  freqDF <- freqDF %>% union(d2)
  bipDF <- bipDF %>% union(d3)
  hitDF <- hitDF %>% union(d4)
  singDF <- singDF %>% union(d5)
  doubDF <- doubDF %>% union(d6)
  tripDF <- tripDF %>% union(d7)
  hrDF <- hrDF %>% union(d8)
  
}

#remove dummy row
freqDF <- freqDF[-1,]
bipDF <- bipDF[-1,]
hitDF <- hitDF[-1,]
singDF <- singDF[-1,]
doubDF <- doubDF[-1,]
tripDF <- tripDF[-1,]
hrDF <- hrDF[-1,]


#summaries
freqDF %>% summarize(total = sum(total),
            totalBIP = sum(totalBIP),
            totalOut = sum(totalOut),
            totalHit = sum(totalHit)) %>%
  mutate(propBIP = totalBIP/total,
         propOut = totalOut/total,
         propHit = totalHit/total)

#   total totalBIP totalOut totalHit propBIP propOut propHit
#   <dbl>    <dbl>    <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
#1 277444    48527    31270    15797   0.175   0.113  0.0569

doSummary <- function(thisDF){
  thisDF %>% summarize(
    total = sum(total, na.rm = T),
    avgLaunchSpeed = mean(avgLaunchSpeed, na.rm = T),
    avgLaunchAngle = mean(avgLaunchAngle, na.rm = T)
    )
  }

w1 <- doSummary(bipDF)
w1 <- w1 %>% mutate(type = "BIP")
#  total avgLaunchSpeed avgLaunchAngle
#  <dbl>          <dbl>          <dbl>
#1 48527           87.9           11.8

w2 <- doSummary(hitDF)
w2 <- w2 %>% mutate(type = "hit")
#  total avgLaunchSpeed avgLaunchAngle
#  <dbl>          <dbl>          <dbl>
#1 15797           93.5           11.6

w3 <- doSummary(singDF)
w3 <- w3 %>% mutate(type = "single")
#  total avgLaunchSpeed avgLaunchAngle
#  <dbl>          <dbl>          <dbl>
#1 10136           89.9           6.13

w4 <- doSummary(doubDF)
w4 <- w4 %>% mutate(type = "double")
#  total avgLaunchSpeed avgLaunchAngle
#  <dbl>          <dbl>          <dbl>
#1  3177           97.4           16.6

w5 <- doSummary(tripDF)
w5 <- w5 %>% mutate(type = "triple")
#  total avgLaunchSpeed avgLaunchAngle
#  <dbl>          <dbl>          <dbl>
#1   298           96.9           19.4

w6 <- doSummary(hrDF)
w6 <- w6 %>% mutate(type = "HR")
#  total avgLaunchSpeed avgLaunchAngle
#  <dbl>          <dbl>          <dbl>
#1  2186           104.           28.4

w <- w1 %>% union(w2) %>% union(w3) %>% union(w4) %>% union(w5) %>% union(w6)



