
library(boot)

library(baseballr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)

#setwd("~/Projects/GithubRepositories/datavizExtras/")
source("datavizExtras.R")

#setwd("~/Projects/GithubRepositories/baseballAnalysis/")

#want to use bootstrapping to get MLB league averages on certain stats

#need a quick way to cycle through the dates of each baseball season

#-----
#a map for how many days in each month
mguide <- c("03" = 31, "04" = 30, "05" = 31, "06" = 30, "07" = 31, "08" = 31, "09" = 30, "10" = 31)

#function to make a month-long vector of consecutive dates in "yyyy-mm-dd" format
make_month <- function(m_2dig, y_4dig, numDays){
  v <- rep("", numDays)
  for(i in 1:numDays){
    v[i] <- paste0(y_4dig, "-", m_2dig, "-", str_pad(as.character(i), 2, side = "left", pad = "0"))
  }
  return(v)
}
#function to make a year-long vector of consecutive dates in "yyyy-mm-dd" format
make_year <- function(monthsDF){
  w <- vector()
  for(r in 1:dim(monthsDF)[1]){
    w <- c(w, make_month(monthsDF$mnth[r], monthsDF$yr[r], monthsDF$dys[r]))
  }
  return(w)
}
#function to make input for make_year function
#startDate and endDate must be "yyyy-mm-dd"
make_monthDF <- function(startDate, endDate){
  
  syear <- str_sub(startDate, 1, 4)
  
  smonth <- str_sub(startDate, 6, 7)
  emonth <- str_sub(endDate, 6, 7)
  
  sday <- str_sub(startDate, 9, 10)
  eday <- str_sub(endDate, 9, 10)
  
  #num of rows in df is det by num of unique months in season
  mi <- as.numeric(smonth):as.numeric(emonth)
  ri <- 1:length(mi)
  
  mnth = str_pad(mi, 2, side = "left", pad = "0")
  yr = rep(syear, length(mi))
  dys = mguide[mnth]
  dys[length(dys)] = eday
  
  return(
    data.frame(
      mnth,
      yr,
      dys
      )
    )
}

#https://www.baseball-reference.com/leagues/majors/2021-schedule.shtml
#The 2021 season took place Apr 1 - Oct 3, ASB Jul 12-15
#The 2020 regular season took place Jul 23 - Sep 27
#The 2019 regular season took place Mar 20 - Sep 29, ASB Jul 8-10
#The 2018 regular season took place Mar 29 - Oct 1, ASB Jul 16-18
#The 2017 regular season took place Apr 2 - Oct 1, ASB Jul 10-13
#The 2016 regular season took place Apr 3 - Oct 2, ASB Jul 11-14
#The 2015 regular season took place Apr 5 - Oct 4, ASB Jul 13-16

#a data frame containing reg season start and end dates of statcast years (2015-now)
SCseasons <- data.frame(
  season = 2015:2021,
  start = c("2015-04-05", "2016-04-03", "2017-04-02", "2018-03-29", "2019-03-20", 
            "2020-07-23", "2021-04-01"),
  end = c("2015-10-04", "2016-10-02", "2017-10-01", "2018-10-01", "2019-09-29",
          "2020-09-27", "2021-10-03"))

#a vector to hold all dates
bigDates <- vector()

#running code to build vector of dates
for(i in 1:dim(SCseasons)[1]){
  #produce a monthDF for each season
  monthsYR <- make_monthDF(SCseasons$start[i], SCseasons$end[i])
  #produce a vector of dates for each season
  datesYR <- make_year(monthsYR)
  #trim the head of the vector to season start date
  s <- as.numeric(str_sub(SCseasons$start[i], 9, 10))
  datesYR.t <- datesYR[s:length(datesYR)]
  #append to vector of all dates
  bigDates <- c(bigDates, datesYR.t)
  
}

#remove all star breaks from dates
bigDates <- bigDates[-c(which(bigDates == "2021-07-12"):which(bigDates == "2021-07-15"))]
bigDates <- bigDates[-c(which(bigDates == "2019-07-08"):which(bigDates == "2019-07-10"))]
bigDates <- bigDates[-c(which(bigDates == "2018-07-16"):which(bigDates == "2018-07-18"))]
bigDates <- bigDates[-c(which(bigDates == "2017-07-10"):which(bigDates == "2017-07-13"))]
bigDates <- bigDates[-c(which(bigDates == "2016-07-11"):which(bigDates == "2016-07-14"))]
bigDates <- bigDates[-c(which(bigDates == "2015-07-13"):which(bigDates == "2015-07-16"))]



#----

#Code for each call to statcast search
#----

#each call requires a start date and end date

#now i have a big vector of dates
head(bigDates)
tail(bigDates)

#i can sample from these dates to avoid pulling giant amounts of data

#want to sample from each year separately

#pick some random days

#sample size
nDates <- 10
#sample of dates
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
                    "sdLaunchSpeed" = 9.9,
                    "madLaunchSpeed" = 9.9,
                    "q05LaunchSpeed" = 9.9,
                    "q95LaunchSpeed" = 9.9,
                    
                    "avgLaunchAngle" = 9.9,
                    "sdLaunchAngle" = 9.9,
                    "madLaunchAngle" = 9.9,
                    "q05LaunchAngle" = 9.9,
                    "q95LaunchAngle" = 9.9
                    )

hitDF <- data.frame("date" = ymd("2022-01-01"),
                    "total" = 99,
                    "avgLaunchSpeed" = 99.9,
                    "sdLaunchSpeed" = 9.9,
                    "madLaunchSpeed" = 9.9,
                    "q05LaunchSpeed" = 9.9,
                    "q95LaunchSpeed" = 9.9,
                    
                    "avgLaunchAngle" = 9.9,
                    "sdLaunchAngle" = 9.9,
                    "madLaunchAngle" = 9.9,
                    "q05LaunchAngle" = 9.9,
                    "q95LaunchAngle" = 9.9
)

singDF <- data.frame("date" = ymd("2022-01-01"),
                     "total" = 99,
                     "avgLaunchSpeed" = 99.9,
                     "sdLaunchSpeed" = 9.9,
                     "madLaunchSpeed" = 9.9,
                     "q05LaunchSpeed" = 9.9,
                     "q95LaunchSpeed" = 9.9,
                     
                     "avgLaunchAngle" = 9.9,
                     "sdLaunchAngle" = 9.9,
                     "madLaunchAngle" = 9.9,
                     "q05LaunchAngle" = 9.9,
                     "q95LaunchAngle" = 9.9
)

doubDF <- data.frame("date" = ymd("2022-01-01"),
                     "total" = 99,
                     "avgLaunchSpeed" = 99.9,
                     "sdLaunchSpeed" = 9.9,
                     "madLaunchSpeed" = 9.9,
                     "q05LaunchSpeed" = 9.9,
                     "q95LaunchSpeed" = 9.9,
                     
                     "avgLaunchAngle" = 9.9,
                     "sdLaunchAngle" = 9.9,
                     "madLaunchAngle" = 9.9,
                     "q05LaunchAngle" = 9.9,
                     "q95LaunchAngle" = 9.9
)

tripDF <- data.frame("date" = ymd("2022-01-01"),
                     "total" = 99,
                     "avgLaunchSpeed" = 99.9,
                     "sdLaunchSpeed" = 9.9,
                     "madLaunchSpeed" = 9.9,
                     "q05LaunchSpeed" = 9.9,
                     "q95LaunchSpeed" = 9.9,
                     
                     "avgLaunchAngle" = 9.9,
                     "sdLaunchAngle" = 9.9,
                     "madLaunchAngle" = 9.9,
                     "q05LaunchAngle" = 9.9,
                     "q95LaunchAngle" = 9.9
)

hrDF <- data.frame("date" = ymd("2022-01-01"),
                   "total" = 99,
                   "avgLaunchSpeed" = 99.9,
                   "sdLaunchSpeed" = 9.9,
                   "madLaunchSpeed" = 9.9,
                   "q05LaunchSpeed" = 9.9,
                   "q95LaunchSpeed" = 9.9,
                   
                   "avgLaunchAngle" = 9.9,
                   "sdLaunchAngle" = 9.9,
                   "madLaunchAngle" = 9.9,
                   "q05LaunchAngle" = 9.9,
                   "q95LaunchAngle" = 9.9
)

pitDF <- data.frame("date" = ymd("2022-01-01"),
                    "total" = 99,
                    "avgLaunchSpeed" = 99.9,
                    "sdLaunchSpeed" = 9.9,
                    "madLaunchSpeed" = 9.9,
                    "q05LaunchSpeed" = 9.9,
                    "q95LaunchSpeed" = 9.9,
                    
                    "avgLaunchAngle" = 9.9,
                    "sdLaunchAngle" = 9.9,
                    "madLaunchAngle" = 9.9,
                    "q05LaunchAngle" = 9.9,
                    "q95LaunchAngle" = 9.9
)


#loop through selected dates
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
      sdLaunchSpeed = sd(launch_speed, na.rm = T),
      madLaunchSpeed = mad(launch_speed, na.rm = T),
      q05LaunchSpeed = quantile(launch_speed, probs = c(0.05), na.rm = T),
      q95LaunchSpeed = quantile(launch_speed, probs = c(0.95), na.rm = T),
      
      avgLaunchAngle = mean(launch_angle, na.rm = T),
      sdLaunchAngle = mean(launch_angle, na.rm = T),
      madLaunchAngle = mad(launch_angle, na.rm = T),
      q05LaunchAngle = quantile(launch_angle, probs = c(0.05), na.rm = T),
      q95LaunchAngle = quantile(launch_angle, probs = c(0.95), na.rm = T))
  
  #avg launch speed and angle of hits
  d4 <- d1 %>% filter(events %in% c("single","double","triple","home_run")) %>%
    summarize(
      date = first(game_date),
      total = n(),
      
      avgLaunchSpeed = mean(launch_speed, na.rm = T),
      sdLaunchSpeed = sd(launch_speed, na.rm = T),
      madLaunchSpeed = mad(launch_speed, na.rm = T),
      q05LaunchSpeed = quantile(launch_speed, probs = c(0.05), na.rm = T),
      q95LaunchSpeed = quantile(launch_speed, probs = c(0.95), na.rm = T),
      
      avgLaunchAngle = mean(launch_angle, na.rm = T),
      sdLaunchAngle = mean(launch_angle, na.rm = T),
      madLaunchAngle = mad(launch_angle, na.rm = T),
      q05LaunchAngle = quantile(launch_angle, probs = c(0.05), na.rm = T),
      q95LaunchAngle = quantile(launch_angle, probs = c(0.95), na.rm = T))
  
  #avg launch speed and angle of singles
  d5 <- d1 %>% filter(events == "single") %>%
    summarize(date = first(game_date),
              total = n(),
              
              avgLaunchSpeed = mean(launch_speed, na.rm = T),
              sdLaunchSpeed = sd(launch_speed, na.rm = T),
              madLaunchSpeed = mad(launch_speed, na.rm = T),
              q05LaunchSpeed = quantile(launch_speed, probs = c(0.05), na.rm = T),
              q95LaunchSpeed = quantile(launch_speed, probs = c(0.95), na.rm = T),
              
              avgLaunchAngle = mean(launch_angle, na.rm = T),
              sdLaunchAngle = mean(launch_angle, na.rm = T),
              madLaunchAngle = mad(launch_angle, na.rm = T),
              q05LaunchAngle = quantile(launch_angle, probs = c(0.05), na.rm = T),
              q95LaunchAngle = quantile(launch_angle, probs = c(0.95), na.rm = T))
  
  #avg launch speed and angle of doubles
  d6 <- d1 %>% filter(events == "double") %>%
    summarize(date = first(game_date),
              total = n(),
              
              avgLaunchSpeed = mean(launch_speed, na.rm = T),
              sdLaunchSpeed = sd(launch_speed, na.rm = T),
              madLaunchSpeed = mad(launch_speed, na.rm = T),
              q05LaunchSpeed = quantile(launch_speed, probs = c(0.05), na.rm = T),
              q95LaunchSpeed = quantile(launch_speed, probs = c(0.95), na.rm = T),
              
              avgLaunchAngle = mean(launch_angle, na.rm = T),
              sdLaunchAngle = mean(launch_angle, na.rm = T),
              madLaunchAngle = mad(launch_angle, na.rm = T),
              q05LaunchAngle = quantile(launch_angle, probs = c(0.05), na.rm = T),
              q95LaunchAngle = quantile(launch_angle, probs = c(0.95), na.rm = T))
  
  #avg launch speed and angle of triples
  d7 <- d1 %>% filter(events == "triple") %>%
    summarize(date = first(game_date),
              total = n(),
              
              avgLaunchSpeed = mean(launch_speed, na.rm = T),
              sdLaunchSpeed = sd(launch_speed, na.rm = T),
              madLaunchSpeed = mad(launch_speed, na.rm = T),
              q05LaunchSpeed = quantile(launch_speed, probs = c(0.05), na.rm = T),
              q95LaunchSpeed = quantile(launch_speed, probs = c(0.95), na.rm = T),
              
              avgLaunchAngle = mean(launch_angle, na.rm = T),
              sdLaunchAngle = mean(launch_angle, na.rm = T),
              madLaunchAngle = mad(launch_angle, na.rm = T),
              q05LaunchAngle = quantile(launch_angle, probs = c(0.05), na.rm = T),
              q95LaunchAngle = quantile(launch_angle, probs = c(0.95), na.rm = T))
  
  #avg launch speed and angle of HR
  d8 <- d1 %>% filter(events == "home_run") %>%
    summarize(date = first(game_date),
              total = n(),
              
              avgLaunchSpeed = mean(launch_speed, na.rm = T),
              sdLaunchSpeed = sd(launch_speed, na.rm = T),
              madLaunchSpeed = mad(launch_speed, na.rm = T),
              q05LaunchSpeed = quantile(launch_speed, probs = c(0.05), na.rm = T),
              q95LaunchSpeed = quantile(launch_speed, probs = c(0.95), na.rm = T),
              
              avgLaunchAngle = mean(launch_angle, na.rm = T),
              sdLaunchAngle = mean(launch_angle, na.rm = T),
              madLaunchAngle = mad(launch_angle, na.rm = T),
              q05LaunchAngle = quantile(launch_angle, probs = c(0.05), na.rm = T),
              q95LaunchAngle = quantile(launch_angle, probs = c(0.95), na.rm = T))
  
  #avg launch speed and angle of outs to the pitcher
  d9 <- d1 %>% filter(events %in% outEvents, hit_location == 1) %>%
    summarize(date = first(game_date),
              total = n(),
              
              avgLaunchSpeed = mean(launch_speed, na.rm = T),
              sdLaunchSpeed = sd(launch_speed, na.rm = T),
              madLaunchSpeed = mad(launch_speed, na.rm = T),
              q05LaunchSpeed = quantile(launch_speed, probs = c(0.05), na.rm = T),
              q95LaunchSpeed = quantile(launch_speed, probs = c(0.95), na.rm = T),
              
              avgLaunchAngle = mean(launch_angle, na.rm = T),
              sdLaunchAngle = mean(launch_angle, na.rm = T),
              madLaunchAngle = mad(launch_angle, na.rm = T),
              q05LaunchAngle = quantile(launch_angle, probs = c(0.05), na.rm = T),
              q95LaunchAngle = quantile(launch_angle, probs = c(0.95), na.rm = T))
  
  #append new stats to dataframes
  freqDF <- freqDF %>% union(d2)
  bipDF <- bipDF %>% union(d3)
  hitDF <- hitDF %>% union(d4)
  singDF <- singDF %>% union(d5)
  doubDF <- doubDF %>% union(d6)
  tripDF <- tripDF %>% union(d7)
  hrDF <- hrDF %>% union(d8)
  pitDF <- pitDF %>% union(d9)
  
}


#remove dummy row
freqDF <- freqDF[-1,]
bipDF <- bipDF[-1,]
hitDF <- hitDF[-1,]
singDF <- singDF[-1,]
doubDF <- doubDF[-1,]
tripDF <- tripDF[-1,]
hrDF <- hrDF[-1,]
pitDF <- pitDF[-1,]

#------------------------------------------------------
# Restart here! ---------------------------------------

#let's look at the results!
#write.csv(freqDF, "data/output/freqDF.csv")
#write.csv(bipDF, "data/output/bipDF.csv")
#write.csv(hitDF, "data/output/hitDF.csv")
#write.csv(singDF, "data/output/singDF.csv")
#write.csv(doubDF, "data/output/doubDF.csv")
#write.csv(tripDF, "data/output/tripDF.csv")
#write.csv(hrDF, "data/output/hrDF.csv")
#write.csv(pitDF, "data/output/pitDF.csv")

freqDF <- read.csv("data/output/freqDF.csv")
bipDF <- read.csv("data/output/bipDF.csv")
hitDF <- read.csv("data/output/hitDF.csv")
singDF <- read.csv("data/output/singDF.csv")
doubDF <- read.csv("data/output/doubDF.csv")
tripDF <- read.csv("data/output/tripDF.csv")
hrDF <- read.csv("data/output/hrDF.csv")
pitDF <- read.csv("data/output/pitDF.csv")

#frequency dataframe
#want to establish a baseline frequency for balls in play, outs, and hits on all pitches
freqDFx <- freqDF %>% mutate(pctBIP = totalBIP/total,
                             pctOut = totalOut/total,
                             pctHit = totalHit/total)


ggplot(freqDFx) + juiceGlass + 
  geom_histogram(aes(pctBIP), fill = oj["orange4"])
ggplot(freqDFx) + juiceGlass + 
  geom_boxplot(aes(pctBIP), fill = oj["orange4"])
ggplot(freqDFx) + juiceGlass + 
  geom_qq(aes(sample = pctBIP), fill = oj["orange4"])

ggplot(freqDFx) + juiceGlass + 
  geom_histogram(aes(pctOut), fill = oj["orange4"])
ggplot(freqDFx) + juiceGlass + 
  geom_boxplot(aes(pctOut), fill = oj["orange4"])
ggplot(freqDFx) + juiceGlass + 
  geom_qq(aes(sample = pctOut), fill = oj["orange4"])

freqDF %>% summarize(total = sum(total),
            totalBIP = sum(totalBIP),
            totalOut = sum(totalOut),
            totalHit = sum(totalHit)) %>%
  mutate(propBIP = totalBIP/total,
         propOut = totalOut/total,
         propHit = totalHit/total)


bipDF %>% summarize(
  total = sum(total, na.rm = T),
  avgLaunchSpeed = mean(avgLaunchSpeed, na.rm = T),
  sdLaunchSpeed = mean(sdLaunchSpeed, na.rm = T),
  madLaunchSpeed = mean(madLaunchSpeed, na.rm = T),
  q05LaunchSpeed = mean(q05LaunchSpeed, na.rm = T),
  q95LaunchSpeed = mean(q95LaunchSpeed, na.rm = T)
)

hitDF %>% summarize(
  total = sum(total, na.rm = T),
  avgLaunchSpeed = mean(avgLaunchSpeed, na.rm = T),
  sdLaunchSpeed = mean(sdLaunchSpeed, na.rm = T),
  madLaunchSpeed = mean(madLaunchSpeed, na.rm = T),
  q05LaunchSpeed = mean(q05LaunchSpeed, na.rm = T),
  q95LaunchSpeed = mean(q95LaunchSpeed, na.rm = T)
)

singDF %>% summarize(
  total = sum(total, na.rm = T),
  avgLaunchSpeed = mean(avgLaunchSpeed, na.rm = T),
  sdLaunchSpeed = mean(sdLaunchSpeed, na.rm = T),
  madLaunchSpeed = mean(madLaunchSpeed, na.rm = T),
  q05LaunchSpeed = mean(q05LaunchSpeed, na.rm = T),
  q95LaunchSpeed = mean(q95LaunchSpeed, na.rm = T)
)

doubDF %>% summarize(
  total = sum(total, na.rm = T),
  avgLaunchSpeed = mean(avgLaunchSpeed, na.rm = T),
  sdLaunchSpeed = mean(sdLaunchSpeed, na.rm = T),
  madLaunchSpeed = mean(madLaunchSpeed, na.rm = T),
  q05LaunchSpeed = mean(q05LaunchSpeed, na.rm = T),
  q95LaunchSpeed = mean(q95LaunchSpeed, na.rm = T)
)

tripDF %>% summarize(
  total = sum(total, na.rm = T),
  avgLaunchSpeed = mean(avgLaunchSpeed, na.rm = T),
  sdLaunchSpeed = mean(sdLaunchSpeed, na.rm = T),
  madLaunchSpeed = mean(madLaunchSpeed, na.rm = T),
  q05LaunchSpeed = mean(q05LaunchSpeed, na.rm = T),
  q95LaunchSpeed = mean(q95LaunchSpeed, na.rm = T)
)

hrDF %>% summarize(
  total = sum(total, na.rm = T),
  avgLaunchSpeed = mean(avgLaunchSpeed, na.rm = T),
  sdLaunchSpeed = mean(sdLaunchSpeed, na.rm = T),
  madLaunchSpeed = mean(madLaunchSpeed, na.rm = T),
  q05LaunchSpeed = mean(q05LaunchSpeed, na.rm = T),
  q95LaunchSpeed = mean(q95LaunchSpeed, na.rm = T)
)



bipDF %>% summarize(
  total = sum(total, na.rm = T),
  meanLaunchSpeed = mean(avgLaunchSpeed, na.rm = T),
  sdLaunchSpeed = sd(avgLaunchSpeed, na.rm = T),
  madLaunchSpeed = mad(avgLaunchSpeed, na.rm = T),
  q05LaunchSpeed = quantile(avgLaunchSpeed, probs = 0.05, na.rm = T),
  q95LaunchSpeed = quantile(avgLaunchSpeed, probs = 0.95, na.rm = T)
)

hitDF %>% summarize(
  total = sum(total, na.rm = T),
  meanLaunchSpeed = mean(avgLaunchSpeed, na.rm = T),
  sdLaunchSpeed = sd(avgLaunchSpeed, na.rm = T),
  madLaunchSpeed = mad(avgLaunchSpeed, na.rm = T),
  q05LaunchSpeed = quantile(avgLaunchSpeed, probs = 0.05, na.rm = T),
  q95LaunchSpeed = quantile(avgLaunchSpeed, probs = 0.95, na.rm = T)
)

singDF %>% summarize(
  total = sum(total, na.rm = T),
  meanLaunchSpeed = mean(avgLaunchSpeed, na.rm = T),
  sdLaunchSpeed = sd(avgLaunchSpeed, na.rm = T),
  madLaunchSpeed = mad(avgLaunchSpeed, na.rm = T),
  q05LaunchSpeed = quantile(avgLaunchSpeed, probs = 0.05, na.rm = T),
  q95LaunchSpeed = quantile(avgLaunchSpeed, probs = 0.95, na.rm = T)
)

doubDF %>% summarize(
  total = sum(total, na.rm = T),
  meanLaunchSpeed = mean(avgLaunchSpeed, na.rm = T),
  sdLaunchSpeed = sd(avgLaunchSpeed, na.rm = T),
  madLaunchSpeed = mad(avgLaunchSpeed, na.rm = T),
  q05LaunchSpeed = quantile(avgLaunchSpeed, probs = 0.05, na.rm = T),
  q95LaunchSpeed = quantile(avgLaunchSpeed, probs = 0.95, na.rm = T)
)

tripDF %>% summarize(
  total = sum(total, na.rm = T),
  meanLaunchSpeed = mean(avgLaunchSpeed, na.rm = T),
  meanLaunchAngle = mean(avgLaunchAngle, na.rm = T)
) %>% mutate(BIP_type = "triple")

hrDF %>% summarize(
  total = sum(total, na.rm = T),
  meanLaunchSpeed = mean(avgLaunchSpeed, na.rm = T),
  meanLaunchAngle = mean(avgLaunchAngle, na.rm = T)
) %>% mutate(BIP_type = "home_run")






angle1 = 28.3
speed1 = 103.4

y1 = sinpi(angle1/180)*speed1
x1 = cospi(angle1/180)*speed1

#plotting launch angle and speed
ggplot(hrDF) + 
  napkin + 
  coord_cartesian(xlim = c(-100, 100), ylim = c(-100,100)) + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) + 
  geom_segment(aes(x = 0, xend = x1, y = 0, yend = y1))

#make these variables in the data
hrDF %>% summarize(
  total = sum(total, na.rm = T),
  meanLaunchSpeed = mean(avgLaunchSpeed, na.rm = T),
  meanLaunchAngle = mean(avgLaunchAngle, na.rm = T)
) %>% mutate(BIP_type = "home_run",
             plotY = sinpi(meanLaunchAngle/180)*meanLaunchSpeed,
             plotX = cospi(meanLaunchAngle/180)*meanLaunchAngle)

doSummary <- function(someDF, bipType){
  someDF %>% summarize(
    total = sum(total, na.rm = T),
    meanLaunchSpeed = mean(avgLaunchSpeed, na.rm = T),
    meanLaunchAngle = mean(avgLaunchAngle, na.rm = T)
  ) %>% mutate(BIP_type = bipType,
               plotY = sinpi(meanLaunchAngle/180)*meanLaunchSpeed,
               plotX = cospi(meanLaunchAngle/180)*meanLaunchSpeed)
  
}


bipSum <- doSummary(bipDF, "BIP")
hitSum <- doSummary(hitDF, "hit")
singSum <- doSummary(singDF, "single")
doubSum <- doSummary(doubDF, "double")
tripSum <- doSummary(tripDF, "triple")
hrSum <- doSummary(hrDF, "home_run")

bigSum <- bipSum %>% union(hitSum) %>%
  union(singSum) %>% union(doubSum) %>%
  union(tripSum) %>% union(hrSum)

bigSum

#plotting launch angle and speed
ggplot(bigSum[3:6,]) + 
  napkin + 
  coord_cartesian(xlim = c(-110, 110), ylim = c(-110,110)) + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) + 
  geom_segment(aes(x = 0, y = 0, xend = plotX, yend = plotY))

ggplot(bigSum[3:6,]) + 
  napkin + 
  coord_cartesian(xlim = c(-120,120), ylim = c(-120,120)) + 
  geom_vline(xintercept = 0, color = ibm["blue"]) + 
  geom_hline(yintercept = 0, color = ibm["blue"]) + 
  geom_spoke(aes(x = 0, y = 0, angle = (pi/180)*meanLaunchAngle, radius = meanLaunchSpeed))
  

