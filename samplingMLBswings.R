

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
nDates <- 20

#getting seeds
#mySeeds <- sample(100:999, size = 7)
mySeeds <- c(947, 645, 755, 711, 547, 574, 826)

#samples of dates
set.seed(mySeeds[1]); sDates15 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2015"], size = nDates)
set.seed(mySeeds[2]); sDates16 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2016"], size = nDates)
set.seed(mySeeds[3]); sDates17 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2017"], size = nDates)
set.seed(mySeeds[4]); sDates18 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2018"], size = nDates)
set.seed(mySeeds[5]); sDates19 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2019"], size = nDates)
set.seed(mySeeds[6]); sDates20 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2020"], size = nDates)
set.seed(mySeeds[7]); sDates21 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2021"], size = nDates)

sDates <- c(sDates15, sDates16, sDates17, sDates18, sDates19, sDates20, sDates21)


swingEvents <- c("hit_into_play", "swinging_strike", "foul",
                 "foul_tip", "swinging_strike_blocked")
missEvents <- c("swinging_strike", "swinging_strike_blocked")
foulEvents <- c("foul", "foul_tip")
hitEvents <- c("single", "double", "triple", "home_run")
xbhEvents <- c("double", "triple", "home_run")

#empty data frames to hold results
freqDF <- data.frame("date" = ymd("2022-01-01"),
                     "pitches" = 99,
                     "swings" = 9,
                     "misses" = 9,
                     "fouls" = 9,
                     "BIP" = 9,
                     "H" = 9,
                     "XBH" = 9
                     )


#loop through selected dates, import data and record stats
for(i in seq_along(sDates)){
  
  print(paste0("Scraping ", i, " of ",length(sDates)))
  #pulling data with baseballr statcast_search function
  d1 <- statcast_search(start_date = ymd(sDates[i]), 
                        end_date = ymd(sDates[i]),
                        player_type = "batter")

  #the following functions add one row of sample stats at a time
  #   |   |   |   |   |   |   |   |
  #   V   V   V   V   V   V   V   V
  
  #Frequencies of BIP, outs, and hits among all pitches
  d2 <- d1 %>% 
    mutate(
      isSwing = ifelse(description %in% swingEvents, 1, 0),
      isMiss = ifelse(description %in% missEvents, 1, 0),
      isFoul = ifelse(description %in% foulEvents, 1, 0),
      isBIP = ifelse(description == "hit_into_play", 1, 0),
      isH = ifelse(events %in% hitEvents, 1, 0),
      isXBH = ifelse(events %in% xbhEvents, 1, 0)
      ) %>%
    summarize(
      date = first(game_date),
      pitches = n(),
      swings = sum(isSwing, na.rm = T),
      misses = sum(isMiss, na.rm = T),
      fouls = sum(isFoul, na.rm = T),
      BIP = sum(isBIP, na.rm = T),
      H = sum(isH, na.rm = T),
      XBH = sum(isXBH, na.rm = T))
  
  
  #append new stats to dataframes
  freqDF <- freqDF %>% union(d2)
  
  
}

#remove dummy row
freqDF <- freqDF[-1,]

#remove row with 0 pitches
freqDF <- freqDF[-which(freqDF$pitches == 0),]

summary(freqDF)

#summarizing the results overall
res1 <- freqDF %>% 
  #variables calculated for each date
  mutate(season = as.numeric(str_sub(date, start = 1, end = 4)),
         swingRate = swings/pitches,
         missRate = misses/pitches,
         foulRate = fouls/pitches,
         bipRate = BIP/pitches,
         hitRate = H/pitches,
         xbhRate = XBH/pitches) %>%
  #averaging together stats from each game in sample
  summarize(pitches = sum(pitches, na.rm = T),
            swingRate = mean(swingRate, na.rm = T),
            missRate = mean(missRate, na.rm = T),
            foulRate = mean(foulRate, na.rm = T),
            bipRate = mean(bipRate, na.rm = T),
            hitRate = mean(hitRate, na.rm = T),
            xbhRate = mean(xbhRate, na.rm = T))

#pitches swingRate missRate foulRate bipRate hitRate xbhRate
# 275249     0.468    0.109    0.180   0.178  0.0586  0.0211 
# 273489     0.467    0.109    0.180   0.178  0.0583  0.0209

#summarizing the results by season
res2 <- freqDF %>% 
  #variables calculated for each game
  mutate(season = as.numeric(str_sub(date, start = 1, end = 4)),
         swingRate = swings/pitches,
         missRate = misses/pitches,
         foulRate = fouls/pitches,
         bipRate = BIP/pitches,
         hitRate = H/pitches,
         xbhRate = XBH/pitches) %>%
  group_by(season) %>%
  #averaging together stats from each game in sample
  summarize(pitches = sum(pitches, na.rm = T),
            swingRate = mean(swingRate, na.rm = T),
            missRate = mean(missRate, na.rm = T),
            foulRate = mean(foulRate, na.rm = T),
            bipRate = mean(bipRate, na.rm = T),
            hitRate = mean(hitRate, na.rm = T),
            xbhRate = mean(xbhRate, na.rm = T)) %>%
  arrange(desc(season))

#  season pitches swingRate missRate foulRate bipRate hitRate
#1   2021   37998     0.468    0.109    0.186   0.173  0.0545
#1   2021   39153     0.471    0.112    0.188   0.171  0.0559

#2   2020   43673     0.461    0.113    0.180   0.167  0.0560
#2   2020   39573     0.460    0.117    0.175   0.167  0.0550

#3   2019   34252     0.488    0.125    0.171   0.191  0.0650
#3   2019   37068     0.487    0.122    0.174   0.192  0.0656

#4   2018   37615     0.462    0.111    0.177   0.173  0.0574
#4   2018   42401     0.464    0.106    0.184   0.174  0.0561

#5   2017   42768     0.465    0.103    0.183   0.179  0.0590
#5   2017   35971     0.465    0.105    0.182   0.178  0.0585

#6   2016   40830     0.462    0.101    0.181   0.180  0.0596
#6   2016   39601     0.463    0.103    0.179   0.180  0.0601

#7   2015   38113     0.469    0.102    0.182   0.185  0.0584
#7   2015   39722     0.462    0.0985   0.179   0.185  0.0579

res <- res1 %>% mutate(season = NA) %>%
  union(res2)

#write.csv(res, "data/sampleSwingRates.csv")
