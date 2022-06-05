
library(baseballr)
library(tidyverse)
library(lubridate)
library(devtools)

source_url("https://raw.githubusercontent.com/JanLMoffett/baseballAnalysis/master/buildDatesMLB.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/datavizExtras.R")

#all dates so far in 2022 season
ms <- c(rep("04", 24), rep("05", 31), rep("06", 1))
ds <- c(7:30, 1:31, 1)
ys <- rep("2022", 56)

dts22 <- paste0(ys, "-", ms, "-", str_pad(ds, side = "left", width = 2, pad = "0"))

#stuff i need to make variables:
hitEvents <- c("single","double","triple","home_run")
soEvents <- c("strikeout","strikeout_double_play")
outEvents <- c("field_out","fielders_choice_out","force_out","double_play",
               "grounded_into_double_play", "sac_fly")

swingDxs <- c("swinging_strike","foul","hit_into_play","swinging_strike_blocked",
              "foul_tip")
takeDxs <- c("ball","called_strike","blocked_ball")
whiffDxs <- c("swinging_strike","swinging_strike_blocked")
foulDxs <- c("foul","foul_tip")
intentDxs <- c("intent_ball","pitchout","missed_bunt","foul_bunt")
#bip <- c("hit_into_play")
#hbp <- ("hit_by_pitch")


#i also want to know if pitches are inside or outside of the strike zone
#based on location not result
szr <- 19.94/24
szl <- -19.94/24
#this is the width of the strike zone in inches, converted to feet 
#and divided by two to get dist from center

#determining which called strikes were bad calls
#how many inches of leeway should be give the umps? 
leeway <- 1/12 #1 inch

#proportions of pitches-----------------------------------

scrapeDates <- function(datesVector){
  resDF <- data.frame(
    "date" = ymd("2022-04-01"),
    
    PA = 0,
    batters = 0,
    pitchers = 0,
    
    pitches = 0,
    takes = 0,
    swings = 0,
    
    calledStrikes = 0,
    whiffs = 0,
    fouls = 0,
    
    BIP = 0,
    H = 0,
    
    BB = 0,
    SO = 0,
    
    outsideZone = 0,
    insideZone = 0,
    
    badCalls = 0,
    
    swingsOZ = 0,
    swingsIZ = 0,
    takesOZ = 0,
    takesIZ = 0
  )
  
  #for each game date
  for(i in seq_along(datesVector)){
    #scrape that day's statcast data
    print(paste0("Scraping ", i, " of ", length(datesVector)))
    
    d.i <- statcast_search(
      start_date = as.character(datesVector[i]),
      end_date = as.character(datesVector[i])
    )
    
    #make variables for stats
    d.i <- d.i %>% mutate(
      paID = paste0(game_pk, "_", pitcher, "_", batter, "_",
                    str_pad(inning, side = "left", width = 2, pad = "0"), "_",
                    str_pad(at_bat_number, side = "left", width = 2, pad = "0")),
      isTake = ifelse(description %in% takeDxs, 1, 0),
      isSwing = ifelse(description %in% swingDxs, 1, 0),
      
      isCalledStrike = ifelse(description == "called_strike", 1, 0),
      isWhiff = ifelse(description %in% whiffDxs, 1, 0),
      isFoul = ifelse(description %in% foulDxs, 1, 0),
      
      isBIP = ifelse(description == "hit_into_play", 1, 0),
      isHit = ifelse(events %in% hitEvents, 1, 0),
      isBB = ifelse(events == "walk", 1, 0),
      isSO = ifelse(events %in% soEvents, 1, 0),

      szOutside = ifelse(plate_x < (szl - leeway), 1, 0),
      szInside = ifelse(plate_x > (szr + leeway), 1, 0),
      szLow = ifelse(plate_z < (sz_bot - leeway), 1, 0),
      szHigh = ifelse(plate_z > (sz_top + leeway), 1, 0)
    ) %>%
      mutate(
        isOutsideZone = ifelse(szOutside + szInside + szHigh + szLow > 0, 1, 0),
        isInsideZone = ifelse(szOutside + szInside + szHigh + szLow == 0, 1, 0)
      ) %>%
      mutate(
        isBadCall = ifelse(isOutsideZone + isCalledStrike > 1, 1 , 0),
        isSwingOZ = ifelse(isSwing + isOutsideZone > 1, 1, 0),
        isSwingIZ = ifelse(isSwing + isInsideZone > 1, 1, 0),
        isTakeOZ = ifelse(isTake + isOutsideZone > 1, 1, 0),
        isTakeIZ = ifelse(isTake + isInsideZone > 1, 1, 0)
      )
      
    #summarize the stats into a row
    d.i.summary <- d.i %>% summarize(
      date = first(game_date),
      
      PA = n_distinct(paID),
      batters = n_distinct(batter),
      pitchers = n_distinct(pitcher),
      
      pitches = n(),
      takes = sum(isTake, na.rm = T),
      swings = sum(isSwing, na.rm = T),
      
      calledStrikes = sum(isCalledStrike, na.rm = T),
      whiffs = sum(isWhiff, na.rm = T),
      fouls = sum(isFoul, na.rm = T),
      
      BIP = sum(isBIP, na.rm = T),
      H = sum(isHit, na.rm = T),
      
      BB = sum(isBB, na.rm = T),
      SO = sum(isSO, na.rm = T),
      
      outsideZone = sum(isOutsideZone, na.rm = T),
      insideZone = sum(isInsideZone, na.rm = T),
      
      badCalls = sum(isBadCall, na.rm = T),
      
      swingsOZ = sum(isSwingOZ, na.rm = T),
      swingsIZ = sum(isSwingIZ, na.rm = T),
      takesOZ = sum(isTakeOZ, na.rm = T),
      takesIZ = sum(isTakeIZ, na.rm = T)
    )
    
    #add row to df of results
    resDF <- union(resDF, d.i.summary) 
    
  }
  
  #return df of results
  return(resDF)
  
}

pitchProps22 <- scrapeDates(dts22)
pitchProps22 <- pitchProps22[-1,]
#write.csv(pitchProps22, "data/pitchProps22.csv")

pitchProps21 <- scrapeDates(bigDates[str_which(bigDates, "2021")])
pitchProps21 <- pitchProps21[-1,]
#write.csv(pitchProps21, "data/pitchProps21.csv")

pitchProps20 <- scrapeDates(bigDates[str_which(bigDates, "2020")])
pitchProps20 <- pitchProps20[-1,]
#write.csv(pitchProps20, "data/pitchProps20.csv")

pitchProps19 <- scrapeDates(bigDates[str_which(bigDates, "2019")])
pitchProps19 <- pitchProps19[-c(1,9),]
#write.csv(pitchProps19, "data/pitchProps19.csv")

pitchProps18 <- scrapeDates(bigDates[str_which(bigDates, "2018")])
pitchProps18 <- pitchProps18[-1,]
#write.csv(pitchProps18, "data/pitchProps18.csv")

pitchProps17 <- scrapeDates(bigDates[str_which(bigDates, "2017")])
pitchProps17 <- pitchProps17[-1,]
#write.csv(pitchProps17, "data/pitchProps17.csv")

pitchProps16 <- scrapeDates(bigDates[str_which(bigDates, "2016")])
pitchProps16 <- pitchProps16[-1,]
#write.csv(pitchProps16, "data/pitchProps16.csv")

pitchProps15 <- scrapeDates(bigDates[str_which(bigDates, "2015")])
pitchProps15 <- pitchProps15[-1,]
#write.csv(pitchProps15, "data/pitchProps15.csv")




