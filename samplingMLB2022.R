
library(baseballr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(boot)
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
#purposely omitting bunts from these variables, the decision is made before the pitch

#i also want to know if pitches are inside or outside of the strike zone
#based on location not result
szr <- 19.94/24
szl <- -19.94/24
#this is the width of the strike zone in inches, converted to feet 
#and divided by two to get dist from center

#determining which called strikes were bad calls
#how many inches of leeway should be give the umps? 
leeway <- 1/12 #1 inch


#per game stats------------------------------------------

#iterate through dates, calc stats for each date and save in df
for(i in seq_along(dts22)){
  
  print(paste0("Scraping ", i, " of ", length(dts22)))
  #get a date's worth of statcast data:
  d.i <- statcast_search(start_date = dts22[i], end_date = dts22[i])
  
  #calculate the same stats i did for player in question
  d.i <- d.i %>%
    mutate(
      paID = paste0(game_date, "_", pitcher, "_", batter, "_",
                    str_pad(inning, side = "left", width = 2, pad = "0"), "_",
                    str_pad(at_bat_number, side = "left", width = 2, pad = "0")),
      isCalledStrike = ifelse(description == "called_strike", 1, 0),
      isWhiff = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked"), 1, 0),
      isSwing = ifelse(description %in% swingDxs, 1, 0),
      isBIP = ifelse(description == "hit_into_play", 1, 0),
      isHit = ifelse(events %in% hitEvents, 1, 0),
      isOut = ifelse(events %in% outEvents, 1, 0),
      isBB = ifelse(events == "walk", 1, 0),
      isSO = ifelse(events %in% soEvents, 1, 0),
      szOutside = ifelse(plate_x < (szl - leeway), 1, 0),
      szInside = ifelse(plate_x > (szr + leeway), 1, 0),
      szLow = ifelse(plate_z < (szb - leeway), 1, 0),
      szHigh = ifelse(plate_z > (szt + leeway), 1, 0)
    ) %>%
    mutate(
      isOutsideZone = ifelse(szOutside + szInside + szHigh + szLow > 0, 1, 0)
    ) %>%
    mutate(
      isBadCall = ifelse(isOutsideZone + isCalledStrike > 1, 1 , 0)
    )
  
  #summarize the stats into a row and add to table
  d.i.summary <- d.i %>% summarize(
    date = first(game_date),
    pitches = n(),
    PA = n_distinct(paID),
    batters = n_distinct(batter),
    pitchers = n_distinct(pitcher),
    calledStrikes = sum(isCalledStrike, na.rm = T),
    whiffs = sum(isWhiff, na.rm = T),
    swings = sum(isSwing, na.rm = T),
    BIP = sum(isBIP, na.rm = T),
    H = sum(isHit, na.rm = T),
    BIP_out = sum(isOut, na.rm = T),
    BB = sum(isBB, na.rm = T),
    SO = sum(isSO, na.rm = T),
    outsideZone = sum(isOutsideZone, na.rm = T),
    badCalls = sum(isBadCall, na.rm = T)
  )
  
  dayStats <- union(dayStats, d.i.summary)
  
  d.i.batSummary <- d.i %>% group_by(batter) %>%
    summarize(
      date = first(game_date),
      pitches = n(),
      PA = n_distinct(paID),
      pitchersFaced = n_distinct(pitcher),
      calledStrikes = sum(isCalledStrike, na.rm = T),
      whiffs = sum(isWhiff, na.rm = T),
      swings = sum(isSwing, na.rm = T),
      BIP = sum(isBIP, na.rm = T),
      H = sum(isHit, na.rm = T),
      BIPouts = sum(isOut, na.rm = T),
      BB = sum(isBB, na.rm = T),
      SO = sum(isSO, na.rm = T),
      outsideZone = sum(isOutsideZone, na.rm = T),
      badCalls = sum(isBadCall, na.rm = T)
    ) %>% 
    filter(PA > 3) %>%
    summarize(
      date = first(date),
      batters = n(),
      pitches = mean(pitches, na.rm = T),
      PA = mean(PA, na.rm = T),
      pitchersFaced = mean(pitchersFaced, na.rm = T),
      calledStrikes = mean(calledStrikes, na.rm = T),
      whiffs = mean(whiffs, na.rm = T),
      swings = mean(swings, na.rm = T),
      BIP = mean(BIP, na.rm = T),
      H = mean(H, na.rm = T),
      BIPouts = mean(BIPouts, na.rm = T),
      BB = mean(BB, na.rm = T),
      SO = mean(SO, na.rm = T),
      outsideZone = mean(outsideZone, na.rm = T),
      badCalls = mean(badCalls, na.rm = T)
    )
  
  dayStatsBatter <- union(dayStatsBatter, d.i.batSummary)
  
}

#write.csv(dayStats, "data/dayStats2022.csv")
#write.csv(dayStatsBatter, "data/dayStatsBatter2022.csv")

#-------------------------------------------------------








