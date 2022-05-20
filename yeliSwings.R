
library(baseballr)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(devtools)

#importing colors and themes for plots
#github.com/JanLMoffett/datavizExtras
source_url("https://raw.githubusercontent.com/JanLMoffett/baseballAnalysis/master/buildDatesMLB.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/datavizExtras.R")

#looking up Christian's MLB ID (592885)
#cyID <- playerid_lookup(last_name = "Yelich", first_name = "Christian")$mlbam_id
cyID <- 592885

#getting the data from baseball savant data with baseballr
#----
#the 2022 regular season started on Apr 7
cy22 <- statcast_search(start_date = "2022-04-07", 
                      end_date = today(),
                      player_type = "batter",
                      playerid = cyID)

#the SCseasons df from buildDatesMLB.R has start and end dates
#regular seasons 2015-2021

#scraping Baseball Savant data with baseballr 
#making this into a function to keep code clean
getData <- function(fourDigYr){
  d <- statcast_search(start_date = SCseasons$start[SCseasons$season == fourDigYr],
                  end_date = SCseasons$end[SCseasons$season == fourDigYr],
                  player_type = "batter",
                  playerid = cyID)
  return(d)
}

cy21 <- getData(2021)
cy20 <- getData(2020)
cy19 <- getData(2019)
cy18 <- getData(2018)
cy17 <- getData(2017)
cy16 <- getData(2016)

#combining into one df
cy <- cy22 %>% union(cy21) %>% union(cy20) %>% 
  union(cy19) %>% union(cy18) %>% 
  union(cy17) %>% union(cy16) %>%
  mutate(season = as.numeric(str_sub(game_date, start = 1, end = 4)))

#----

#creating a variable to identify swings vs non-swings
unique(cy$description)
#[1] "hit_into_play"   X       "called_strike"           "ball"                   
#[4] "swinging_strike"  X       "foul"   X                 "blocked_ball"           
#[7] "foul_tip"  X              "swinging_strike_blocked" X  "hit_by_pitch"           
#[10] "missed_bunt"             "foul_bunt"               "intent_ball"    
swingEvents <- c("hit_into_play", "swinging_strike", "foul",
                 "foul_tip", "swinging_strike_blocked")
missEvents <- c("swinging_strike", "swinging_strike_blocked")
foulEvents <- c("foul", "foul_tip")
watchEvents <- c("called_strike", "ball", "blocked_ball")

bySeason <- cy %>% group_by(season) %>%
  mutate(isSwing = ifelse(description %in% swingEvents, 1, 0),
         isFoul = ifelse(description %in% foulEvents, 1, 0),
         isWatch = ifelse(description %in% watchEvents, 1, 0)) %>%
  summarize(pitches = n(),
            swings = sum(isSwing, na.rm = T),
            fouls = sum(isFoul, na.rm = T),
            watches = sum(isWatch, na.rm = T)) %>%
  mutate(swingRt = swings/pitches,
         foulRt = fouls/pitches,
         watchRt = watches/pitches) %>%
  arrange(desc(season))

#  season pitches swings fouls watches swingRt foulRt watchRt
#   <dbl>   <int>  <dbl> <dbl>   <dbl>   <dbl>  <dbl>   <dbl>
#1   2022     648    254    99     392   0.392  0.153   0.605
#2   2021    2012    832   356    1177   0.414  0.177   0.585
#3   2020    1100    381   136     718   0.346  0.124   0.653
#4   2019    2301   1045   386    1246   0.454  0.168   0.542
#5   2018    2532   1114   437    1404   0.440  0.173   0.555
#6   2017    2883   1223   500    1652   0.424  0.173   0.573
#7   2016    2715   1094   400    1603   0.403  0.147   0.590

ggplot(bySeason) +
  geom_line(aes(x = season, y = swingRt)) + 
  geom_point(aes(x = season, y = swingRt))

ggplot(bySeason) + 
  geom_line(aes(x = season, y = foulRt)) + 
  geom_point(aes(x = season, y = foulRt))
