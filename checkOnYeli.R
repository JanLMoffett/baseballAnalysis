
library(baseballr)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)

#importing colors and themes for plots
#github.com/JanLMoffett/datavizExtras
source("datavizExtras.R")

#looking up Christian's MLB ID (592885)
cyID <- playerid_lookup(last_name = "Yelich", first_name = "Christian")$mlbam_id

#getting the data with baseball savant data with baseballr
#the regular season started on Apr 7, 2022
cy <- statcast_search(start_date = "2022-04-07", 
                end_date = today(),
                player_type = "batter",
                playerid = cyID)

#making some variables for the summary
cy <- cy %>% 
  mutate(
    paID = paste0(game_date,"_", at_bat_number),
    isBall = ifelse(type == "B", 1, 0),
    isStrike = ifelse(type == "S", 1, 0),
    isBIP = ifelse(type == "X", 1, 0),
    isHit = ifelse(events %in% c("single","double","triple","home_run"),1,0),
    is1B = ifelse(events == "single",1,0),
    is2B = ifelse(events == "double",1,0),
    is3B = ifelse(events == "triple",1,0),
    isHR = ifelse(events == "home_run",1,0)
  )

#calculating stats by game
byGame <- cy %>% 
  group_by(game_date) %>% 
  summarize(
    pitches = n(),
    PA = n_distinct(paID),
    totalBall = sum(isBall, na.rm = T),
    totalStrike = sum(isStrike, na.rm = T),
    totalBIP = sum(isBIP, na.rm = T),
    totalHit = sum(isHit, na.rm = T),
    total1B = sum(is1B, na.rm = T),
    total2B = sum(is2B, na.rm = T),
    total3B = sum(is3B, na.rm = T),
    totalHR = sum(isHR, na.rm = T)
             ) %>%
  mutate(
    BABIP = totalHit/totalBIP,
    cumBIP = cumsum(totalBIP),
    cumHit = cumsum(totalHit)
  )

#game by game data on his balls in play
byGameBIP <- cy %>% 
  filter(type == "X") %>%
  group_by(game_date) %>%
  mutate(BIPover93 = ifelse(launch_speed > 93, 1, 0),
         BIPover100 = ifelse(launch_speed > 100, 1, 0),
         GBover93 = ifelse(launch_speed > 93 & launch_angle < 0, 1, 0),
         GBover100 = ifelse(launch_speed > 100 & launch_angle < 0, 1, 0)
         ) %>%
  
  summarize(avgLA = mean(launch_angle, na.rm = T),
            avgLS = mean(launch_speed, na.rm = T),
            BIPover93 = sum(BIPover93, na.rm = T),
            BIPover100 = sum(BIPover100, na.rm = T),
            GBover93 = sum(GBover93, na.rm = T),
            GBover100 = sum(GBover100, na.rm = T)
            )
         
#adding BIP data to other game stats                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
byGame <- byGame %>% left_join(byGameBIP, by = "game_date")

v <- c(jmbn["highlighter"], jmbn["periwinkle"])
names(v) <- c("1","0")

ggplot(cy %>% filter(type == "X")) + 
  digiturf + 
  geom_point(aes(launch_angle, launch_speed, color = factor(isHit))) + 
  scale_color_manual(values = v)



