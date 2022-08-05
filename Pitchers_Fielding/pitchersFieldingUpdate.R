
library(baseballr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(devtools)

source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/datavizExtras.R")


fg <- read.csv("data/FGleadersSP2022.csv")

plays <- read.csv("data/startingPitcherPlays2022.csv")

unique(plays$events)
#[1] "single"                    "field_out"                 ""                         
#[4] "sac_bunt"                  "double"                    "force_out"                
#[7] "fielders_choice"           "grounded_into_double_play" "double_play"              
#[10] "strikeout"                 "field_error"               "fielders_choice_out"      
#[13] "walk"

#a lot of these events are involving pickoffs, steals, other mid-PA plays, not field outs

unique(plays %>% filter(hit_location == 1) %>% pull(events))
#[1] "single"                    "field_out"                 "sac_bunt"                 
#[4] "double"                    "force_out"                 "grounded_into_double_play"
#[7] "double_play"               "field_error"               "fielders_choice_out"      
#[10] "fielders_choice"

outEvents <- c("field_out", "force_out", "grounded_into_double_play", "double_play", "fielders_choice_out")

plays.outs <- plays %>% filter(hit_location == 1, events %in% outEvents)
plays.errors <- plays %>% filter(hit_location == 1, events == "field_error")

plays.outs <- plays.outs %>% 
  mutate(speed_over94 = ifelse(launch_speed > 94, 1, 0),
         speed_over99 = ifelse(launch_speed > 99, 1, 0),
         isLD = ifelse(bb_type == "line_drive", 1, 0),
         isGB = ifelse(bb_type == "ground_ball", 1, 0))

length(unique(plays.outs$pitcher)) #who is missing?
setdiff(unique(fg$key_mlbam), unique(plays.outs$pitcher))
#641482
fg[which(fg$key_mlbam == 641482),]
#Nestor Cortes

sp.outs <- plays.outs %>% group_by(pitcher) %>%
  summarize(
    out_plays = n(),
    avg_speed = mean(launch_speed, na.rm = T),
    avg_angle = mean(launch_angle, na.rm = T),
    outs_over94 = sum(speed_over94, na.rm = T),
    outs_over99 = sum(speed_over99, na.rm = T),
    outs_LD = sum(isLD, na.rm = T),
    outs_GB = sum(isGB, na.rm = T))

fg2 <- left_join(fg, sp.outs, by = c("key_mlbam" = "pitcher"))
names(fg2)

fg3 <- fg2 %>% mutate(BIP = FB + GB + LD) %>%
  mutate(BIP_per_IP = BIP/IP) %>%
  select(out_plays, avg_speed, avg_angle, outs_over94, outs_over99,
         outs_LD, outs_GB,
         Name, Team, IP, Pitches, BIP,
         BIP_per_IP, BABIP, K_9, BB_9, GB, LD, GB_pct, LD_pct, Hard_pct, Cent_pct)

fg3$out_plays[which(fg3$Name == "Nestor Cortes")] <- 0

ggplot(fg3) + napkin +
  labs(title = "Out Plays Made by Starting Pitchers",
       subtitle = "Qualified Starting Pitchers, Through June 3, 2022",
       x = "Out Plays Made", y = "Pitchers") + 
  geom_histogram(aes(out_plays), binwidth = 1, fill = ibm["purple"])

write.csv(fg3, "data/pitcherFieldingLeaders2022.csv")


mostOutPlays <- fg3 %>% 
  select(out_plays, Name, Team, IP, 
         BIP_per_IP, outs_over94, outs_over99, 
         outs_GB, outs_LD) %>%
  arrange(desc(out_plays)) %>%
  slice_head(n = 10)

write.csv(mostOutPlays, "data/pitchersFielding_mostOutPlays.csv")

hiAvgSpd <- fg3 %>%
  select(avg_speed, out_plays, Name, Team, IP,
         BIP_per_IP, outs_over94, outs_over99,
         outs_GB, outs_LD) %>%
  arrange(desc(avg_speed)) %>%
  filter(out_plays > 3) %>%
  slice_head(n = 10)

write.csv(hiAvgSpd, "data/pitchersFielding_hiAvgSpd.csv")


mostOver94 <- fg3 %>%
  select(outs_over94, outs_over99,
         Name, Team, IP, BIP_per_IP, out_plays,
                            outs_GB, outs_LD) %>%
  arrange(desc(outs_over94), desc(outs_over99)) %>%
  slice_head(n = 15)
  
write.csv(mostOver94, "data/pitchersFielding_mostOver94.csv")


highestCentpct <- fg3 %>%
  arrange(desc(Cent_pct)) %>% 
  select(Cent_pct, out_plays, Name, Team) %>%
  slice_head(n = 10)

write.csv(highestCentpct, "data/pitchersFielding_hiCentPct.csv")

highestGBpct <- fg3 %>%
  arrange(desc(GB_pct)) %>%
  select(GB_pct, out_plays, Name, Team) %>%
  slice_head(n = 10)

write.csv(highestGBpct, "data/pitchersFielding_hiGBPct.csv")

highestBIPperIP <- fg3 %>%
  arrange(desc(BIP_per_IP)) %>% 
  select(BIP_per_IP, out_plays, Name, Team) %>%
  slice_head(n = 10)

write.csv(highestBIPperIP, "data/pitchersFielding_hiBipIP.csv")





