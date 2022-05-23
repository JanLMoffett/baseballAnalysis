
library(baseballr)
library(tidyverse)
library(lubridate)
library(devtools)
library(boot)

#a randomly sampled set of BIP to derive "average" comparison
av <- read.csv(url("https://raw.githubusercontent.com/JanLMoffett/baseballAnalysis/master/data/BIP_35dates_2015_21.csv"))
#see samplingMLB.R for how sample was gathered

#importing colors and themes for plots
#github.com/JanLMoffett/datavizExtras
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/datavizExtras.R")

#looking up Christian's MLB ID (592885)
#cyID <- playerid_lookup(last_name = "Yelich", first_name = "Christian")$mlbam_id
cyID <- 592885

#getting the data from baseball savant data with baseballr
#the 2022 regular season started on Apr 7
cy22 <- statcast_search(start_date = "2022-04-07", 
                end_date = today(),
                player_type = "batter",
                playerid = cyID)


#need a random sample of all 2022 pitches to get a 2022 MLB avg
av22 <- statcast_search(start_date = "2022-04-07",
                        end_date = today(),
                        player_type = "batter")

hitEvents <- c("single","double","triple","home_run")
outEvents <- c("field_out", "fielders_choice_out", "force_out",
               "grounded_into_double_play", "double_play")

unique(cy22$events)

#making some variables for the summary
cy22 <- cy22 %>% 
  mutate(
    paID = paste0(game_date,"_", at_bat_number),
    
    ball = ifelse(type == "B", 1, 0),
    strike = ifelse(type == "S", 1, 0),
    callStrike = ifelse(description == "called_strike", 1, 0),
    swingStrike = ifelse(description == "swinging_strike", 1, 0),
    
    BB = ifelse(events == "walk", 1, 0),
    SO = ifelse(events == "strikeout", 1, 0),
    
    BIP = ifelse(description == "hit_into_play", 1, 0),
    hit = ifelse(events %in% hitEvents, 1, 0),
    single = ifelse(events == "single",1,0),
    double = ifelse(events == "double",1,0),
    triple = ifelse(events == "triple",1,0),
    HR = ifelse(events == "home_run",1,0)
    
    )

#overall stats to date, 2022 season
#----

#all pitches
t(
cy22 %>%
  summarize(
    pitches = n(),
    PA = n_distinct(paID),
    
    balls = sum(ball, na.rm = T),
    strikes = sum(strike, na.rm = T),
    callStrikes = sum(callStrike, na.rm = T),
    swingStrikes = sum(swingStrike, na.rm = T),
    
    BB = sum(BB, na.rm = T),
    SO = sum(SO, na.rm = T),
    
    BIP = sum(BIP, na.rm = T),
    hits = sum(hit, na.rm = T),
    singles = sum(single, na.rm = T),
    doubles = sum(double, na.rm = T),
    triples = sum(triple, na.rm = T),
    HR = sum(HR, na.rm = T)
    ) %>%
  mutate(
    BABIP = hits/BIP,
    bbRt = BB/PA,
    soRt = SO/PA
    
  ))

#Analysis of Launch Characteristics

#Launch characteristics of BIP
#Christian
cy1 <- cy22 %>% filter(description == "hit_into_play") %>%
  summarize(avgLaunchAngle = mean(launch_angle, na.rm = T),
            avgLaunchSpeed = mean(launch_speed, na.rm = T)) %>%
  mutate(across(1:2, ~round(.x, digits = 1)))
#MLB average
av1 <- av %>% filter(description == "hit_into_play") %>%
  summarize(avgLaunchAngle = mean(launch_angle, na.rm = T),
            avgLaunchSpeed = mean(launch_speed, na.rm = T)) %>%
  mutate(across(1:2, ~round(.x, digits = 1)))

#where Christian lands among the distribution of launch speeds
ggplot(av) + napkin + 
  geom_histogram(aes(launch_speed), fill = transpa(ibm["purple"], 50)) + 
  geom_vline(xintercept = c(av1$avgLaunchSpeed, cy1$avgLaunchSpeed), color = c(ibm["purple"], ibm["pink"]))

#boxplots
tav <- av %>% select(launch_angle, launch_speed) %>%
  mutate(player = "MLB Average")
tcy <- cy22 %>% select(launch_angle, launch_speed) %>%
  mutate(player = "Christian Yelich")

mean(av$launch_speed, na.rm = T)



ggplot(td) + napkin + 
  geom_boxplot(aes(launch_speed, factor(player)), fill = transpa(ibm["purple"], 50))

#hits
cy22 %>% filter(events %in% hitEvents) %>%
  group_by(events) %>%
  summarize(avgLaunchAngle = mean(launch_angle, na.rm = T),
            avgLaunchSpeed = mean(launch_speed, na.rm = T))


#averages from samplingMLB file
w <- data.frame(
  type = c("BIP", "hit", "single", "double", "triple", "HR"),
  total = c(48527, 15797, 10136, 3177, 298, 2186),
  avgLaunchAngle = c(11.833299, 11.620858,  6.128548, 16.581866, 19.448190, 28.350927),
  avgLaunchSpeed = c(87.90092, 93.47043, 89.87291, 97.43947, 96.87187, 103.62863))  
  

#calculating stats by game
byGame <- cy %>% 
  group_by(game_date) %>% 
  summarize(
    pitches = n(),
    PA = n_distinct(paID),
    
    balls = sum(ball, na.rm = T),
    strikes = sum(strike, na.rm = T),
    callStrikes = sum(callStrike, na.rm = T),
    swingStrikes = sum(swingStrike, na.rm = T),
    
    BB = sum(BB, na.rm = T),
    SO = sum(SO, na.rm = T),
    
    BIP = sum(BIP, na.rm = T),
    hits = sum(hit, na.rm = T),
    singles = sum(single, na.rm = T),
    doubles = sum(double, na.rm = T),
    triples = sum(triple, na.rm = T),
    HR = sum(HR, na.rm = T)
             ) %>%
  mutate(
    game_seq = seq_along(unique(cy$game_date)),
    
    cumPitches = cumsum(pitches),
    cumPA = cumsum(PA),
    
    cumBB = cumsum(BB),
    cumSO = cumsum(SO),
    
    cumBIP = cumsum(BIP),
    cumHit = cumsum(hits)
    
  )

#game by game stats on his balls in play
byGameBIP <- cy %>% 
  filter(type == "X") %>%
  mutate(
    BIPover93 = ifelse(launch_speed > 93, 1, 0),
    BIPover96 = ifelse(launch_speed > 96, 1, 0),
    BIPover99 = ifelse(launch_speed > 99, 1, 0),

    GBover96 = ifelse(launch_speed > 96 & launch_angle < 0, 1, 0),
    GBover99 = ifelse(launch_speed > 99 & launch_angle < 0, 1, 0)
        ) %>%
  group_by(game_date) %>%
  summarize(
    avgLA = mean(launch_angle, na.rm = T),
    minLA = min(launch_angle, na.rm = T),
    maxLA = max(launch_angle, na.rm = T),
    
    avgLS = mean(launch_speed, na.rm = T),
    minLS = min(launch_speed, na.rm = T),
    maxLS = max(launch_speed, na.rm = T),
    
    BIPover93 = sum(BIPover93, na.rm = T),
    BIPover96 = sum(BIPover96, na.rm = T),
    BIPover99 = sum(BIPover99, na.rm = T),
    
    GBover96 = sum(GBover96, na.rm = T),
    GBover99 = sum(GBover99, na.rm = T)
            )
         
#adding BIP to other game stats                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
byGame <- byGame %>% left_join(byGameBIP, by = "game_date")
  

#plots

#pitch by pitch BIP data
p1 <- ggplot(cy %>% filter(type == "X")) + digiturf
cap1 <- "Source: Baseball Savant"
subt1 <- "Christian Yelich's Balls in Play, 2022 Season"
  
#color scale vectors
v <- c(jmbn["periwinkle"], jmbn["highlighter"])
names(v) <- c("0","1")
v2 <- c(jmbn["navy"], transpa(jmbn["highlighter"], 100))
names(v2) <- c("0","1")

#launch speed by launch angle
p1 + geom_point(aes(x = launch_angle, y = launch_speed, color = factor(hit))) +
  scale_color_manual(values = v) + 
  labs(title = "Launch Speed by Launch Angle",
       subtitle = subt1, caption = cap1)

#boxplot of launch angle
p1 + geom_boxplot(aes(x = launch_angle, y = factor(hit), color = factor(hit), fill = factor(hit)), 
                  outlier.size = 3, size = 1) + 
  scale_color_manual(values = v) + 
  scale_fill_manual(values = v2) +
  labs(title = "Launch Angle", subtitle = subt1, caption = cap1)

#boxplot of launch speed
p1 + geom_boxplot(aes(x = launch_speed, y = factor(hit), color = factor(hit), fill = factor(hit)), 
                  outlier.size = 3, size = 1) + 
  scale_color_manual(values = v) + 
  scale_fill_manual(values = v2) +
  labs(title = "Launch Speed",
       subtitle = subt1, caption = cap1)

#histogram of launch angle
p1 + geom_histogram(aes(x = launch_angle), binwidth = 10, 
                    fill = jmbn["mauve"], color = jmbn["rose"]) + 
  labs(title = "Launch Angle",
       subtitle = subt1, caption = cap1)

#histogram of launch speed
p1 + geom_histogram(aes(x = launch_speed), binwidth = 10,
                    fill = jmbn["mauve"], color = jmbn["rose"]) + 
  labs(title = "Launch Speed",
       subtitle = subt1, caption = cap1)


#game sequence time series plots
p2 <- ggplot(byGame) + digiturf

p2 + geom_line(aes(x = game_seq, y = cumPA), color = jmbn["thistle"]) + 
  geom_line(aes(x = game_seq, y = cumBIP), color = jmbn["periwinkle"])

nGames = dim(byGame)[1]
#rates
cy %>%
  summarize(
    PA = n_distinct(paID),
    BIP = sum(BIP, na.rm = T),
    hits = sum(hit, na.rm = T)
  ) %>%
  mutate(
    PAperGame = PA/nGames,
    BIPperGame = BIP/nGames,
    HperGame = hits/nGames,
    PofBIP = BIP/PA,
    PofH = hits/PA
  )


#what is he hitting and what is he missing?

#total he's seen of each pitch type
byPT <- cy %>% group_by(pitch_name) %>% summarize(totalSeen = n())

#BIP
byPTbip <- cy %>% filter(type == "X") %>%
  group_by(pitch_name) %>%
  summarize(totalBIP = n(),
            avgLaunchSpeed = mean(launch_speed, na.rm = T),
            minLaunchSpeed = min(launch_speed, na.rm = T),
            maxLaunchSpeed = max(launch_speed, na.rm = T),
            avgLaunchAngle = mean(launch_angle, na.rm = T),
            minLaunchAngle = min(launch_angle, na.rm = T),
            maxLaunchAngle = max(launch_angle, na.rm = T))

byPT %>% left_join(byPTbip, by = "pitch_name")

#hits
cy %>% filter(events %in% hitEvents) %>% 
  group_by(pitch_name) %>% tally()


#whiffs
cy %>% filter(description == "swinging_strike") %>%
  group_by(pitch_name) %>% tally()


#looking
cy %>% filter(description == "called_strike")


#by pitch location zone

cy <- cy %>% mutate(szRegion = case_when(
  
  zone %in% c(1:3) ~ "top",
  zone %in% c(4:6) ~ "mid",
  zone %in% c(7:9) ~ "bot",
  zone %in% c(11:14) ~"out",
  TRUE ~ "unk"
  
))

cy %>% select(szRegion)

byReg <- cy %>% group_by(szRegion) %>% summarize(totalSeen = n())

cy %>% filter(type == "X") %>% group_by(szRegion) %>%
  summarize(totalBIP = n(),
            avgLA = mean(launch_angle, na.rm = T),
            avgLS = mean(launch_speed, na.rm = T))


#i'm curious about how many bad calls Christian has suffered through

#strike zone boundaries
szt <- mean(cy$sz_top, na.rm = T)
szb <- mean(cy$sz_bot, na.rm = T)

szr <- 19.94/24
szl <- -19.94/24
#this is the width of the strike zone in inches, converted to feet 
#and divided by two to get dist from center

#coord to draw home plate on plot of pitch locations
home_plate <- data.frame("x" = c(-17/24, 17/24, 17/24, 0, -17/24), 
                         "y" = c(0, 0, -0.2, -0.4, -0.2))

#determining which called strikes were bad calls
#how many inches of leeway should be give the umps? 
leeway <- 1/12 #1 inches
cy <- cy %>% 
  mutate(
    szOutside = ifelse(plate_x < (szl - leeway), 1, 0),
    szInside = ifelse(plate_x > (szr + leeway), 1, 0),
    
    szLow = ifelse(plate_z < (szb - leeway), 1, 0),
    szHigh = ifelse(plate_z > (szt + leeway), 1, 0)
    
    ) %>%
  mutate(
    szOZone = ifelse(szOutside + szInside + szHigh + szLow > 0, 1, 0)
  ) %>%
  mutate(
    szBadCall = ifelse(szOZone + callStrike > 1, 1 , 0)
  )

#color scale vector
cv1 <- c(jmbn["rose"], jmbn["highlighter"])
names(cv1) <- c("0", "1")

#plot of pitch locations, called strikes with bad calls highlighted
ggplot(data = cy %>% filter(description == "called_strike")) + 
  
  coord_cartesian(xlim = c(-4,4), ylim = c(-1,7)) + 
  digiturf + 
  
  geom_vline(xintercept = c(szl, szr), color = jmbn["periwinkle"], size = 0.5, linetype = 2) +
  geom_hline(yintercept = c(szt, szb), color = jmbn["periwinkle"], size = 0.5, linetype = 2) +
  annotate("rect", xmin = szl, xmax = szr, ymin = szb, ymax = szt, fill = transpa(jmbn["periwinkle"], 80)) +
  
  annotate("text", x = -3.9, y = -0.2, label = "Catcher's Perspective", color = jmbn["blush"], hjust = 0) +
  
  annotate("rect", xmin = -5, xmax = 5, ymin = -2, ymax = 0, fill = transpa(jmbn["mauve"], 100)) + 
  annotate("polygon", x = home_plate$x, y = home_plate$y, fill = transpa(jmbn["blush"], 100), color = jmbn["mauve"]) + 
  
  labs(title = "Locations of Called Strikes",
       subtitle = "Christian Yelich, 2022 Season",
       x = "Horizontal Location (ft)", 
       y = "Vertical Location (ft)") +
  
  geom_point(aes(x = plate_x, y = plate_z, color = factor(szBadCall)), shape = 16) + 
  scale_color_manual(values = cv1)
  
#looking closer at these bad calls, and what effect they had on the plate appearance
badCallpaID <- cy %>% filter(szBadCall == 1) %>% pull(paID)

badCallPA <- cy %>% filter(paID %in% badCallpaID)
#6 out of 8 of these ended in strikeouts, the other 2 were walks

badCallPA %>% arrange(paID, pitch_number) %>% 
  select(paID, pitch_number, description, szBadCall, szLow, szOutside, events)

