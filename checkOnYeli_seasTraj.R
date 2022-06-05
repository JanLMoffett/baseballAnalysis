
library(baseballr)
library(tidyverse)
library(lubridate)
library(devtools)
library(boot)

#importing colors and themes for plots
#github.com/JanLMoffett/datavizExtras
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/datavizExtras.R")
#mlb season dates
source_url("https://raw.githubusercontent.com/JanLMoffett/baseballAnalysis/master/buildDatesMLB.R")

# Build datasets (skip this part)
################################################################################

#looking up Christian's MLB ID (592885)
#cyID <- playerid_lookup(last_name = "Yelich", first_name = "Christian")$mlbam_id
cyID <- 592885

#getting the data from baseball savant data with baseballr
#the 2022 regular season started on Apr 7
cy22 <- statcast_search(start_date = "2022-04-07", 
                end_date = today(),
                player_type = "batter",
                playerid = cyID)


#getting data from past seasons
row.names(SCseasons) <- SCseasons$season
getCYdata <- function(season){
  return(statcast_search(start_date = SCseasons[as.character(season),"start"],
                         end_date = SCseasons[as.character(season), "end"],
                         player_type = "batter",
                         playerid = cyID))
}

cy21 <- getCYdata(2021)
cy20 <- getCYdata(2020)
cy19 <- getCYdata(2019)
cy18 <- getCYdata(2018)
cy17 <- getCYdata(2017)
cy16 <- getCYdata(2016)
cy15 <- getCYdata(2015)

#what i need is sequential data, game by game, (index games in stead of using dates),
#PA by PA, and pitch by pitch

#append the tables together
cy <- union(cy22, cy21) %>% union(cy20) %>% union(cy19) %>% union(cy18) %>%
  union(cy17) %>% union(cy16) %>% union(cy15)

#put in sequential order 
cy <- cy %>% mutate(game_date = ymd(game_date)) %>%
  mutate(season = year(game_date)) %>%
  arrange(game_date, pitcher, at_bat_number, pitch_number)

#make unique gameID's with date and home SP's ID
cy <- cy %>% group_by(game_date) %>%
  mutate(hsp = first(pitcher)) %>%
  ungroup() %>%
  mutate(gameID = paste0(game_date, "_", hsp))

#make unique PA ID and pitch ID
cy <- cy %>% mutate(
  paID = paste0(gameID, "_", str_pad(at_bat_number, width = 2, side = "left", pad = "0")),
  pitchID = paste0(gameID, "_", str_pad(at_bat_number, width = 2, side = "left", pad = "0"), 
                   str_pad(pitch_number, width = 2, side = "left", pad = "0")))


#index pitches in sequential order
cy$pitch_seq <- 1:dim(cy)[1]
#make a column of 1's to make sequences
cy$one <- 1
#index pitches within games
cy <- cy %>% group_by(gameID) %>%
  mutate(pitch_of_game_seq = cumsum(one)) %>%
  ungroup()
#index pitches within seasons
cy <- cy %>% group_by(season) %>%
  mutate(pitch_of_seas_seq = cumsum(one)) %>%
  ungroup()


#make some of the variables I'm interested in tracking:
#BIP, swings, called strikes, hits, outs
unique(cy$description)
unique(cy$events)
hitEvents <- c("single","double","triple","home_run")
soEvents <- c("strikeout","strikeout_double_play")
outEvents <- c("field_out","fielders_choice_out","force_out","double_play",
               "grounded_into_double_play", "sac_fly")
swingDxs <- c("swinging_strike","foul","hit_into_play","swinging_strike_blocked",
              "foul_tip")
#purposely omitting bunts from these variables, the decision is made before the pitch

#i also want to know if pitches are inside or outside of the strike zone
#based on location not result
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
leeway <- 1/12 #1 inch

cy <- cy %>%
  mutate(
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

#index all PA in sequential order
paIndex <- cy %>% group_by(paID) %>%
  summarize(one = n_distinct(paID),
            gameID = first(gameID),
            season = first(season),
            pitches = n(),
            called_strikes = sum(isCalledStrike, na.rm = T),
            whiffs = sum(isWhiff, na.rm = T),
            swings = sum(isSwing, na.rm = T),
            pitches_outside_zone = sum(isOutsideZone, na.rm = T),
            bad_calls = sum(isBadCall, na.rm = T),
            isBIP = sum(isBIP, na.rm = T),
            result = last(events),
            delta_run_exp = sum(delta_run_exp, na.rm = T)
            ) %>%
  mutate(PA_seq = cumsum(one))
#index PA within games
paIndex <- paIndex %>% group_by(gameID) %>%
  mutate(PA_of_game_seq = cumsum(one)) %>%
  ungroup()
#index PA within seasons
paIndex <- paIndex %>% group_by(season) %>%
  mutate(PA_of_seas_seq = cumsum(one)) %>%
  ungroup()

#index all games in sequential order
gameIndex <- cy %>% group_by(gameID) %>%
  summarize(one = n_distinct(gameID),
            season = first(season),
            pitches = n(),
            PA = n_distinct(paID),
            called_strikes = sum(isCalledStrike, na.rm = T),
            whiffs = sum(isWhiff, na.rm = T),
            swings = sum(isSwing, na.rm = T),
            pitches_outside_zone = sum(isOutsideZone, na.rm = T),
            bad_calls = sum(isBadCall, na.rm = T),
            BIP = sum(isBIP, na.rm = T),
            H = sum(isHit, na.rm = T),
            BIP_outs = sum(isOut, na.rm = T),
            BB = sum(isBB, na.rm = T),
            SO = sum(isSO, na.rm = T),
            delta_run_exp = sum(delta_run_exp, na.rm = T)
            ) %>%
  mutate(game_seq = cumsum(one))
#index games within seasons
gameIndex <- gameIndex %>% group_by(season) %>%
  mutate(game_of_seas_seq = cumsum(one)) %>%
  ungroup()

#save data
#write.csv(gameIndex, "data/yelichGameIndex.csv")
#write.csv(paIndex, "data/yelichPAIndex.csv")
#write.csv(cy, "data/yelichPitches.csv")

#!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-
#NOTE: Double headers may be out of order, games on same date are ordered by pitcher ID
#!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-

################################################################################

#Start here!
################################################################################
#read in data sets
g <- read.csv("data/yelichGameIndex.csv")

names(g)

#want cumulative stats so i can plot rates
g <- g %>% group_by(season) %>%
  mutate(cumSwings = cumsum(swings),
         cumWhiff = cumsum(whiffs),
         cumBB = cumsum(BB),
         cumSO = cumsum(SO),
         cumBIP = cumsum(BIP),
         cumH = cumsum(H)
         ) %>%
  ungroup()

#BIP per game rates
ggplot(g) + napkin +
  labs(title = "BIP per Game Rates by Season",
       subtitle = "Christian Yelich, 2015 - 2022",
       caption = "Data Source: Baseball Savant",
       tag = "Fig 1.1",
       x = "Game of Season",
       y = "Cumulative Balls in Play") + 
  geom_line(aes(x = game_of_seas_seq, 
                y = cumBIP, 
                group = factor(season), 
                color = factor(season)),
            size = 1)
#detail of last plot, zoom in on first 50 games
last_plot() + coord_cartesian(xlim = c(0,50), ylim = c(0,170)) + 
  labs(tag = "Fig 1.2")

#Hit per game rates
ggplot(g) + napkin +
  labs(title = "Hits per Game Rates by Season",
       subtitle = "Christian Yelich, 2015 - 2022",
       caption = "Data Source: Baseball Savant",
       tag = "Fig 2.1",
       x = "Game of Season",
       y = "Cumulative Hits") + 
  geom_line(aes(x = game_of_seas_seq, 
                y = cumH, 
                group = factor(season), 
                color = factor(season)),
            size = 1)
#detail of last plot, zoom in on first 50 games
last_plot() + coord_cartesian(xlim = c(0,50), ylim = c(0,70)) + 
  labs(tag = "Fig 2.2")


#swings per game rates
ggplot(g) + napkin +
  labs(title = "Swings per Game Rates by Season",
       subtitle = "Christian Yelich, 2015 - 2022",
       caption = "Data Source: Baseball Savant",
       tag = "Fig 3.1",
       x = "Game of Season",
       y = "Cumulative Swings") + 
  geom_line(aes(x = game_of_seas_seq, 
                y = cumSwings, 
                group = factor(season), 
                color = factor(season)),
            size = 1)
#detail of last plot, zoom in on first 50 games
last_plot() + coord_cartesian(xlim = c(0,50), ylim = c(0,400)) + 
  labs(tag = "Fig 3.2")

#whiffs per game rates
ggplot(g) + napkin +
  labs(title = "Whiffs per Game Rates by Season",
       subtitle = "Christian Yelich, 2015 - 2022",
       caption = "Data Source: Baseball Savant",
       tag = "Fig 4.1",
       x = "Game of Season",
       y = "Cumulative Whiffs") + 
  geom_line(aes(x = game_of_seas_seq, 
                y = cumWhiff, 
                group = factor(season), 
                color = factor(season)),
            size = 1)
#detail of last plot, zoom in on first 50 games
last_plot() + coord_cartesian(xlim = c(0,50), ylim = c(0,120)) + 
  labs(tag = "Fig 4.2")

#SO per game rates
ggplot(g) + napkin +
  labs(title = "Strikeouts per Game Rates by Season",
       subtitle = "Christian Yelich, 2015 - 2022",
       caption = "Data Source: Baseball Savant",
       tag = "Fig 5.1",
       x = "Game of Season",
       y = "Cumulative Strikeouts") + 
  geom_line(aes(x = game_of_seas_seq, 
                y = cumSO, 
                group = factor(season), 
                color = factor(season)),
            size = 1)
#detail of last plot, zoom in on first 50 games
last_plot() + coord_cartesian(xlim = c(0,50), ylim = c(0,75)) + 
  labs(tag = "Fig 5.2")

#BB per game rates
ggplot(g) + napkin +
  labs(title = "Walks per Game Rates by Season",
       subtitle = "Christian Yelich, 2015 - 2022",
       caption = "Data Source: Baseball Savant",
       tag = "Fig 6.1",
       x = "Game of Season",
       y = "Cumulative Walks") + 
  geom_line(aes(x = game_of_seas_seq, 
                y = cumBB, 
                group = factor(season), 
                color = factor(season)),
            size = 1)
#detail of last plot, zoom in on first 50 games
last_plot() + coord_cartesian(xlim = c(0,50), ylim = c(0,45)) + 
  labs(tag = "Fig 6.2")

#table of totals and perGame rates by season
totalsAndRates <- g %>% group_by(season) %>%
  summarize(
    games = n(),
    pitches = sum(pitches, na.rm = T),
    PA = sum(PA, na.rm = T),
    called_strikes = sum(called_strikes, na.rm = T),
    whiffs = sum(whiffs, na.rm = T),
    swings = sum(swings, na.rm = T),
    pitches_outside_zone = sum(pitches_outside_zone, na.rm = T),
    bad_calls = sum(bad_calls, na.rm = T),
    BIP = sum(BIP, na.rm = T),
    H = sum(H, na.rm = T),
    BIP_outs = sum(BIP_outs, na.rm = T),
    BB = sum(BB, na.rm = T), 
    SO = sum(SO, na.rm = T),
    delta_run_exp = sum(delta_run_exp)
  ) %>%
  mutate(
    pitches_perGame = pitches/games,
    PA_perGame = PA/games,
    called_strikes_perGame = called_strikes/games,
    whiffs_perGame = whiffs/games,
    swings_perGame = swings/games,
    pitches_oZone_perGame = pitches_outside_zone/games,
    bad_calls_perGame = bad_calls/games,
    BIP_perGame = BIP/games,
    H_perGame = H/games,
    BIP_outs_perGame = BIP_outs/games,
    BB_perGame = BB/games,
    SO_perGame = SO/games,
    DRX_perGame = delta_run_exp/games
  )

#table of rates per game to put in post
ratesPerGame <- totalsAndRates %>%
  select(season, 
         pitches_perGame, 
         PA_perGame,
         called_strikes_perGame, 
         whiffs_perGame,
         swings_perGame, 
         pitches_oZone_perGame,
         bad_calls_perGame,
         BIP_perGame,
         H_perGame,
         BIP_outs_perGame,
         BB_perGame,
         SO_perGame) %>%
  arrange(desc(season)) %>%
  mutate(pitches_perGame = round(pitches_perGame, digits = 2), 
         PA_perGame = round(PA_perGame, digits = 2),
         called_strikes_perGame = round(called_strikes_perGame, digits = 2), 
         whiffs_perGame = round(whiffs_perGame, digits = 2),
         swings_perGame = round(swings_perGame, digits = 2), 
         pitches_oZone_perGame = round(pitches_oZone_perGame, digits = 2),
         bad_calls_perGame = round(bad_calls_perGame, digits = 2),
         BIP_perGame = round(BIP_perGame, digits = 2),
         H_perGame = round(H_perGame, digits = 2),
         BIP_outs_perGame = round(BIP_outs_perGame, digits = 2),
         BB_perGame = round(BB_perGame, digits = 2),
         SO_perGame = round(SO_perGame, digits = 2))

#saving table to include in blog post
#write.csv(ratesPerGame, "data/yelichPerGameBySeason.csv")


#going to zoom in on certain stats, and seasons, and fit some models to 
#predict some numbers for 2022

#linear models based on time series of cumulative stats
m.H.2018 <- lm(cumH ~ game_of_seas_seq, data = g %>% filter(season == 2018))
m.H.2019 <- lm(cumH ~ game_of_seas_seq, data = g %>% filter(season == 2019))
m.H.2020 <- lm(cumH ~ game_of_seas_seq, data = g %>% filter(season == 2020))
m.H.2021 <- lm(cumH ~ game_of_seas_seq, data = g %>% filter(season == 2021))
m.H.2022 <- lm(cumH ~ game_of_seas_seq, data = g %>% filter(season == 2022))

m.BB.2018 <- lm(cumBB ~ game_of_seas_seq, data = g %>% filter(season == 2018))
m.BB.2019 <- lm(cumBB ~ game_of_seas_seq, data = g %>% filter(season == 2019))
m.BB.2020 <- lm(cumBB ~ game_of_seas_seq, data = g %>% filter(season == 2020))
m.BB.2021 <- lm(cumBB ~ game_of_seas_seq, data = g %>% filter(season == 2021))
m.BB.2022 <- lm(cumBB ~ game_of_seas_seq, data = g %>% filter(season == 2022))

m.SO.2018 <- lm(cumSO ~ game_of_seas_seq, data = g %>% filter(season == 2018))
m.SO.2019 <- lm(cumSO ~ game_of_seas_seq, data = g %>% filter(season == 2019))
m.SO.2020 <- lm(cumSO ~ game_of_seas_seq, data = g %>% filter(season == 2020))
m.SO.2021 <- lm(cumSO ~ game_of_seas_seq, data = g %>% filter(season == 2021))
m.SO.2022 <- lm(cumSO ~ game_of_seas_seq, data = g %>% filter(season == 2022))

models <- list("H.2018" = m.H.2018,  
               "H.2019" = m.H.2019,  
               "H.2020" = m.H.2020,  
               "H.2021" = m.H.2021,  
               "H.2022" = m.H.2022,
               
               "BB.2018" = m.BB.2018, 
               "BB.2019" = m.BB.2019, 
               "BB.2020" = m.BB.2020, 
               "BB.2021" = m.BB.2021, 
               "BB.2022" = m.BB.2022,
               
               "SO.2018" = m.SO.2018, 
               "SO.2019" = m.SO.2019, 
               "SO.2020" = m.SO.2020, 
               "SO.2021" = m.SO.2021, 
               "SO.2022" = m.SO.2022)

#intercept
models[[1]]$coefficients[1]
models[["H.2018"]]$coefficients[1]
#slope
models[[1]]$coefficients[2]
#model names
names(models)[1]

slopes <- vector()
intercepts <- vector()

for(i in seq_along(models)){
  slopes <- c(slopes, models[[i]]$coefficients[2])
  intercepts <- c(intercepts, models[[i]]$coefficients[1])
}

names(slopes) <- names(models)
names(intercepts) <- names(models)

#color scale for plots
seasCols <- ibm
names(seasCols) <- 2018:2022



#Hits per game rates (2018-2022)
ggplot(g %>% filter(season > 2017)) + dustyBlk +
  labs(title = "Predicted Hit Totals by Season",
       subtitle = "Christian Yelich, 2018 - 2022",
       caption = "Data Source: Baseball Savant",
       tag = "Fig 7",
       x = "Game of Season",
       y = "Cumulative Hits") + 
  geom_line(aes(x = game_of_seas_seq, 
                y = cumH, 
                group = factor(season), 
                color = factor(season)),
            size = 1) + 
  scale_color_manual(values = seasCols) + 
  geom_abline(slope = slopes[1:5],
              intercept = intercepts[1:5],
              linetype = 3, color = ibm,
              size = 1)

#predictions for stats at 145 games
data.frame(slope = m.H.2022$coefficients[2])
pred.H.2022 <- predict(m.H.2022, 
        newdata = data.frame("game_of_seas_seq" = 145), 
        type = "response", 
        interval = "confidence", level = 0.9)

#Hits per game rates (2018-2022)
ggplot(g %>% filter(season > 2017)) + dustyBlk +
  labs(title = "Predicted Hit Totals by Season",
       subtitle = "Christian Yelich, 2018 - 2022",
       caption = "Data Source: Baseball Savant",
       tag = "Fig 7",
       x = "Game of Season",
       y = "Cumulative Hits") + 
  geom_line(aes(x = game_of_seas_seq, 
                y = cumH, 
                group = factor(season), 
                color = factor(season)),
            size = 1) + 
  scale_color_manual(values = seasCols) + 
  geom_abline(slope = slopes[1:5],
              intercept = intercepts[1:5],
              linetype = 2, color = ibm,
              size = 0.5) + 
  annotate("pointrange", x = 145, y = pred.H.2022[1], 
           ymin = pred.H.2022[2], ymax = pred.H.2022[3],
                    color = ibm[5], size = 0.3) + 
  annotate("text", x = 145, y = 149, label = as.character(round(pred.H.2022[1], digits = 1)),
           color = ibm[5])

pred.BB.2022 <- predict(m.BB.2022, 
                       newdata = data.frame("game_of_seas_seq" = 145), 
                       type = "response", 
                       interval = "confidence", level = 0.9)

#BB per game rates (2018-2022)
ggplot(g %>% filter(season > 2017)) + dustyBlk +
  coord_cartesian(ylim = c(0,100)) +
  labs(title = "Predicted Walk Totals by Season",
       subtitle = "Christian Yelich, 2018 - 2022",
       caption = "Data Source: Baseball Savant",
       tag = "Fig 8",
       x = "Game of Season",
       y = "Cumulative Walks") + 
  geom_line(aes(x = game_of_seas_seq, 
                y = cumBB, 
                group = factor(season), 
                color = factor(season)),
            size = 1) + 
  scale_color_manual(values = seasCols) + 
  geom_abline(slope = slopes[6:10],
              intercept = intercepts[6:10],
              linetype = 2, color = ibm,
              size = 0.5) + 
  annotate("pointrange", x = 145, y = pred.BB.2022[1], 
           ymin = pred.BB.2022[2], ymax = pred.BB.2022[3],
           color = ibm[5], size = 0.3) + 
  annotate("text", x = 145, y = 70, label = as.character(round(pred.BB.2022[1], digits = 1)),
           color = ibm[5])

pred.SO.2022 <- predict(m.SO.2022, 
                       newdata = data.frame("game_of_seas_seq" = 145), 
                       type = "response", 
                       interval = "confidence", level = 0.9)

#SO per game rates (2018-2022)
ggplot(g %>% filter(season > 2017)) + dustyBlk +
  coord_cartesian(ylim = c(0,180)) +
  labs(title = "Predicted Strikeout Totals by Season",
       subtitle = "Christian Yelich, 2018 - 2022",
       caption = "Data Source: Baseball Savant",
       tag = "Fig 9",
       x = "Game of Season",
       y = "Cumulative SO") + 
  geom_line(aes(x = game_of_seas_seq, 
                y = cumSO, 
                group = factor(season), 
                color = factor(season)),
            size = 1) + 
  scale_color_manual(values = seasCols) + 
  geom_abline(slope = slopes[11:15],
              intercept = intercepts[11:15],
              linetype = 2, color = ibm,
              size = 0.5) + 
  annotate("pointrange", x = 145, y = pred.SO.2022[1], 
           ymin = pred.SO.2022[2], ymax = pred.SO.2022[3],
           color = ibm[5], size = 0.3) + 
  annotate("text", x = 145, y = pred.SO.2022[3] + 2, label = as.character(round(pred.SO.2022[1], digits = 1)),
           color = ibm[5])

#MLB averages for 2022, from samplingMLB2022.R
avs1 <- read.csv("data/dayStats2022.csv")
avs2 <- read.csv("data/dayStatsBatter2022.csv")

avs1 <- avs1[-c(1,58),]
avs2 <- avs2[-c(1,58),]

avs1 <- avs1 %>% mutate(across(calledStrikes:badCalls, ~ .x / batters, .names = "perGame_{.col}"))
avs1.sum <- avs1 %>% summarize(across(starts_with("perGame"), mean))

t(avs1.sum)
#perGame_calledStrikes 2.4809920
#perGame_whiffs        1.6887410
#perGame_swings        7.1691125
#perGame_BIP           2.6453361
#perGame_H             0.8309705
#perGame_BIP_out       1.7699625
#perGame_BB            0.3188302
#perGame_SO            0.8636545
#perGame_outsideZone   7.4796748
#perGame_badCalls      0.2919266

ggplot(avs1) + geom_histogram(aes(x = perGame_H))

avs2.sum <- avs2 %>% summarize(across(calledStrikes:badCalls, mean))
t(avs2.sum)
#calledStrikes 2.7839297
#whiffs        1.8728174
#swings        8.0947732
#BIP           3.0139187
#H             0.9731480
#BIPouts       1.9925522
#BB            0.3741939
#SO            0.9431741
#outsideZone   8.4710530
#badCalls      0.3299872

ggplot(avs2) + geom_histogram(aes(H))
