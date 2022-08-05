
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

d <- NULL
#note to self: comment out diamond dataset in dataviz file

#Building dataset

#looking up Christian's MLB ID (592885)
#cyID <- playerid_lookup(last_name = "Yelich", first_name = "Christian")$mlbam_id
cyID <- 592885

#You can skip this part and start below
#--------------------------------------------------------


#getting the data from baseball savant data with baseballr

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
cy15 <- getData(2015)

#combining into one df
cy <- cy22 %>% union(cy21) %>% 
  union(cy20) %>% union(cy19) %>% union(cy18) %>% 
  union(cy17) %>% union(cy16) %>% union(cy15) %>%
  mutate(season = as.numeric(str_sub(game_date, start = 1, end = 4)))

write.csv(cy, "data/yelichPitches_15_22.csv")

#this dataset is also saved in my github repos at 
#https://github.com/JanLMoffet/baseballAnalysis/
#----
#close building dataset


#------------------------------------------------------
# START HERE!!! 
#------------------------------------------------------

cy <- read.csv(url("https://raw.githubusercontent.com/JanLMoffett/baseballAnalysis/master/data/yelichPitches_15_22.csv"))
#need a random sample of all MLB pitches to get avgs to compare to
av <- read.csv(url("https://raw.githubusercontent.com/JanLMoffett/baseballAnalysis/master/data/sampleSwingRates.csv"))
av

#creating a variable to identify swings vs non-swings
unique(cy$description)
# [1] "hit_into_play"           "foul"                   
#[3] "called_strike"           "swinging_strike"        
#[5] "ball"                    "blocked_ball"           
#[7] "foul_tip"                "swinging_strike_blocked"
#[9] "hit_by_pitch"            "missed_bunt"            
#[11] "foul_bunt"               "intent_ball"            
#[13] "pitchout"               

swingEvents <- c("hit_into_play", "swinging_strike", "foul",
                 "foul_tip", "swinging_strike_blocked")
missEvents <- c("swinging_strike", "swinging_strike_blocked")
foulEvents <- c("foul", "foul_tip")
hitEvents <- c("single", "double", "triple", "home_run")
xbhEvents <- c("double", "triple", "home_run")


cy <- cy %>% 
  mutate(isSwing = ifelse(description %in% swingEvents, 1, 0),
         isMiss = ifelse(description %in% missEvents, 1, 0),
         isFoul = ifelse(description %in% foulEvents, 1, 0),
         isBIP = ifelse(description == "hit_into_play", 1, 0),
         isH = ifelse(events %in% hitEvents, 1, 0),
         isXBH = ifelse(events %in% xbhEvents, 1, 0))

#Swing Rates by Season
bySeason <- cy %>% group_by(season) %>%
  summarize(pitches = n(),
            swings = sum(isSwing, na.rm = T),
            misses = sum(isMiss, na.rm = T),
            fouls = sum(isFoul, na.rm = T),
            BIP = sum(isBIP, na.rm = T),
            H = sum(isH, na.rm = T),
            XBH = sum(isXBH, na.rm = T)) %>%
  mutate(swingRt = swings/pitches,
         missRt = misses/pitches,
         foulRt = fouls/pitches,
         bipRt = BIP/pitches,
         hitRt = H/pitches,
         xbhRt = XBH/pitches) %>%
  arrange(desc(season))

bySeason

#By Season Timeplots of swing behavior
#----
cap1 = "Data Source: Baseball Savant"
sub1 = "Christian Yelich, Regular Season Games"

ggplot(bySeason) + napkin + 
  labs(title = "Swing Rate on All Pitches",
       subtitle = sub1,
       caption = cap1,
       x = "Season", y = "Swings/Pitches") +
  geom_line(aes(x = season, y = swingRt), color = ibm["blue"]) + 
  geom_point(aes(x = season, y = swingRt), color = ibm["blue"]) + 
  geom_point(data = av, aes(x = season, y = swingRate), color = ibm["orange"]) +
  geom_line(data = av, aes(x = season, y = swingRate), color = ibm["orange"]) + 
  annotate("text", x = c(2016, 2016), y = c(0.47, 0.43), label = c("MLB Average", "Yelich"), color = c(ibm["orange"], ibm["blue"])) 

ggplot(bySeason) + napkin +
  labs(title = "Swing and Miss Rate on All Pitches",
       subtitle = sub1,
       caption = cap1,
       x = "Season", y = "Swinging Strikes/Pitches") +
  geom_line(aes(x = season, y = missRt), color = ibm["blue"]) + 
  geom_point(aes(x = season, y = missRt), color = ibm["blue"]) +
  geom_point(data = av, aes(x = season, y = missRate), color = ibm["orange"]) +
  geom_line(data = av, aes(x = season, y = missRate), color = ibm["orange"]) + 
  annotate("text", x = c(2016, 2016), y = c(0.106, 0.094), label = c("MLB Average", "Yelich"), color = c(ibm["orange"], ibm["blue"])) 

#sometimes Christian puts his bat on the ball too much, hits a soft gb and gets out,
#where he would be better off missing the pitch altogether and probably getting to try again
#see the big rise in swings and missing in 2019

ggplot(bySeason) + napkin +
  labs(title = "Foul Ball Rate on All Pitches",
       subtitle = sub1,
       caption = cap1,
       x = "Season", y = "Foul Balls/Pitches") +
  geom_line(aes(x = season, y = foulRt), color = ibm["blue"]) + 
  geom_point(aes(x = season, y = foulRt), color = ibm["blue"])+
  geom_point(data = av, aes(x = season, y = foulRate), color = ibm["orange"]) +
  geom_line(data = av, aes(x = season, y = foulRate), color = ibm["orange"]) + 
  annotate("text", x = c(2016, 2016), y = c(0.183, 0.162), label = c("MLB Average", "Yelich"), color = c(ibm["orange"], ibm["blue"])) 


ggplot(bySeason) + napkin +
  labs(title = "BIP Rate on All Pitches",
       subtitle = sub1,
       caption = cap1,
       x = "Season", y = "BIP/Pitches") +
  geom_line(aes(x = season, y = bipRt), color = ibm["blue"]) + 
  geom_point(aes(x = season, y = bipRt), color = ibm["blue"])+
  geom_point(data = av, aes(x = season, y = bipRate), color = ibm["orange"]) +
  geom_line(data = av, aes(x = season, y = bipRate), color = ibm["orange"]) + 
  annotate("text", x = c(2016, 2016), y = c(0.185, 0.16), label = c("MLB Average", "Yelich"), color = c(ibm["orange"], ibm["blue"])) 

ggplot(bySeason) + napkin +
  labs(title = "Hit Rate on All Pitches",
       subtitle = sub1,
       caption = cap1,
       x = "Season", y = "H/Pitches") +
  geom_line(aes(x = season, y = hitRt), color = ibm["blue"]) + 
  geom_point(aes(x = season, y = hitRt), color = ibm["blue"])+
  geom_point(data = av, aes(x = season, y = hitRate), color = ibm["orange"]) +
  geom_line(data = av, aes(x = season, y = hitRate), color = ibm["orange"]) + 
  annotate("text", x = c(2016, 2016), y = c(0.057, 0.067), label = c("MLB Average", "Yelich"), color = c(ibm["orange"], ibm["blue"])) 

ggplot(bySeason) + napkin +
  labs(title = "Extra Base Hit Rate on All Pitches",
       subtitle = sub1,
       caption = cap1,
       x = "Season", y = "XBH/Pitches") +
  geom_line(aes(x = season, y = xbhRt), color = ibm["blue"]) + 
  geom_point(aes(x = season, y = xbhRt), color = ibm["blue"])+
  geom_point(data = av, aes(x = season, y = xbhRate), color = ibm["orange"]) +
  geom_line(data = av, aes(x = season, y = xbhRate), color = ibm["orange"]) + 
  annotate("text", x = c(2016, 2016), y = c(0.019, 0.024), label = c("MLB Average", "Yelich"), color = c(ibm["orange"], ibm["blue"])) 



#----
#close by season time plots

#were pitchers just treating him drastically differently
#in 2020 than in 2019?  
#wouldn't pitchers have started treating him differently 
#from the start of 2019 since he won 2018 MVP?

#searching for factors that may affect decision to swing

#Logistic Regressions on factor(swing)~[0,1]
#----

#logistic function to interpret results of logistic regression
logistic <- function(L){
  return((tanh(L / 2) + 1) / 2)
}

#need to divide sets in to testing and training
#first, i will mix all seasons together
#later, i will treat it more like a time series

cyInd <- 1:dim(cy)[1]
#sampling rows from dataframe index
teInd <- sample(cyInd, size = length(cyInd)*.2, replace = F)
cy.te <- cy[teInd,]
cy.tr <- cy[-teInd,]

#Does pitch type have an effect on swinging?
m1 <- glm(factor(isSwing) ~ factor(pitch_type), data = cy.tr, family = "binomial")

L_swing <- predict(m1, newdata = cy.te)
P_swing <- logistic(L_swing)

#P(swing) x pitch type
ggplot(cy.te) + napkin + 
  coord_cartesian( ylim = c(0,1)) +
  geom_point(aes(x = factor(pitch_type), y = P_swing), color = ibm["pink"]) +
  labs(title = "Probability of Swing by Pitch Type",
       subtitle = "All Pitches, Christian Yelich, 2016-2022",
       caption = "Data Source: Baseball Savant") + 
  geom_smooth(aes(x = factor(pitch_type), y = P_swing), color = ibm["orange"])

#Let's take out the weird pitch types and see what the results look like on more common ones
table(cy$pitch_type)
keep <- c("CH", "CU", "FC", "FF", "FS", "KC", "SI", "SL") 

tcy <- cy %>% filter(pitch_type %in% keep)

#split sets again
cyInd <- 1:dim(tcy)[1]
#sampling rows from dataframe index
teInd <- sample(cyInd, size = round(length(cyInd)*.2), replace = F)
cy.te <- tcy[teInd,]
cy.tr <- tcy[-teInd,]

table(factor(cy.te$pitch_type))
table(factor(cy.tr$pitch_type))

mean(cy.tr$isSwing[cy.tr$pitch_type == "CH"])

#run regression again
m1.2 <- glm(factor(isSwing) ~ factor(pitch_type), data = cy.tr, family = "binomial")

L_swing <- predict(m1.2, newdata = cy.te)
P_swing <- logistic(L_swing)

#ANOVA chi squared test shows significant effect
anova(m1.2, test = "Chisq")

#here are the probabilities of swing for each pitch type:
P_pitchType = logistic(m1.2$coefficients)
b = m1.2$coefficients[1]
P_pitchType[2:8] = logistic(b + m1.2$coefficients[2:8])


#P(swing) x pitch type
ggplot(cy.te) + napkin + 
  coord_cartesian( ylim = c(0,1)) +
  geom_point(aes(x = factor(pitch_type), y = P_swing), color = ibm["pink"]) +
  labs(title = "Probability of Swing by Pitch Type",
       subtitle = "All Pitches, Christian Yelich, 2016-2022",
       caption = "Data Source: Baseball Savant",
       x = "Pitch Type",
       y = "P(swing)") + 
  geom_smooth(aes(x = factor(pitch_type), y = P_swing), color = ibm["orange"]) + 
  annotate("text", x = 1:8, y = 0.1, label = round(P_pitchType, digits = 2))


#Does release speed?
m2 <- glm(factor(isSwing) ~ release_speed, data = cy.tr, family = "binomial")

L_swing <- predict.glm(m2, newdata = cy.te)
P_swing <- logistic(L_swing)

#P(swing) x release speed
ggplot(cy.te) + napkin + 
  coord_cartesian(xlim = c(60,110), ylim = c(0,1)) +
  geom_point(aes(x = release_speed, y = P_swing), color = ibm["pink"]) +
  labs(title = "Probability of Swing by Release Speed",
       subtitle = "All Pitches, Christian Yelich, 2016-2022",
       caption = "Data Source: Baseball Savant") + 
  geom_smooth(aes(x = release_speed, y = P_swing), color = ibm["orange"])


#digging deeper into swings
sw <- cy %>% filter(description %in% swingEvents)
