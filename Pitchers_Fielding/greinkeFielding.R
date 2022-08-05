
#Zack Greinke fielding
setwd("~/R/R Projects")

library(lubridate)
library(dplyr)
library(ggplot2)
library(stringr)
library(baseballr)
source("plot_colors.R")
source("mlb_functions.R")

pl21 <- read.csv("data/fg_pitching_leaders_2021.csv")
pl20 <- read.csv("data/fg_pitching_leaders_2020.csv")
pl19 <- read.csv("data/fg_pitching_leaders_2019.csv")
pl18 <- read.csv("data/fg_pitching_leaders_2018.csv")
pl17 <- read.csv("data/fg_pitching_leaders_2017.csv")
pl16 <- read.csv("data/fg_pitching_leaders_2016.csv")
pl15 <- read.csv("data/fg_pitching_leaders_2015.csv")
#These datasets were acquired in "scraping_fangraphs.R"

#combining into one dataset
fg <- pl21 %>% 
  union(pl20) %>%
  union(pl19) %>% 
  union(pl18) %>%
  union(pl17) %>%
  union(pl16) %>%
  union(pl15)

names(fg)

pct_vars <- c("LD_pct", "GB_pct", "FB_pct", 
              "Pull_pct", "Cent_pct", "Oppo_pct",   
              "Soft_pct", "Med_pct", "Hard_pct")


#Fangraphs documentation of pitching variables
#https://library.fangraphs.com/pitching/complete-list-pitching/

#some new variables
fg <- fg %>% 
  mutate(
    BIP = GB + LD + FB,
    Swings = (Swing_pct/100)*Pitches
  ) %>%
  mutate(
    BIP_over_P = round((BIP / Pitches)*100, digits=1),
    P_over_IP = Pitches / IP
  ) 

  

#~~~~~~~~~~~~~~~~~~~~Contact Pct
#Fangraphs' Contact% = Contact Swings/Total Swings


#----

#Zack's variables by season
zg <- fg %>% filter(Name == "Zack Greinke")

zg %>% arrange(desc(Season)) %>%
       select(Name, Season, Pitches, BIP,
              Swing_pct, Contact_pct, BIP_over_P, 
              GB_pct, LD_pct, FB_pct,
              Soft_pct, Med_pct, Hard_pct,
              Pull_pct, Cent_pct, Oppo_pct
              )
  
ggplot(zg) + coord_cartesian(xlim = c(2015,2022), ylim = c(10,30)) + 
  geom_line(aes(Season, BIP_over_P))

#distribution of BIP/P
zg_BIP_over_P <- fg %>% filter(Name == "Zack Greinke") %>%
  select(Season, BIP_over_P)
mean_BIP_over_P <- fg %>% group_by(Season) %>%
  summarize(m = mean(BIP_over_P, na.rm = T))

myHist <- function(a.Season){
  ggplot(fg %>% filter(Season == a.Season)) + 
    geom_histogram(aes(BIP_over_P)) +
    geom_vline(xintercept = mean_BIP_over_P$m[mean_BIP_over_P$Season == a.Season], 
               color = lavender) +
    geom_vline(xintercept = zg_BIP_over_P$BIP_over_P[zg_BIP_over_P$Season == a.Season], 
               color = fire_orange)}

ggplot(fg) + 
  geom_boxplot(aes(factor(Season), BIP_over_P)) + 
  annotate("point", 
           x = factor(zg_BIP_over_P$Season), 
           y = zg_BIP_over_P$BIP_over_P, 
           pch = 3, size = 4, color = fire_orange) +
  annotate("point", 
           x = factor(zg_BIP_over_P$Season), 
           y = zg_BIP_over_P$BIP_over_P, 
           pch = 3, size = 4, color = fire_orange) 

ggplot(fg) + 
  geom_histogram(aes(BIP_over_P)) +
  geom_vline(xintercept = mean_BIP_over_P$m, 
             color = grey1) +
  geom_vline(xintercept = zg_BIP_over_P$BIP_over_P, 
             color = fire_orange) 

myHist(2015)
myHist(2016)
myHist(2017)
myHist(2018)
myHist(2019)
myHist(2020)
myHist(2021)

ggplot(fg) + 
  geom_boxplot(aes(factor(Season), Soft_pct))

zgplot + geom_line() + 
  geom_point()

#
median(zg %>% pull(Contact_pct))


#! need time plot of Greinke contact pct (by year)
#on top of dataset avg's

#where do those numbers fall in the distribution?
#----
#summary stats overall
summary(fg$Contact_pct)
fvnm <- fivenum(fg$Contact_pct)

#distribution plots
p <- ggplot(fg, aes(Contact_pct))
thisTitle <- "Distribution of Contact Percentages by Pitcher-Season"
thisSubtitle <- "Starting Pitchers with > 100 IP | 2015-2019, 2021 Seasons"
quantAnn <- c(paste0("[Min: ", fvnm[1], "%]"), 
              paste0("[Q1: ", fvnm[2], "%]"), 
              paste0("[Med: ", fvnm[3], "%]"),
              paste0("[Q3: ", fvnm[4], "%]"),
              paste0("[Max: ", fvnm[5], "%]"))

#boxplot of contact pcts
p + coord_cartesian(xlim = c(60,95)) + 
  geom_boxplot(fill = pale_orange) + 
  clean_laundry + 
  labs(title = thisTitle,
       subtitle = thisSubtitle)
#density plot of contact pcts
p + coord_cartesian(xlim = c(60,95)) + 
  geom_density(fill = pale_orange) + 
  geom_vline(xintercept = fvnm, col = fire_orange, lty = 2) + 
  annotate("rect", xmin = 60, xmax = 95,
           ymin = -.02, ymax = 0, fill = "white") + 
  annotate("text", x = fvnm, y = c(-.005,-.015,-.005,-.015, -.005), label = quantAnn,
           size = 3.5) + 
  clean_laundry + 
  labs(title = thisTitle,
       subtitle = thisSubtitle,
       y = "Density") + 
  geom_hline(yintercept = 0, lty = 1)

#bar plot of contact pcts
p + coord_cartesian(xlim = c(60,95)) + 
  geom_bar(fill = pale_orange, width = 0.1) + 
  geom_vline(xintercept = fvnm, col = fire_orange, lty = 2) + 
  annotate("rect", xmin = 60, xmax = 95,
           ymin = -2, ymax = 0, fill = "white") + 
  annotate("text", x = fvnm, y = c(-.5, -1.5, -.5, -1.5, -.5), 
           label = quantAnn, size = 3.5) + 
  clean_laundry + 
  labs(title = thisTitle,
       subtitle = thisSubtitle,
       y = "") + 
  geom_hline(yintercept = 0, lty = 1)

#----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~Ground Ball Pct
#Fangraphs' GB% = GB/BIP
#my GB_pct2 = GB/Total Pitches

#Zack's numbers by season
fg %>% filter(Name == "Zack Greinke") %>% arrange(desc(Season)) %>%
  select(Name, Season, Pitches, GB_pct, Contact_pct)
#summary treating contact pct as var
summary(fg %>% filter(Name == "Zack Greinke") %>% pull(Contact_pct))

fg %>% filter(Name == "Zack Greinke") %>%
  arrange(desc(Season)) %>%
  mutate(GB_pct2 = GB/Pitches) %>%
  select(Name, Season, Pitches, GB_pct, GB_pct2, Contact_pct)

#summary stats by season
fg %>%
  group_by(Season) %>%
  summarize(
    Min = min(GB_pct, na.rm = T),
    Q1 = quantile(GB_pct, probs = 0.25, na.rm = T),
    Mean = mean(GB_pct, na.rm = T),
    Median = median(GB_pct, na.rm = T),
    Q3 = quantile(GB_pct, probs = 0.75, na.rm = T),
    Max = max(GB_pct, na.rm = T))


summary(fg$GB_pct)
fvnm <- fivenum(fg$GB_pct)

#zack's numbers, by season
fg %>% filter(Name == "Zack Greinke") %>%

#distribution plots
p <- ggplot(fg, aes(GB_pct))
thisTitle <- "Distribution of Ground Ball Percentages by Pitcher-Season"
thisSubtitle <- "Starting Pitchers with > 100 IP | 2015-2019, 2021 Seasons"
quantAnn <- c(paste0("[Min: ", fvnm[1], "%]"), 
              paste0("[Q1: ", fvnm[2], "%]"), 
              paste0("[Med: ", fvnm[3], "%]"),
              paste0("[Q3: ", fvnm[4], "%]"),
              paste0("[Max: ", fvnm[5], "%]"))

#boxplot of contact pcts
p + coord_cartesian(xlim = c(20,70)) + 
  geom_boxplot(fill = pale_orange) + 
  clean_laundry + 
  labs(title = thisTitle,
       subtitle = thisSubtitle)
#density plot of contact pcts
p + coord_cartesian(xlim = c(20,70)) + 
  geom_density(fill = pale_orange) + 
  geom_vline(xintercept = fvnm, col = fire_orange, lty = 2) + 
  annotate("rect", xmin = 20, xmax = 70, 
           ymin = -.02, ymax = 0, fill = "white") + 
  annotate("text", x = fvnm, y = c(-.005,-.015,-.005,-.015, -.005), label = quantAnn,
           size = 3.5) + 
  clean_laundry + 
  labs(title = thisTitle,
       subtitle = thisSubtitle,
       y = "Density") + 
  geom_hline(yintercept = 0, lty = 1)

#bar plot of contact pcts
p + coord_cartesian(xlim = c(20,70)) + 
  geom_bar(fill = pale_orange, width = 0.1) + 
  geom_vline(xintercept = fvnm, col = fire_orange, lty = 2) + 
  annotate("rect", xmin = 60, xmax = 95,
           ymin = -2, ymax = 0, fill = "white") + 
  annotate("text", x = fvnm, y = c(-.5, -1.5, -.5, -1.5, -.5), 
           label = quantAnn, size = 3.5) + 
  clean_laundry + 
  labs(title = thisTitle,
       subtitle = thisSubtitle,
       y = "") + 
  geom_hline(yintercept = 0, lty = 1)

#summary stats by season
fg %>%
  group_by(Season) %>%
  summarize(
    Min = min(GB_pct, na.rm = T),
    Q1 = quantile(GB_pct, probs = 0.25, na.rm = T),
    Mean = mean(GB_pct, na.rm = T),
    Median = median(GB_pct, na.rm = T),
    Q3 = quantile(GB_pct, probs = 0.75, na.rm = T),
    Max = max(GB_pct, na.rm = T))

#----


#questions to answer:

#what % of total pitches were put in play?
#what % of total pitches were straightaway gb and ld (fieldable by pitcher)?
#what % of total pitches were converted into outs by pitcher?

#what other metrics on fg can give an indication of a pitcher's fielding
#ability and where does greinke rank using these metrics?

#straight up # of field outs, # of field outs/innings pitched, 
# or # of fields outs over total outs (ip*3)?

#using statcast, look at the launch speed on BIPs turned into outs by greinke,
#pick a few other guys to compare
MLBID_Greinke <- get_mlbid("Greinke")
MLBID_Keuchel <- get_mlbid("Keuchel")
MLBID_Cole <- get_mlbid("Gerrit Cole")
MLBID_Bieber <- get_mlbid("Shane Bieber")
MLBID_Mize <- get_mlbid("Casey Mize")
MLBID_Ohtani <- get_mlbid("Ohtani")

#Shohei's id comes up twice 
MLBID_Ohtani <- MLBID_Ohtani[1]

#saving search as a function to simplify code
scSearch <- function(fourDigitYr, mlbId){
  stdt <- paste0(fourDigitYr,"-01-01")
  endt <- paste0(fourDigitYr,"-12-31")
  #using baseballr statcast search
  q <- statcast_search(start_date = stdt, end_date = endt,
                       player_type = "pitcher", playerid = mlbId)
  #narrowing down variables
  q <- q %>% select(
                    #identifying vars
                    player_name,
                    game_date, 
                    at_bat_number,
                    pitch_number,
                    pitcher,
                    batter,
                    #vars describing the pitch
                    pitch_type,
                    pitch_name,
                    release_speed,
                    release_pos_x,
                    release_pos_y,
                    release_pos_z,
                    #vars describing the circumstances (to filter for dp opportunities, 
                    #runners on vs clean)
                    on_3b,
                    on_2b,
                    on_1b,
                    if_fielding_alignment,
                    balls,
                    strikes,
                    outs_when_up,
                    #vars describing the batted ball
                    type,
                    bb_type,
                    estimated_ba_using_speedangle,
                    estimated_woba_using_speedangle,
                    woba_value,
                    babip_value,
                    launch_speed,
                    launch_angle,
                    #vars describing the results
                    hc_x,
                    hc_y,
                    hit_location,
                    events,
                    des)
  
}

#Zack Greinke's pitches for 2018, 2019, and 2021
zg21 <- scSearch(2021, MLBID_Greinke)


#general dataset to get league distributions
#taking a week's worth of pitches from the middle of 2021
d.jun <- scrape_statcast_savant(start_date = "2021-06-01", end_date = "2021-06-07", player_type = "pitcher")
d.jul <- scrape_statcast_savant(start_date = "2021-07-01", end_date = "2021-07-07", player_type = "pitcher")
d.aug <- scrape_statcast_savant(start_date = "2021-08-01", end_date = "2021-08-07", player_type = "pitcher")


d <- d.jun %>% union(d.jul) %>% union(d.aug) %>%
  select(
    #identifying vars
    player_name,
    game_date, 
    at_bat_number,
    pitch_number,
    pitcher,
    batter,
    #vars describing the pitch
    pitch_type,
    pitch_name,
    release_speed,
    release_pos_x,
    release_pos_y,
    release_pos_z,
    #vars describing the circumstances (to filter for dp opportunities, 
    #runners on vs clean)
    on_3b,
    on_2b,
    on_1b,
    if_fielding_alignment,
    balls,
    strikes,
    outs_when_up,
    #vars describing the batted ball
    type,
    bb_type,
    estimated_ba_using_speedangle,
    estimated_woba_using_speedangle,
    woba_value,
    babip_value,
    launch_speed,
    launch_angle,
    #vars describing the results
    hc_x,
    hc_y,
    hit_location,
    events,
    des)

#adjusting hc_y variable
d <- d %>% mutate(hc_y2 = 250 - hc_y)
zg21 <- zg21 %>% mutate(hc_y2 = 250 - hc_y)

#filter for hit_location to pitcher
d.to1 <- d %>% filter(hit_location == 1)
toZg <- zg21 %>% filter(hit_location == 1)

#how is hc_x distributed?
summary(d.to1$hc_x)
f <- fivenum(d.to1$hc_x)
q <- quantile(d.to1$hc_x, probs = c(.05, .95), na.rm = T)

#lines(x = c(127,250), y = c(50,175))
#lines(x = c(0,127), y = c(175,50))
ggplot(d.to1) + coord_cartesian(xlim = c(0,250), ylim = c(0,250)) + 
  geom_point(aes(x = hc_x, y = hc_y2)) + 
  annotate("segment", x = 127, xend = 250,
           y = 50, yend = 175, color = CLR_grey2) + 
  annotate("segment", x = 0, xend = 127,
           y = 175, yend = 50, color = CLR_grey2) + 
  geom_vline(xintercept = f, color = CLR_lavender) + 
  geom_point(data = toZg, aes(x = hc_x, y = hc_y2), color = CLR_fire_orange)

ggplot(d.to1) + geom_boxplot(aes(x = hc_x))

ggplot(d.to1) + coord_cartesian() + 
  geom_histogram(aes(hc_x)) + 
  geom_vline(xintercept = f, color = CLR_lavender)

f

#how is launch_angle distributed?
summary(d.to1$launch_angle)
f <- fivenum(d.to1$launch_angle)

#histogram
ggplot(d.to1) + coord_cartesian() + 
  geom_histogram(aes(launch_angle)) + 
  geom_vline(xintercept = f, color = CLR_lavender)
#histogram, filtering for launch speed
ggplot(d.to1 %>% filter(launch_speed <85)) + coord_cartesian() + 
  geom_histogram(aes(launch_angle)) + 
  geom_vline(xintercept = f, color = CLR_lavender)
#histogram, filtering for batted ball type
ggplot(d.to1 %>% filter(bb_type == "ground_ball")) + coord_cartesian() + 
  geom_histogram(aes(launch_angle)) + 
  geom_vline(xintercept = f, color = CLR_lavender)
#histogram, filtering for both
ggplot(d.to1 %>% filter(launch_speed <50, bb_type == "ground_ball")) + coord_cartesian() + 
  geom_histogram(aes(launch_angle)) + 
  geom_vline(xintercept = f, color = CLR_lavender)
#density plot of same thing
f <- fivenum(d.to1 %>% filter(launch_speed <50, bb_type == "ground_ball"))
ggplot(d.to1 %>% filter(launch_speed <50, bb_type == "ground_ball")) + coord_cartesian() + 
  geom_density(aes(launch_angle)) + 
  geom_vline(xintercept = f, color = CLR_lavender)


ggplot(d.to1) + coord_cartesian(xlim = c(-100,100), ylim = c(0,110)) + 
  geom_point(aes(launch_angle, launch_speed, color = events)) + 
  geom_vline(xintercept = c(-87,8), type = 3, color = CLR_lavender)

#how is launch_speed distributed?
summary(d.to1$launch_speed)
f <- fivenum(d.to1$launch_speed)

ggplot(d.to1) + coord_cartesian(xlim = c(-100,100), ylim = c(0,110)) + 
  geom_point(aes(launch_angle, launch_speed, color = bb_type)) + 
  geom_vline(xintercept = c(-87,8), type = 3, color = CLR_lavender)

ggplot(d.to1) + coord_cartesian() + 
  geom_histogram(aes(launch_speed)) + 
  geom_vline(xintercept = f, color = CLR_lavender)


#using 90% confidence bounds to set filter on hc_x 
q
#  5% = 116.4
# 95% = 135.7

#set of pitches that are "straightaway"
d.sa <- d %>% filter(between(hc_x, 116.4, 135.7))
d.sa <- d.sa %>% filter(launch_angle < 7)
d.plot <- d.sa

ggplot(d.plot) + coord_cartesian(xlim = c(0,250), ylim = c(0,250)) + 
  geom_point(aes(x = hc_x, y = hc_y2)) + 
  annotate("segment", x = 127, xend = 250,
           y = 50, yend = 175, color = CLR_grey2) + 
  annotate("segment", x = 0, xend = 127,
           y = 175, yend = 50, color = CLR_grey2) + 
  geom_vline(xintercept = f, color = CLR_lavender) + 
  geom_point(data = toZg, aes(x = hc_x, y = hc_y2), color = CLR_fire_orange)

unique(d.sa$events)
out_events <- c("field_out", "force_out",
                "grounded_into_double_play",
                "fielders_choice_out",
                "double_play")

d.sa <- d.sa %>% 
  mutate(is_out = ifelse(events %in% out_events, 1, 0),
         to1 = ifelse(hit_location == 1, 1, 0))

ggplot(d.plot) + coord_cartesian(xlim = c(0,250), ylim = c(0,250)) + 
  geom_point(aes(x = hc_x, y = hc_y2, color = factor(is_out))) + 
  annotate("segment", x = 127, xend = 250,
           y = 50, yend = 175, color = CLR_grey2) + 
  annotate("segment", x = 0, xend = 127,
           y = 175, yend = 50, color = CLR_grey2) + 
  geom_vline(xintercept = f, color = CLR_lavender) + 
  geom_point(data = toZg, aes(x = hc_x, y = hc_y2), 
             color = CLR_fire_orange)

d.sa %>% group_by(to1) %>%
  mutate(is_out = ifelse(events %in% out_events, 1, 0)) %>%
  summarize(n = n(),
            outs = sum(is_out, na.rm = T)) %>%
  mutate(out_pct = outs/n)

