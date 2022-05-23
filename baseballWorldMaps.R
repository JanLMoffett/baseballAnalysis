
library(baseballr)
library(tidyverse)
library(maps)
library(mapdata)

#importing colors and themes for plots
#github.com/JanLMoffett/datavizExtras
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/datavizExtras.R")


# Getting and cleaning data
#-----
#MLB's players in 2022, from MLB using baseballr:
d.mlb <- mlb_sports_players(sport_id = 1, season = 2022)

#codes for other leagues
mlbSports <- mlb_sports()
mlbSports %>% select(sport_id, sport_name)

#here's the url where this info is: https://statsapi.mlb.com/api/v1/sports/11/players
#url for the mlb api: "https://statsapi.mlb.com/api/{ver}/sports/{sportId}/players"

d.3a <- mlb_sports_players(sport_id = 11, season = 2022)
d.2a <- mlb_sports_players(sport_id = 12, season = 2022)
d.ha <- mlb_sports_players(sport_id = 13, season = 2022)
d.1a <- mlb_sports_players(sport_id = 14, season = 2022)
d.rk <- mlb_sports_players(sport_id = 16, season = 2022)

#adding a league variable to classify these when i combine into one df
#also removing all players that aren't active
addLeague <- function(dx, lgName){
  return(dx %>% mutate(league = lgName) %>%
           filter(active == T))
}

d.mlb <- addLeague(d.mlb, "MLB")
d.3a <- addLeague(d.3a, "Triple_A")
d.2a <- addLeague(d.2a, "Double_A")
d.ha <- addLeague(d.ha, "High_A")
d.1a <- addLeague(d.1a, "Single_A")
d.rk <- addLeague(d.rk, "Rookie")

#these dfs have different numbers of columns, need to make same before appending
setdiff(names(d.3a), names(d.mlb))
setdiff(names(d.2a), names(d.mlb))

setdiff(names(d.3a), names(d.rk))
setdiff(names(d.3a), names(d.rk))
#last_played_date and current_team_name are missing in some

#i'm interested in variables that describe the player's identity and origins
chooseVars <- function(dt){
  return(dt %>% select(player_id,
                       league,
                       active,
                       use_name,
                       first_name, 
                       middle_name,
                       last_name,
                       primary_position_name,
                       link,
                       birth_date,
                       birth_city,
                       birth_state_province,
                       birth_country,
                       draft_year,
                       mlb_debut_date))
  
}
  
d.mlb <- chooseVars(d.mlb) 
d.3a <- chooseVars(d.3a)
d.2a <- chooseVars(d.2a)
d.ha <- chooseVars(d.ha)
d.1a <- chooseVars(d.1a)
d.rk <- chooseVars(d.rk)

#done getting the data

#now I can combine them into one df

#as i add each league, i need to make sure I'm not 
#adding the same player to the data more than once
ids.mlb <- unique(d.mlb$player_id)
ids.3a <- unique(d.3a$player_id)
ids.2a <- unique(d.2a$player_id)
ids.ha <- unique(d.ha$player_id)
ids.1a <- unique(d.1a$player_id)
ids.rk <- unique(d.rk$player_id)

length(ids.mlb)#1118
length(ids.3a) #1338
length(ids.2a) #1047
length(ids.ha) #1053
length(ids.1a) #1132
length(ids.rk) #513

#i only want to keep the row from each player's highest league
#so I'll assign a highest league var to each unique id
#starting with MLB
ids <- data.frame(player_id = ids.mlb, highest_league = "MLB")

#add tripleA
ids.3a <- setdiff(ids.3a, ids.mlb)
length(ids.3a) #934

tids <- data.frame(player_id = ids.3a, highest_league = "Triple_A")
ids <- union(ids, tids)

#doubleA
ids.2a <- setdiff(ids.2a, ids.3a) 
ids.2a <- setdiff(ids.2a, ids.mlb)
length(ids.2a) #871

tids <- data.frame(player_id = ids.2a, highest_league = "Double_A")
ids <- union(ids, tids)

#highA
ids.ha <- setdiff(ids.ha, ids.2a)
ids.ha <- setdiff(ids.ha, ids.3a)
ids.ha <- setdiff(ids.ha, ids.mlb)
length(ids.ha) #927

tids <- data.frame(player_id = ids.ha, highest_league = "High_A")
ids <- union(ids, tids)

#singleA
ids.1a <- setdiff(ids.1a, ids.ha) 
ids.1a <- setdiff(ids.1a, ids.2a)
ids.1a <- setdiff(ids.1a, ids.3a)
ids.1a <- setdiff(ids.1a, ids.mlb)
length(ids.1a) #959

tids <- data.frame(player_id = ids.1a, highest_league = "Single_A")
ids <- union(ids, tids)

#rookie
ids.rk <- setdiff(ids.rk, ids.1a)
ids.rk <- setdiff(ids.rk, ids.ha)
ids.rk <- setdiff(ids.rk, ids.2a)
ids.rk <- setdiff(ids.rk, ids.3a)
ids.rk <- setdiff(ids.rk, ids.mlb)
length(ids.rk) #454

tids <- data.frame(player_id = ids.rk, highest_league = "Rookie")
ids <- union(ids, tids)

#and then join the two datasets, and filter out all the rows with league not matching highest league
d <- d.mlb %>% union(d.3a) %>% union(d.2a) %>%
    union(d.ha) %>% union(d.1a) %>% union(d.rk)
d <- d %>%
    left_join(ids, by = "player_id") %>%
    filter(league == highest_league)


#that gives 5263 players rn (05/23/22)
N <- dim(d)[1]

#some of these levels may be redundant, need to fix before plotting
#country codes like DOM need to be changed to names

#code countries
cc <- unique(d$birth_country[which(str_length(d$birth_country) < 4)])
cc
#name countries
setdiff(unique(d$birth_country), cc)

#need to assign names to code countries, and make them match map data
d <- d %>% mutate(birth_country2 = case_when(
  birth_country == "DOM" ~ "Dominican Republic",
  birth_country == "MEX" ~ "Mexico",
  birth_country == "PAN" ~ "Panama",
  birth_country == "Panama Canal Zone" ~ "Panama",
  birth_country == "BAH" ~ "Bahamas",
  birth_country == "NED" ~ "Netherlands",
  birth_country == "VEN" ~ "Venezuela",
  birth_country == "NCA" ~ "Nicaragua",
  birth_country == "CUB" ~ "Cuba",
  birth_country == "PUR" ~ "Puerto Rico",
  birth_country == "CAN" ~ "Canada",
  birth_country == "CUW" ~ "Curacao",
  birth_country == "Netherlands Antilles" ~ "Curacao",
  birth_country == "HKG" ~ "Hong Kong",
  birth_country == "TPE" ~ "Taiwan",
  birth_country == "COL" ~ "Colombia",
  birth_country == "AUS" ~ "Australia",
  birth_country == "Republic of Korea" ~ "South Korea",
  birth_country == "United Kingdom" ~ "UK",
  TRUE ~ birth_country
))

#now all player id's are unique, and countries only have one name each
#write.csv(d, "data/affiliatedPlayers2022.csv")
#----

#################################################
#START HERE

d <- read.csv("data/affiliatedPlayers2022.csv")

#let's see where everyone's from
byCountry <- d %>%
  group_by(birth_country2) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  rename(birth_country = birth_country2)

byCountry <- na.omit(byCountry)
byCountry

#going to use the byCountry dataframe to plot some maps

#getting map data with ggplot2 function
worldMap <- map_data("world")

#i need to add the rest of the countries back in to the df so they
#show up on the map

#differentiating Hong Kong from China in map data
worldMap$region[which(worldMap$subregion == "Hong Kong")] <- "Hong Kong"

#names of countries in map data and player dfs need to match
#wm <- unique(worldMap$region)
#pd <- unique(byCountry$birth_country)

#these countries match
#intersect(pd, wm)

#these countries don't
#setdiff(pd, wm)
#this should be 'character(0)'

#save table to put in blog post
#write.csv(byCountry, "data/affPlyrsByCountry2022.csv")

names(byCountry)
#need to append rows of 0 for all the other countries
unique(worldMap$region)


#names of countries in map data and player dfs need to match
wm <- unique(worldMap$region)
pd <- unique(byCountry$birth_country)

#need df of countries with 0 players
zeroCountries <- setdiff(wm, pd)
zc <- data.frame(birth_country = zeroCountries, n = 0)

bc <- union(byCountry, zc)
#going to make the US NA so it doesn't wreck the curve
#amt was 3286
bc$n[bc$birth_country == "USA"] <- NA
bc$n[bc$n == 0] <- NA
#changing all 0 to NA to make map look better

ggplot(bc, aes(fill = n, map_id = birth_country)) + 
  juiceGlass +
  geom_map(map = worldMap, color = oj["blueK"]) + 
  expand_limits(x = worldMap$long, y = worldMap$lat) + 
  scale_fill_gradient(low = oj["orange0"], high = oj["orange5"], na.value = oj["blue0"])

#how do I zoom in on certain parts?
#have to subset the map df and the value df with only the regions you want
#we don't need antarctica

bc <- bc %>% filter(birth_country != "Antarctica")
wm <- worldMap %>% filter(region != "Antarctica")

ggplot(bc, aes(fill = n, map_id = birth_country)) + 
  juiceGlass +
  geom_map(map = wm, color = oj["blueK"]) + 
  expand_limits(x = wm$long, y = wm$lat) + 
  scale_fill_gradient(low = oj["orange0"], high = oj["orange5"], na.value = oj["blue0"]) +
  labs(title = "Birth Countries of Baseball Players",
       subtitle = "Major and Minor Leagues (Affiliated), 2022",
       caption = "Data Source: MLB Stats API")

