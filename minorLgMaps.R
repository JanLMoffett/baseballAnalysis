
library(baseballr)
library(tidyverse)
library(maps)
library(mapdata)
library(usmap)
library(rworldmap)
library(sf)
library(devtools)
library(lubridate)

source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/datavizExtras.R")

#datasets from mlb stats api, using baseballr
#----
#"sports" in this context means different league levels (ie minors, independent, 14U, etc.)
sports <- mlb_sports()

sports %>% select(sport_name, sport_id)

t.1 <- mlb_teams(sport_ids = 1)
t.11 <- mlb_teams(sport_ids = 11)
t.12 <- mlb_teams(sport_ids = 12)
t.13 <- mlb_teams(sport_ids = 13)
t.14 <- mlb_teams(sport_ids = 14)
t.16 <- mlb_teams(sport_ids = 16)

#t.1 has more cols than the other dfs
add2min <- setdiff(names(t.1), names(t.11))
add2maj <- setdiff(names(t.11), names(t.1))

t.min <- union(t.11, t.12) %>% union(t.13) %>% union(t.14) %>% union(t.16)
dim(t.min)[1]
#203 rows
add2min
#6 variables

v <- rep("", dim(t.min)[1]*length(add2min))
m <- matrix(v, nrow = dim(t.min)[1], ncol = length(add2min))
colnames(m) <- add2min

t.min <- cbind(t.min, data.frame(m))
t.min <- t.min %>%
  mutate(spring_venue_id = NA,
         spring_league_id = NA)



dim(t.1)[1]
#30 rows
add2maj
#2 variables

v <- rep("", dim(t.1)[1]*length(add2maj))
m <- matrix(v, nrow = dim(t.1)[1], ncol = length(add2maj))
colnames(m) <- add2maj

t.maj <- cbind(t.1, data.frame(m))

t.maj <- t.maj %>%
  mutate(parent_org_id = NA)

#now they should have the same columns
t <- union(t.maj, t.min)

#saving this dataset so i don't have to do it again
#write.csv(t, "data/mlbTeams2022.csv")
#----

#i'm going to work on the dataset in excel, since i need to enter some variables manually


#i've added lat and long for teams in US
#still need to work on DR teams

# Start here!!!
teams <- read.csv("data/mlbTeams2022edited.csv")

states <- map_data("state")
canada <- map_data("world", region = "canada")
mexico <- map_data("world", region = "Mexico")
puertoRico <- map_data("world", region = "Puerto Rico")

#i need a map of the US
usBase <- ggplot() + napkin + theme(legend.position = "bottom") +
  geom_polygon(data=states, aes(x=long, y=lat, group=group),
                color="black", fill=jmbn["blush"]) +
  geom_polygon(data=canada, aes(x=long, y=lat, group=group),
               color="black", fill=jmbn["blush"]) +
  geom_polygon(data=mexico, aes(x=long, y=lat, group=group),
               color="black", fill=jmbn["blush"]) +
  coord_sf(xlim = c(-125, -65), 
           ylim = c(25,50))
  
#team locations, by league level
usBase + geom_point(data = teams, 
                    aes(x = long, y = lat, 
                        color = factor(sport_id)))
  
#major league and triple A teams
usBase + geom_point(data = teams %>% 
                      filter(sport_id %in% c(1,11)), 
                    aes(x = long, y = lat, 
                        color = factor(sport_id)))

#triple a teams
usBase + geom_point(data = teams %>% 
                      filter(sport_id == 11), 
                    aes(x = long, y = lat, 
                        color = factor(sport_id)))

#double a teams
dbA <- teams %>% filter(sport_id == 12)
usBase + 
  labs(title = "MLB Affiliated Team Locations",
       subtitle = "Double A, 2022") + 
  geom_point(data = dbA, 
             aes(x = long, y = lat, 
             color = factor(league_name))) + 
  annotate("text", x = dbA$long, y = dbA$lat, 
           label = dbA$parent_abbr, size = 2.8)
