
library(baseballr)
library(tidyverse)
library(maps)
library(mapdata)
library(rworldmap)
library(sf)
library(devtools)

source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/datavizExtras.R")


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
?map_data

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
bc <- bc %>% filter(birth_country != "Antarctica")
wm <- worldMap %>% filter(region != "Antarctica")

#how do I zoom in on certain parts?
#have to subset the map df and the value df with only the regions you want

#World Map
ggplot(bc, aes(fill = n, map_id = birth_country)) + 
  napkin +
  geom_map(map = wm, color = oj["blueK"]) + 
  expand_limits(x = wm$long, y = wm$lat) + 
  scale_fill_gradient2(low = jmbn["periwinkle"], mid = jmbn["rose"], high = oj["orange5"], na.value = "grey99", midpoint = 425) +
  labs(title = "Birth Countries of MLB Players from Outside US",
       subtitle = "Major and Minor Leagues (Affiliated), 2022",
       caption = "Data Source: MLB Stats API",
       x = "Longitude", y = "Latitude")

#need to isolate and map regions separately
#this dataset is from the rworldmap package:
regs <- countryRegions
#this var has some of the categories i want
unique(regs$GEO3major)
#let's try to isolate latin america and the caribbean
lacCountries <- regs$ADMIN[which(regs$GEO3major == "Latin America and the Caribbean")]
lacCountries <- c(lacCountries, "USA")
lacCountries[which(lacCountries == "The Bahamas")] <- "Bahamas"

#filter map data rows for lac countries
lac.mp <- worldMap %>% filter(region %in% lacCountries)
#filter player data rows for lac countries
lac.bc <- bc %>% filter(birth_country %in% lacCountries)

#labels for map
lac.labels <- data.frame(
  include_label = c(0,0,0,1,0,
                    0,1,0,1,0,
                    0,0,1,0,1,
                    1,0,1,0,1,
                    0,0,0,0,0,
                    1,1,1,0,1,
                    1,0,1,1,0,
                    0,0,0,1,0,
                    0,0,0,1,0,
                    0),
  row.names = lacCountries)

lac.labels <- lac.labels %>%
  filter(include_label == 1) %>% 
  mutate(
    cx = 0,
    cy = 0
  )

lac.labels["Aruba", 2:3] <- c( -67, 12)
lac.labels["Bermuda", 2:3] <- c( -69, 31)
lac.labels["Brazil", 2:3] <- c( -58, -10)
lac.labels["Colombia", 2:3] <- c( -76, 5)          
lac.labels["Cuba", 2:3] <- c( -78, 22)
lac.labels["Curacao", 2:3] <- c( -60, 11)
lac.labels["Dominican Republic", 2:3] <- c( -70, 20)
lac.labels["El Salvador", 2:3] <- c(-95, 14)
lac.labels["Honduras", 2:3] <- c( -86, 16)
lac.labels["Jamaica", 2:3] <- c( -80, 17)
lac.labels["Mexico", 2:3] <- c( -105, 25)
lac.labels["Nicaragua", 2:3] <- c( -84, 13)
lac.labels["Panama", 2:3] <- c( -83, 7)
lac.labels["Peru", 2:3] <- c(-77,-8)
lac.labels["Puerto Rico", 2:3] <- c( -66, 17)
lac.labels["Bahamas", 2:3] <- c( -78, 25)             
lac.labels["Venezuela", 2:3] <- c( -69, 8)

ggplot(lac.bc, aes(fill = n, map_id = birth_country)) + 
  napkin +
  geom_map(map = lac.mp, color = oj["blueK"]) + 
  expand_limits(x = lac.mp$long, y = lac.mp$lat) + 
  scale_fill_gradient2(low = jmbn["periwinkle"], mid = jmbn["rose"], high = oj["orange5"], na.value = "grey99", midpoint = 425) +
  labs(title = "Birth Countries of MLB Players from Latin America\nand the Caribbean",
       subtitle = "Major and Minor Leagues (Affiliated), 2022",
       caption = "Data Source: MLB Stats API",
       x = "Longitude", y = "Latitude") + 
  coord_sf(xlim = c(-118,-38), ylim = c(-32,30)) + 
  annotate("text", 
           label = row.names(lac.labels),
           x = lac.labels$cx,
           y = lac.labels$cy,
           adj = 0, 
           size = 3)

#caribbean detail
lac.labels["Aruba", 2:3] <- c( -71, 13)          
lac.labels["Cuba", 2:3] <- c( -78, 21)
lac.labels["Curacao", 2:3] <- c( -69.5, 12.5)
lac.labels["Dominican Republic", 2:3] <- c( -71, 20.5)
lac.labels["Honduras", 2:3] <- c( -87, 15)
lac.labels["Jamaica", 2:3] <- c( -78, 17.5)
lac.labels["Nicaragua", 2:3] <- c( -86, 13)
lac.labels["Puerto Rico", 2:3] <- c( -67.5, 17.5)
lac.labels["Bahamas", 2:3] <- c( -74, 23.5)             
lac.labels["Venezuela", 2:3] <- c( -68, 10)

ggplot(lac.bc, aes(fill = n, map_id = birth_country)) + 
  napkin +
  geom_map(map = lac.mp, color = oj["blueK"]) + 
  expand_limits(x = lac.mp$long, y = lac.mp$lat) + 
  scale_fill_gradient2(low = jmbn["periwinkle"], mid = jmbn["rose"], high = oj["orange5"], na.value = "grey99", midpoint = 425) +
  labs(title = "Birth Countries of MLB Players from the Caribbean",
       subtitle = "Major and Minor Leagues (Affiliated), 2022",
       caption = "Data Source: MLB Stats API",
       x = "Longitude", y = "Latitude") + 
  coord_sf(xlim = c(-87,-62), ylim = c(10,25)) + 
  annotate("text", 
           label = row.names(lac.labels),
           x = lac.labels$cx,
           y = lac.labels$cy,
           adj = 0, 
           size = 3)

#asia map
unique(regs$GEO3)

asia_regs <- c("NW Pacific and East Asia", 
               "Southeast Asia", "South Asia",
               "Central Asia")

apacCountries <- regs$ADMIN[which(regs$GEO3 %in% asia_regs)]
apacCountries <- c(apacCountries, "Hong Kong", "Russia")


#filter map data rows for apac countries
apac.mp <- worldMap %>% filter(region %in% apacCountries)
#filter player data rows for apac countries
apac.bc <- bc %>% filter(birth_country %in% apacCountries)



ggplot(apac.bc, aes(fill = n, map_id = birth_country)) + 
  napkin +
  geom_map(map = apac.mp, color = oj["blueK"]) + 
  expand_limits(x = apac.mp$long, y = apac.mp$lat) + 
  scale_fill_gradient2(low = jmbn["periwinkle"], mid = jmbn["rose"], high = oj["orange5"], na.value = "grey99", midpoint = 8) +
  labs(title = "Birth Countries of MLB Players from Asia",
       subtitle = "Major and Minor Leagues (Affiliated), 2022",
       caption = "Data Source: MLB Stats API",
       x = "Longitude", y = "Latitude") + 
  coord_sf(xlim = c(70,160), ylim = c(5,60))

#-----

#labels for map
apac.labels <- data.frame(
  row.names = c(
    "China",
    "South Korea",
    "Japan",
    "Taiwan",
    "Hong Kong"
  ),
  cx = c(
    102,
    124,
    137,
    122,
    113
  ),
  cy = c(
    35,
    37,
    39,
    23,
    20
  )
  
)


ggplot(apac.bc, aes(fill = n, map_id = birth_country)) + 
  napkin +
  geom_map(map = apac.mp, color = oj["blueK"]) + 
  expand_limits(x = apac.mp$long, y = apac.mp$lat) + 
  scale_fill_gradient2(low = jmbn["periwinkle"], mid = jmbn["rose"], high = oj["orange5"], na.value = "grey99", midpoint = 8) +
  labs(title = "Birth Countries of MLB Players from Asia",
       subtitle = "Major and Minor Leagues (Affiliated), 2022",
       caption = "Data Source: MLB Stats API",
       x = "Longitude", y = "Latitude") + 
  coord_sf(xlim = c(70,160), ylim = c(5,60)) +
  annotate("text", 
           label = row.names(apac.labels),
           x = apac.labels$cx,
           y = apac.labels$cy,
           adj = 0, 
           size = 3.5) + 
  annotate("segment",
           x = 115, xend = 114,
           y = 20, yend = 22)




