
library(baseballr)
library(tidyverse)
library(maps)
library(mapdata)
library(rworldmap)
library(sf)
library(devtools)
library(lubridate)

source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/datavizExtras.R")

#i want a bigger, more comprehensive player dataset

#using the people table from the Lahman databank
#https://www.seanlahman.com/baseball-archive/statistics/
db <- read.csv("data/LahmanDatabank_people.csv")

#pretty sure this dataset is Major League players throughout history
#let's try to find out...
ggplot(db) + geom_histogram(aes(birthYear))
summary(db$birthYear)
#oldest guy born in 1820, youngest in 2001
db %>% filter(birthYear == 2001) %>% select(nameFirst, nameLast) #Wander Franco

#whose birth years are missing?
naBirthYr <- db %>% filter(is.na(birthYear)) %>%
  select(playerID, nameFirst, nameLast, 
         birthCountry, debut, deathYear)
#all these guys are from the 1800's

#number of players by country with earliest and latest debut dates
db <- db %>% 
  mutate(debutYear = as.numeric(str_sub(debut, start =1 ,end=4)))
  
byCountry <- db %>%
  group_by(birthCountry)%>% 
  summarize(n = n(),
            firstDebut = min(debutYear, na.rm = T),
            lastDebut = max(debutYear, na.rm = T)) %>%
  arrange(desc(n))
  
#this gives an idea of which countries had immigrants playing in us early on,
#vs other countries with baseball that started sending players later on
byCountry %>% arrange(firstDebut)
byCountry %>% arrange(desc(lastDebut))
#i want to make a distinction between players who are immigrants to america and 
#international players. need another variable aside from birth country,
#that describes where the player lived and trained through most of their development.

#i know i want to filter out countries that had last debut before 2000
bc <- byCountry %>% filter(lastDebut >= 2000)
#here are the countries that I'll call my intnl baseball countries
print(bc, n = 32)
ibc <- unique(bc$birthCountry)

#i want to filter for players from these countries, regardless of debut
#the idea is to see the history of countries still sending players
db2 <- db %>% filter(birthCountry %in% ibc)

db2 %>% group_by(birthCountry) %>% tally()
#i'm going to need to change some of these so they match map data
#also still need to rule out american nationals born overseas

bc <- bc %>% mutate(countryName = birthCountry)
bc$countryName[which(bc$birthCountry == "D.R.")] <- "Dominican Republic"
bc$countryName[which(bc$birthCountry == "P.R.")] <- "Puerto Rico"
bc$countryName[which(bc$birthCountry == "CAN")] <- "Canada"
bc$countryName[which(bc$birthCountry == "V.I.")] <- "Virgin Islands"

print(bc, n = 32)

db2.jp <- db %>% filter(birthCountry == "Japan")
db2.sk <- db %>% filter(birthCountry == "South Korea")
db2.tw <- db %>% filter(birthCountry == "Taiwan")


db %>% filter(birthCountry == "Lithuania")
#are guys from netherlands from europe or islands?
db %>% filter(birthCountry == "Netherlands")
#i think europe

#united kingdom is weird.  a ton of old timey players, only a couple recent ones
db %>% filter(birthCountry == "United Kingdom", debutYear > 2000)

db %>% filter(birthCountry == "Aruba") #Xander Boegarts
db %>% filter(birthCountry == "Curacao") 
#Andrelton Simmons, Jonathan Schoop, Jurickson Profar,
#Kenley Jansen, Ozzie Albies

db %>% filter(birthCountry == "V.I.")

#curiosities
#i think these are probably army kids
db %>% filter(birthCountry == "Saudi Arabia")
db %>% filter(birthCountry == "Guam")
db %>% filter(birthCountry == "Indonesia")
db %>% filter(birthCountry == "Hong Kong")


#----


#want to look at individual active players from certain places
#see baseballWorldMapsData.R for how i got dataset
d <- read.csv("data/affiliatedPlayers2022.csv")

#China
d %>% filter(birth_country2 %in% c("China", "Hong Kong"))
#2 american guys born in china (drafted), and a guy from Beijing:
#Jolon Zhao, pitcher in single A

#Australia & NZ
d %>% filter(birth_country2 == "Australia")
d %>% filter(birth_country2 == "New Zealand")

#south africa
d %>% filter(birth_country2 == "South Africa")

#uk
d %>% filter(birth_country2 == "UK")
#these guys were drafted, so i don't think they count
