

library(baseballr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(boot)
library(devtools)

source_url("https://raw.githubusercontent.com/JanLMoffett/baseballAnalysis/master/buildDatesMLB.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/datavizExtras.R")

#pick some random days, stratified by season

#number of dates sampled from each season
nDates <- 5
#sample of dates
set.seed(624); sDates15 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2015"], size = nDates)
set.seed(245); sDates16 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2016"], size = nDates)
set.seed(534); sDates17 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2017"], size = nDates)
set.seed(457); sDates18 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2018"], size = nDates)
set.seed(512); sDates19 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2019"], size = nDates)
set.seed(697); sDates20 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2020"], size = nDates)
set.seed(753); sDates21 <- sample(bigDates[str_sub(bigDates, 1, 4) == "2021"], size = nDates)

sDates <- c(sDates15, sDates16, sDates17, sDates18, sDates19, sDates20, sDates21)

#for each date, filter for balls in play
#bin by launch speed and angle and calculate probability of hit

#sample dataframe
d.yesterday <- statcast_search(start_date = "2022-05-09", end_date = "2022-05-09", player_type = "batter")
d.s1 <- d.yesterday[1,]

for(i in seq_along(sDates)){
  print(paste0("Scraping sample ", i, " of ", length(sDates)))
  #pull statcast data for that date
  d1 <- statcast_search(start_date = sDates[i], end_date = sDates[i], player_type = "batter")
  d1 <- d1 %>% filter(type == "X")
  
  #add to sample dataframe
  d.s1 <- d.s1 %>% union(d1)
  
}
#take out dummy row
d.s1 <- d.s1[-1,]

#now i have a sample of balls in play
summary(d.s1$launch_angle)
summary(d.s1$launch_speed)

#saving the dataset
#write.csv(d.s1, "data/BIP_35dates_2015_21.csv")

subt1 <- "All BIP from a stratified random sample\nof game dates, 2015-2021"
#the sample of game date is stratified by year
cap1 <- "Data Source: Baseball Savant"

#histogram of launch speed
ggplot(d.s1) + napkin + 
  geom_histogram(aes(launch_speed), fill = oj["blue3"]) + 
  geom_vline(xintercept = mean(d.s1$launch_speed, na.rm = T), color = oj["orangeK"]) + 
  geom_vline(xintercept = quantile(d.s1$launch_speed, probs = c(0.05,0.95), na.rm = T), color = oj["orange5"]) + 
  labs(title = "Sample Distribution of Launch Speed",
       subtitle = subt1,
       caption = cap1) + 
  annotate("text", x = mean(d.s1$launch_speed, na.rm = T), y = 0, label = round(mean(d.s1$launch_speed, na.rm = T), digits=2)) +
  annotate("text", x = quantile(d.s1$launch_speed, probs = c(0.05, 0.95), na.rm = T), y = 0, label = round(quantile(d.s1$launch_speed, probs = c(0.05, 0.95), na.rm = T), digits=2))

#histogram of launch angle
ggplot(d.s1) + napkin + 
  geom_histogram(aes(launch_angle), fill = oj["blue4"]) + 
  geom_vline(xintercept = mean(d.s1$launch_angle, na.rm = T), color = oj["orangeK"]) + 
  geom_vline(xintercept = quantile(d.s1$launch_angle, probs = c(0.05,0.95), na.rm = T), color = oj["orange5"]) +
  geom_vline(xintercept = quantile(d.s1$launch_angle, probs = c(0.05,0.95), na.rm = T), color = oj["orange5"]) + 
  labs(title = "Sample Distribution of Launch Angle",
       subtitle = subt1,
       caption = cap1) + 
  annotate("text", x = mean(d.s1$launch_angle, na.rm = T), y = 0, label = round(mean(d.s1$launch_angle, na.rm = T), digits=2)) +
  annotate("text", x = quantile(d.s1$launch_angle, probs = c(0.05, 0.95), na.rm = T), y = 0, label = round(quantile(d.s1$launch_angle, probs = c(0.05, 0.95), na.rm = T), digits=2))

#ECDF for launch speed
e <- ggplot(d.s1) + napkin + stat_ecdf(aes(launch_speed)) + 
  labs(title = "ECDF of Launch Speed")

#ECDF for launch angle
ggplot(d.s1) + napkin + stat_ecdf(aes(launch_angle)) + 
  labs(title = "ECDF of Launch Angle")


#i'd like to bootstrap samples from the sample of BIP

#bootstrapping est. of mean launch speed on all BIP, statcast era
#----
#function for boot param
sampleMean.LS <- function(t, i) {
  return(mean(t$launch_speed[i], na.rm = T))
}

#run boot
boot1 <- boot(d.s1, sampleMean.LS, 1000)

#histogram of bootstrap means on launch speed
td1 <- data.frame(x = seq(1,1000,1), sampleMeans = boot1$t)
ggplot(td1) + napkin + 
  geom_histogram(aes(x = sampleMeans), fill = ibm["purple"]) + 
  geom_vline(xintercept = boot1$t0, color = ibm["orange"]) + 
  labs(title = "Distribution of Bootstrap Sample Means",
       subtitle = "Launch Speed on BIP, 2015-2021",
       cap = cap1,
       x = "Sample Means of Launch Speed (MPH)",
       y = "Count") + 
  annotate("text", x = 87.9, y = 105, label = paste0("est. mean = ", round(boot1$t0, digits = 2)))
#----

#bootstrapping est. of mean launch angle
#----
#function for boot param
sampleMean.LA <- function(t, i) {
  return(mean(t$launch_angle[i], na.rm = T))
}

#run boot
boot2 <- boot(d.s1, sampleMean.LA, 1000)

#histogram of bootstrap means on launch speed
td2 <- data.frame(x = seq(1,1000,1), sampleMeans = boot2$t)
ggplot(td2) + napkin + 
  geom_histogram(aes(x = sampleMeans), fill = ibm["purple"]) + 
  geom_vline(xintercept = boot2$t0, color = ibm["orange"]) + 
  labs(title = "Distribution of Bootstrap Sample Means",
       subtitle = "Launch Angle on BIP, 2015-2021",
       cap = cap1) + 
  annotate("text", x = 12.1, y = 85, label = paste0("est. mean = ", round(boot2$t0, digits = 2)))
#----
