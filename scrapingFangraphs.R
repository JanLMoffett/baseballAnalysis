
setwd("~/R/R Projects/")

library(dplyr)
library(baseballr)
library(stringr)

#Scrape a year's fangraphs pitching leaderboard
q <- fg_pitcher_leaders(x = 2021,
                        y = 2021,
                        league = "all",
                        qual = 100,
                        pitcher_type = "sta",
                        ind = 1)


names(q)[str_which(names(q), "Strike")]

q %>% select(Strikes, Pitches, SwStr_pct) %>%
  mutate(SwingsMisses = SwStr_pct*Strikes/100)
  


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

# Zack Greinke Fielding Project

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

#using baseballr to scrape fangraphs pitching leaders, year by year

#Data Restrictions:
#starting pitchers
#at least 100 IP
#regular season (?)
#after scraping, may also narrow down to at least 2000 pitches/season
#leaving out 2020, thoroughly atypical season and not enough data

#since the pitching leaders dataset has so many vars,
#going to pare it down
#-----
myVars <- c("playerid",
            "Season",
            "Name",
            "Team",
            "Age",
            "G",
            "GS",
            "IP",
            "TBF",
            "H",
            "R",
            "HR",
            "BB",
            "HBP",
            "WP",
            "SO",
            "GB",
            "FB",
            "LD",
            "IFFB",
            "Balls",
            "Strikes",
            "Pitches",
            "BABIP",
            "IFFB_pct",
            "HR_FB",
            "Swing_pct",
            "Contact_pct",
            "BIP-Wins",
            "LD_pct",
            "GB_pct",
            "FB_pct",
            "Pull_pct",
            "Cent_pct",
            "Oppo_pct",
            "Soft_pct",
            "Med_pct",
            "Hard_pct")

#----

#making a function to repeat code for multiple years
jm_scrape_fg <- function(season, require = c("baseballr","dplyr")){
  
  q <- fg_pitcher_leaders(x = season,
                          y = season,
                          league = "all",
                          qual = 100,
                          pitcher_type = "sta",
                          ind = 1)
  q <- q %>% select(all_of(myVars))
    
  return(q)
}

jm_scrape_fg2 <- function(season, require = c("baseballr","dplyr")){
  
  q <- fg_pitcher_leaders(x = season,
                          y = season,
                          league = "all",
                          qual = 30,
                          pitcher_type = "sta",
                          ind = 1)
  q <- q %>% select(all_of(myVars))
  
  return(q)
}


#scraping year by year for 2015-2019, 2021
#----
pl21 <- jm_scrape_fg(2021)
pl20 <- jm_scrape_fg2(2020)
pl19 <- jm_scrape_fg(2019)
pl18 <- jm_scrape_fg(2018)
pl17 <- jm_scrape_fg(2017)
pl16 <- jm_scrape_fg(2016)
pl15 <- jm_scrape_fg(2015)

#going to keep years separate, to keep datasets modular
#for use in future projects

#writing csv's to my data folder
#----
write.csv(pl21, "data/fg_pitching_leaders_2021.csv")
write.csv(pl20, "data/fg_pitching_leaders_2020.csv")
write.csv(pl19, "data/fg_pitching_leaders_2019.csv")
write.csv(pl18, "data/fg_pitching_leaders_2018.csv")
write.csv(pl17, "data/fg_pitching_leaders_2017.csv")
write.csv(pl16, "data/fg_pitching_leaders_2016.csv")
write.csv(pl15, "data/fg_pitching_leaders_2015.csv")
#----
#End Zack Greinke Fielding Project
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

