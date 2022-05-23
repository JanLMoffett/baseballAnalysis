
library(baseballr)
library(tidyverse)

#MLB's players in 2021, from MLB using baseballr:
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
addLeague <- function(dx, lgName){
  return(dx %>% mutate(league = lgName))
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

#now I can combine them into one df
d <- d.mlb %>% union(d.3a) %>%
    union(d.2a) %>%
    union(d.ha) %>%
    union(d.1a) %>%
    union(d.rk)

#remove the players that aren't active
d <- d %>% filter(active == T)

#that gives 5259 players rn (05/23/22)
N <- dim(d)[1]

#let's see where everyone's from
d %>%
  group_by(birth_country) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

ggplot(d) + 
  geom_bar(aes(birth_country))
