

library(baseballr)
library(tidyverse)
library(lubridate)
library(devtools)
library(boot)

#all dates so far in 2022 season
ms <- c(rep("04", 24), rep("05", 31), rep("06", 7))
ds <- c(7:30, 1:31, 1:7)
ys <- rep("2022", 62)
dts22 <- paste0(ys, "-", ms, "-", str_pad(ds, side = "left", width = 2, pad = "0"))

#looking up Christian's MLB ID (592885)
#cyID <- playerid_lookup(last_name = "Yelich", first_name = "Christian")$mlbam_id
cyID <- 592885

cy.plays <- statcast_search(start_date = "2022-04-07", end_date = "2022-04-07")
cy.plays <- cy.plays[1,]

for(i in seq_along(dts22)){
  
  print(paste0("Scraping ", i, " of ", length(dts22)))
  d.i <- statcast_search(start_date = dts22[i], end_date = dts22[i])
  d.i <- d.i %>% filter(events != "", fielder_7 == cyID, str_detect(des, "Yelich"))
  
  cy.plays <- union(cy.plays, d.i)
  
}

cy.plays <- cy.plays[-1,]
cy.plays <- cy.plays %>% filter(events %in% c("field_out", "sac_fly"))

#write.csv(plays, "data/yelichLFplays.csv")


#looking up Greinke's MLB ID 425844
#zgID <- playerid_lookup(last_name = "Greinke", first_name = "Zack")$mlbam_id
zgID <- 425844

zg22 <- statcast_search(start_date = "2022-04-07", 
                         end_date = as.character(today()-1), 
                         player_type = "pitcher",
                         playerid = zgID)
zg.plays <- zg22 %>% filter(str_detect(des, "Zack Greinke"))
zg.plays <- zg.plays %>% filter(events != "")

zg.plays <- zg.plays[-3,]

#write.csv(zg.plays, "data/greinkePlays.csv")

#how do i get a big list of pitchers mlbam id's to compare zack to?
fgPitchLdrs22 <- fg_pitcher_leaders(x = 2022, y = 2022, league = "all", pitcher_type = "sta")

#need to match fangraphs player id's to mlbam player id's

chad <- chadwick_player_lu()

#fangraphs ids of starting pitchers on leaderboard
fgids <- as.numeric(fgPitchLdrs22$playerid)
fgnames <- fgPitchLdrs22$Name
#do all of the fg id's show up on chad?
setdiff(fgids, chad$key_fangraphs)
#22182 is missing - Hunter Greene
fgPitchLdrs22$Name[fgPitchLdrs22$playerid == "22182"]

fg_to_mlbid <- fgPitchLdrs22 %>% select(playerid, Name) %>%
  mutate(fgid = as.numeric(playerid))
chad2 <- chad %>% select(key_fangraphs, key_mlbam, name_first, name_last)

#fill in Hunter Greene's info
fg_to_mlbid <- left_join(fg_to_mlbid, chad2, by = c("fgid" = "key_fangraphs"))
fg_to_mlbid$key_mlbam[fg_to_mlbid$Name == "Hunter Greene"] <- 668881
fg_to_mlbid$name_first[fg_to_mlbid$Name == "Hunter Greene"] <- "Hunter"
fg_to_mlbid$name_last[fg_to_mlbid$Name == "Hunter Greene"] <- "Greene"

fgp22 <- fgPitchLdrs22 %>% mutate(playerid = as.numeric(playerid)) %>%
  left_join(fg_to_mlbid[,3:4], by = c("playerid" = "fgid"))

#is anyone's mlbam id missing in fgp22?
sum(is.na(fgp22$key_mlbam))

#reduce vars in dataset
fgp22 <- fgp22 %>% select(-c(ends_with("_pi"), ends_with("(sc)")))

spIDs <- unique(pit22$key_mlbam)

sp.plays <- statcast_search(start_date = "2022-04-07", end_date = "2022-04-07", player_type = "pitcher")
sp.plays <- sp.plays[1,]

i = 1
for(spid in spIDs){
  
  print(paste0("Scraping ", i, " of ", length(spIDs)))
  
  plays.spid <- statcast_search(start_date = "2022-04-07", end_date = "2022-06-07", playerid = spid, player_type = "pitcher")
  plays.spid <- plays.spid %>% filter(str_detect(des, "pitcher"))
  
  sp.plays <- union(sp.plays, plays.spid)
  i = i + 1
}

sp.plays <- sp.plays[-1,]

write.csv(fgp22, "data/FGleadersSP2022.csv")
write.csv(sp.plays, "data/startingPitcherPlays2022.csv")






