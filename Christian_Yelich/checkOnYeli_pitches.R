

library(baseballr)
library(tidyverse)
library(lubridate)
library(devtools)
library(boot)

#importing colors and themes for plots
#github.com/JanLMoffett/datavizExtras
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/datavizExtras.R")
#mlb season dates
source_url("https://raw.githubusercontent.com/JanLMoffett/baseballAnalysis/master/buildDatesMLB.R")

#get Christian Yelich's pitches
cy <- read.csv("data/yelichPitches.csv")
names(cy)

#get MLB averages
pp22 <- read.csv("data/pitchProps22.csv")
pp21 <- read.csv("data/pitchProps21.csv")
pp20 <- read.csv("data/pitchProps20.csv")
pp19 <- read.csv("data/pitchProps19.csv")
pp18 <- read.csv("data/pitchProps18.csv")
pp17 <- read.csv("data/pitchProps17.csv")
pp16 <- read.csv("data/pitchProps16.csv")
pp15 <- read.csv("data/pitchProps15.csv")

pp <- union(pp22, pp21) %>% union(pp20) %>% union(pp19) %>% 
  union(pp18) %>% union(pp17) %>% union(pp16) %>% union(pp15)

pp <- pp %>% mutate(season = as.numeric(str_sub(date, start = 1, end = 4)))


pp.smzd <- pp %>% group_by(season) %>%
  summarize(
    dates = n(),
    across(PA:takesIZ, sum)
  ) %>%
  mutate(
    prop_takes = takes/pitches,
    prop_swings = swings/pitches,
    prop_insideZone = insideZone/pitches,
    prop_outsideZone = outsideZone/pitches,
    prop_swingsIZ = swingsIZ/(swingsIZ + swingsOZ),
    prop_swingsOZ = swingsOZ/(swingsIZ + swingsOZ),
    prop_takesIZ = takesIZ/(takesIZ + takesOZ),
    prop_takesOZ = takesOZ/(takesIZ + takesOZ)
  )

pp.smzd$one <- 1


#making a more plot-friendly variable for inside/outside zone
cy <- cy %>% 
  mutate(
    in_out_zone = case_when(isOutsideZone == 1 ~ "Outside Strike Zone",
                            isOutsideZone == 0 ~ "Inside Strike Zone",
                            TRUE ~ "NA")) %>%
  mutate(in_out_zone = factor(in_out_zone))

unique(cy$description)
take <- c("ball","called_strike","blocked_ball")
whf <- c("swinging_strike","swinging_strike_blocked")
foul  <- c("foul","foul_tip")
bip <- c("hit_into_play")
int <- c("intent_ball","pitchout","missed_bunt",
         "foul_bunt")
hbp <- ("hit_by_pitch")

cy <- cy %>% mutate(
  description_type = case_when(
    description %in% take ~ "Take",
    description %in% whf ~ "Whiff",
    description %in% foul ~ "Foul",
    description == "hit_into_play" ~ "BIP",
    description %in% int ~ "Bunt or Intentional Ball",
    description == "hit_by_pitch" ~ "HBP",
    TRUE ~ "NA"
  ) 
)



threeColors <- c(ibm["blue"], ibm["yellow"], "grey50")
names(threeColors) <- c("Outside Strike Zone", "Inside Strike Zone", "NA")

#proportion of pitches inside and outside the strike zone
#CY all pitches
ggplot(cy) + 
  napkin + 
  labs(title = "All Pitches: Proportion Outside vs. Inside Strike Zone",
       subtitle = "Pitches to Christian Yelich, 2015-2021, 2022 through June 1",
       x = "Proportion of Total Pitches", 
       y = "Season",
       caption = "Data Source: Baseball Savant",
       fill = "Location",
       tag = "Fig 1.2") + 
  geom_bar(aes(y = factor(season), fill = in_out_zone), position = "fill") +
  scale_fill_manual(values = threeColors) + 
  geom_vline(xintercept = mean(cy$isOutsideZone, na.rm = T), linetype = 2, size = 0.9, color = "black") + 
  geom_col(data = pp.smzd, aes(y = factor(season), x = prop_outsideZone), 
           fill = transpa(ibm["pink"], 50), color = ibm["pink"], linetype = 3, size = 1)
#CY takes
ggplot(cy %>% filter(description_type == "Take")) + 
  napkin +
  labs(title = "Takes: Proportion Outside vs. Inside Strike Zone",
       subtitle = "Pitches to Christian Yelich, 2015-2021, 2022 through June 1",
       x = "Proportion of Total Takes",
       y = "Season",
       caption = "Data Source: Baseball Savant",
       fill = "Location",
       tag = "Fig 2.2") +
  geom_bar(aes(y = factor(season), fill = in_out_zone), position = "fill") + 
  scale_fill_manual(values = threeColors) + 
  geom_vline(xintercept = mean(cy %>% filter(description_type == "Take") %>% 
                                 pull(isOutsideZone), na.rm = T), linetype = 2, size = 0.9) + 
  geom_col(data = pp.smzd, aes(y = factor(season), x = prop_takesOZ), 
           fill = transpa(ibm["pink"], 50), color = ibm["pink"], linetype = 3, size = 1)
#CY swings
ggplot(cy %>% filter(isSwing == 1)) + 
  napkin +
  labs(title = "Swings: Proportion Outside vs. Inside Strike Zone",
       subtitle = "Pitches to Christian Yelich, 2015-2021, 2022 through June 1",
       x = "Proportion of Total Swings",
       y = "Season",
       caption = "Data Source: Baseball Savant",
       fill = "Location",
       tag = "Fig 3.2") +
  geom_bar(aes(y = factor(season), fill = in_out_zone), position = "fill") + 
  scale_fill_manual(values = threeColors) + 
  geom_vline(xintercept = mean(cy %>% filter(isSwing == 1) %>% 
                                 pull(isOutsideZone), na.rm = T), linetype = 2, size = 0.9) + 
  geom_col(data = pp.smzd, aes(y = factor(season), x = prop_swingsOZ), 
           fill = transpa(ibm["pink"], 50), color = ibm["pink"], linetype = 3, size = 1)



threeColors <- c(ibm["yellow"], ibm["orange"], ibm["pink"])
names(threeColors) <- c("Whiff", "Foul", "Take")

ggplot(cy %>% filter(isSwing == 1)) + 
  napkin + 
  labs(title = "Whiff Rate on Swings",
       x = "Proportion of Total",
       y = "Season",
       caption = "Data Source: Baseball Savant",
       fill = "Whiff vs. Contact") + 
  geom_bar(aes(y = factor(season), fill = description_type), position = "fill") + 
  scale_fill_manual(values = threeColors)

cy %>% filter(type == "S") %>% group_by(description_type) %>% tally()


unique(cy$type)

fourColors <- c(ibm["yellow"], ibm["orange"], ibm["pink"], ibm["blue"])
names(fourColors) <- c("Whiff", "Take", "Foul", "Bunt or Intentional Ball")
#kinds of strikes
ggplot(cy %>% filter(type == "S")) + 
  napkin + 
  labs(title = "Breakdown of Strikes",
       x = "Proportion of Total Strikes",
       y = "Season",
       caption = "Data Source: Baseball Savant",
       fill = "Type of Srike") + 
  geom_bar(aes(y = factor(season), fill = description_type), position = "fill") + 
  scale_fill_manual(values = fourColors)
 
 