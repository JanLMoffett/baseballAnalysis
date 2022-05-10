
#using ggplot2 to visualize launch angle and launch speed variables from Baseball Savant data


bipSum <- doSummary(bipDF, "BIP")
hitSum <- doSummary(hitDF, "hit")
singSum <- doSummary(singDF, "single")
doubSum <- doSummary(doubDF, "double")
tripSum <- doSummary(tripDF, "triple")
hrSum <- doSummary(hrDF, "home_run")

bigSum <- bipSum %>% union(hitSum) %>%
  union(singSum) %>% union(doubSum) %>%
  union(tripSum) %>% union(hrSum)

bigSum

#plotting launch angle and speed

ggplot(bigSum[3:6,]) + 
  napkin + 
  coord_cartesian(xlim = c(-120,120), ylim = c(-120,120)) + 
  geom_vline(xintercept = 0, color = ibm["blue"]) + 
  geom_hline(yintercept = 0, color = ibm["blue"]) + 
  geom_spoke(aes(x = 0, y = 0, angle = (pi/180)*meanLaunchAngle, radius = meanLaunchSpeed))

