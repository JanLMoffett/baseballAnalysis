# baseballAnalysis
R scripts to answer questions about baseball.

Packages required:
- [baseballr](https://billpetti.github.io/baseballr/)
- dplyr
- ggplot2
- devtools
- stringr
- lubridate

### [buildDatesMLB.R](buildDatesMLB.R)

Build a vector of all dates that will return regular season game data from Baseball Savant's Statcast Search.

### [samplingMLB.R](samplingMLB.R)

Draw data from randomly chosen game dates to estimate MLB average launch angle and launch speed for balls in play.  

### [samplingMLBbatters.R](samplingMLBbatters.R)

Draw data from randomly chosen game dates to estimate probability distributions of launch angle and launch speed for MLB balls in play.

### [checkOnYeli.R](checkOnYeli.R)

Is Christian Yelich hitting or what?

### [plotLaunch.R](plotLaunch.R)

Plot the launch angle and speed of batted balls.
