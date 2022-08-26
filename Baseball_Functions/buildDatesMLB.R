
#code to build a big vector of game dates for MLB seasons beginning in 2015

#a map for how many days in each month
#leap years don't matter bc season always begins after feb
mguide <- c("03" = 31, "04" = 30, "05" = 31, "06" = 30, "07" = 31, "08" = 31, "09" = 30, "10" = 31)

#function to make a month-long vector of consecutive dates in "yyyy-mm-dd" format
make_month <- function(m_2dig, y_4dig, numDays){
  v <- rep("", numDays)
  for(i in 1:numDays){
    v[i] <- paste0(y_4dig, "-", m_2dig, "-", str_pad(as.character(i), 2, side = "left", pad = "0"))
  }
  return(v)
}
#function to make a year-long vector of consecutive dates in "yyyy-mm-dd" format
make_year <- function(monthsDF){
  w <- vector()
  for(r in 1:dim(monthsDF)[1]){
    w <- c(w, make_month(monthsDF$mnth[r], monthsDF$yr[r], monthsDF$dys[r]))
  }
  return(w)
}
#function to make input for make_year function
#startDate and endDate must be "yyyy-mm-dd"
make_monthDF <- function(startDate, endDate){
  
  syear <- str_sub(startDate, 1, 4)
  
  smonth <- str_sub(startDate, 6, 7)
  emonth <- str_sub(endDate, 6, 7)
  
  sday <- str_sub(startDate, 9, 10)
  eday <- str_sub(endDate, 9, 10)
  
  #num of rows in df is det by num of unique months in season
  mi <- as.numeric(smonth):as.numeric(emonth)
  ri <- 1:length(mi)
  
  mnth = str_pad(mi, 2, side = "left", pad = "0")
  yr = rep(syear, length(mi))
  dys = mguide[mnth]
  dys[length(dys)] = eday
  
  return(
    data.frame(
      mnth,
      yr,
      dys
    )
  )
}

#https://www.baseball-reference.com/leagues/majors/2021-schedule.shtml
#The 2022 season is taking place Apr 7 - Oct 5, ASB Jul 18-20
#The 2021 season took place Apr 1 - Oct 3, ASB Jul 12-15
#The 2020 regular season took place Jul 23 - Sep 27
#The 2019 regular season took place Mar 20 - Sep 29, ASB Jul 8-10
#The 2018 regular season took place Mar 29 - Oct 1, ASB Jul 16-18
#The 2017 regular season took place Apr 2 - Oct 1, ASB Jul 10-13
#The 2016 regular season took place Apr 3 - Oct 2, ASB Jul 11-14
#The 2015 regular season took place Apr 5 - Oct 4, ASB Jul 13-16

#a data frame containing reg season start and end dates of statcast years (2015-now)
SCseasons <- data.frame(
  season = 2015:2022,
  start = c("2015-04-05", "2016-04-03", "2017-04-02", "2018-03-29", "2019-03-20", 
            "2020-07-23", "2021-04-01", "2022-04-07"),
  end = c("2015-10-04", "2016-10-02", "2017-10-01", "2018-10-01", "2019-09-29",
          "2020-09-27", "2021-10-03", "2022-10-05"))

#a vector to hold all dates
bigDates <- vector()

#running code to build vector of dates
for(i in 1:dim(SCseasons)[1]){
  #produce a monthDF for each season
  monthsYR <- make_monthDF(SCseasons$start[i], SCseasons$end[i])
  #produce a vector of dates for each season
  datesYR <- make_year(monthsYR)
  #trim the head of the vector to season start date
  s <- as.numeric(str_sub(SCseasons$start[i], 9, 10))
  datesYR.t <- datesYR[s:length(datesYR)]
  #append to vector of all dates
  bigDates <- c(bigDates, datesYR.t)
  
}

#remove all star breaks from dates
bigDates <- bigDates[-c(which(bigDates == "2022-07-18"):which(bigDates == "2022-07-20"))]
bigDates <- bigDates[-c(which(bigDates == "2021-07-12"):which(bigDates == "2021-07-15"))]
bigDates <- bigDates[-c(which(bigDates == "2019-07-08"):which(bigDates == "2019-07-10"))]
bigDates <- bigDates[-c(which(bigDates == "2018-07-16"):which(bigDates == "2018-07-18"))]
bigDates <- bigDates[-c(which(bigDates == "2017-07-10"):which(bigDates == "2017-07-13"))]
bigDates <- bigDates[-c(which(bigDates == "2016-07-11"):which(bigDates == "2016-07-14"))]
bigDates <- bigDates[-c(which(bigDates == "2015-07-13"):which(bigDates == "2015-07-16"))]

#write.csv(data.frame(dates = bigDates), "data/all_mlb_dates_2015_2022.csv")
