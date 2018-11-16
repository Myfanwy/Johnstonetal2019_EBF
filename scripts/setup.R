#--------------------------------------------#
# setup
#--------------------------------------------#
library(fishtrackr)
library(ybp)
library(dplyr)
library(ggplot2)
library(lubridate)
library(cfs.misc)
#--------------------------------------------#
#--------------------------------------------#
# exit station names
# exit stations include putah creek stations north of the check dam for salmon
exitstations = c("BC_joint", "BC_joint2", "Base_TD", "PutahCrk_mace",  "PutahCrk_AbvChk", "PutahCrk_spawn")

# late-fall TagIDs to be excluded:
latefalls <- c(31570, 13720, 13723)

# detection windows for each year:
d <- all69khz_grouped %>% 
  mutate(Detyear = case_when(
    DateTimeUTC %within% (ymd_hms("2011-07-01 00:00:00") %--% ymd_hms("2012-06-30 23:59:59")) ~ 2011 ,
    DateTimeUTC %within% (ymd_hms("2012-07-01 00:00:00") %--% ymd_hms("2013-06-30 23:59:59")) ~ 2012 ,
    DateTimeUTC %within% (ymd_hms("2013-07-01 00:00:00") %--% ymd_hms("2014-06-30 23:59:59")) ~ 2013 ,
    DateTimeUTC %within% (ymd_hms("2014-07-01 00:00:00") %--% ymd_hms("2015-06-30 23:59:59")) ~ 2014 ,
    DateTimeUTC %within% (ymd_hms("2015-07-01 00:00:00") %--% ymd_hms("2016-06-30 23:59:59")) ~ 2015 ,
    DateTimeUTC %within% (ymd_hms("2016-07-01 00:00:00") %--% ymd_hms("2017-06-30 23:59:59")) ~ 2016 ,
    TRUE ~ 2017)) %>% 
  select(-Frequency)

#--------------------------------------------#
# wst detections
wst <- d %>% 
  filter(Sp == "wst") %>% 
  arrange(TagID, DateTimeUTC) 

#--------------------------------------------#
# chn detections
chn <- d %>% 
  filter(Sp == "chn", !(TagID %in% latefalls)) %>% 
  arrange(TagID, DateTimeUTC) 


#############
# functions #--------------------------------#
#############

#--------------------------------------------#
# for converting parm estimates from log-odds to probability:
logistic <- function (x) 
{
    p <- 1/(1 + exp(-x))
    p <- ifelse(x == Inf, 1, p)
    p
}

#--------------------------------------------#
# for finding final detection locations:
firstlastOneFish <- function(x) {
  x = x[order(x$DateTimePST), ]
  
  FirstRow = x[which.min(x$DateTimePST), ]  # subset to the first departure
  LastRow = x[which.max(x$DateTimePST), ] # subset to the last arrival

  data.frame(
    TagID = x$TagID[1],
    FirstStation = FirstRow$Station,
    LastStation = LastRow$Station,
    reachdistance = FirstRow$Rkm - LastRow$Rkm,
       stringsAsFactors = FALSE
  )
}

FirstLast <- function(df) {
  do.call(rbind, lapply(split(df, df$TagID), firstlastOneFish))
}

