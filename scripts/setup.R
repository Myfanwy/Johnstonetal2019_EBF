#--------------------------------------------#
# setup
#--------------------------------------------#

library(fishtrackr)
library(ybp)
library(dplyr)
library(ggplot2)
library(lubridate)
library(cfs.misc)
# detection windows for each year


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

# wst detections
wst <- d %>% 
  filter(Sp == "wst") %>% 
  arrange(TagID, DateTimeUTC) 

#wst <- fishtrackr::fishpaths(wst, wst$TagID, wst$Station)

exitstations = c("BC_joint", "BC_joint2", "Base_TD")

# chn detections
chn <- d %>% 
  filter(Sp == "chn") %>% 
  arrange(TagID, DateTimeUTC) 

# functions
logistic <- function (x) 
{
    p <- 1/(1 + exp(-x))
    p <- ifelse(x == Inf, 1, p)
    p
}
