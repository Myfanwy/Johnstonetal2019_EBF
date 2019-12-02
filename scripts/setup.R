#--------------------------------------------#
# setup
#--------------------------------------------#
library(dplyr)
library(ggplot2)
library(lubridate)
# if(require(tagtales))
#   devtools::install_github("Myfanwy/tagtales")
library(tagtales)
source("scripts/functions.R")
#--------------------------------------------#
#--------------------------------------------#
# exit station names
# exit stations include putah creek stations north of the check dam for salmon
exitstations = c("BC_joint", "BC_joint2", "Base_TD", "PutahCrk_mace",  "PutahCrk_AbvChk", "PutahCrk_spawn")

# all yb array station names
stations <- c("KLRC_North", "Wallace_weir", "Knaggs", "Cache_creek", "Abv_swanston", 
"Swanston", "Greenlake", "I80_1", "I80_2", "I80_3", "I80_4", 
"I80_5", "I80_6", "I80_7", "Levee_marker", "Willow_slough", "PutahCrk_spawn", 
"PutahCrk_mace", "PutahCrk_AbvChk", "Putah_creek", "Abv_lisbon", 
"Lisbon", "Abv_rstr", "Rstr_joint", "RSTR", "Base_TD", "BCE", 
"BCW", "BC_joint", "BCE2", "BCW2", "BC2_joint")

# late-fall TagIDs to be excluded:
latefalls <- c(31570, 13720, 13723)

# detection windows for each year:
d <- readRDS("data/all69khz_grouped.rds") %>% 
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


