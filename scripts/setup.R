#--------------------------------------------#
# setup
#--------------------------------------------#
library(dplyr)
library(ggplot2)
library(lubridate)
# if(require(tagtales))
#   devtools::install_github("Myfanwy/tagtales")
library(tagtales)
source("scripts/functions/munging_fxns.R")
source("scripts/functions/categorizing_exits_functions.R")
source("scripts/functions/convenience_fxns.R")

#--------------------------------------------#
#--------------------------------------------#
# exit station names
# exit stations include putah creek stations north of the check dam for salmon
exitstations = c("BCS", "BCN", "YBBTD", "PCSG",  "YBBCD")

# all yb array station names
stns = readRDS("data_clean/deployment_data/stns.rds")
stations <- stns$GroupedStn

# late-fall TagIDs to be excluded:
latefalls <- c(31570, 13720, 13723)

dets8 = readRDS("data_clean/detection_data/1_all_detections.rds") # output of scripts/analysis/1_munge-raw-data.r ; false detection shave been removed, but sheds/mortalities have not been identified or truncated.
alltags = readRDS("data_raw/tag_data_raw/alltags_raw.rds") # all tags
chntags = readRDS("data_raw/tag_data_raw/chn_tags_raw.rds") # chinook tags

#--------------------------------------------#
# wst detections
wst_dets = subset(dets8, TagID %in% alltags$TagID[alltags$Sp == "wst"])
wst_dets = get_det_year(wst_dets, "DateTimePST")
wst_dets = arrange(wst_dets, TagID, DateTimePST) 

#--------------------------------------------#
# chn detections
chn_dets <- dets8 %>% 
  filter(TagID %in% chntags$TagID, !(TagID %in% latefalls)) %>% 
  arrange(TagID, DateTimePST) 


