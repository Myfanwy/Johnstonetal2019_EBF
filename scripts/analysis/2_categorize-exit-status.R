#-------------------------------------------------------#
# Step 2 of analysis
# M. Johnston
# Fri Dec 27 13:08:02 2019 ------------------------------

# goal: categorize all fish, each year, into one of three "exit" statuses:

# 1) Tag shed in Bypass
#    - occurs when a tagged fish is recovered by hand somewhere after being released, and their detection history shows their final recorded detection somewhere in the Yolo Bypass array.  This applied to three fish in the study, all recovered at Wallace Weir fyke trap and identified either by their floy tags or their fork lengths + detection histories: 

known_sheds = c(37835 , # this fish was recovered in fyke and identified by its floy tag in 2015
                20161 , # captured in WW fyke and identified by fork length/detection history consistent with capture time
                20164 , # captured in WW fyke and identified by fork length/detection history consistent with capture time
                )

# 2) Exited
# - occurs when a fish's last detection location is at BCN, BCS, or the base of the Toe drain, AND that fish is not detected again within the Yolo Bypass array in the same detection year (i.e., white sturgeon have a clear final exit in a given detection year).  Many of these fish have subsequent detections outside the Yolo Bypass array, via the BARD database.

# 3) Shed/Mortality/Did not Exit the Yolo Bypass
# - all other fish in the study fall into this category.

dets8 = readRDS("data_clean/detection_data/1_all_detections.rds") # output of scripts/analysis/1_munge-raw-data.r ; false detection shave been removed, but sheds/mortalities have not been identified or truncated.
alltags = readRDS("data_raw/tag_data_raw/alltags_raw.rds") 
chntags = readRDS("data_raw/tag_data_raw/chn_tags_raw.rds")

source("scripts/functions/munging_fxns.R")
library(tagtales)

# split into chn and wst, because wst have multiple returns
chn_dets = subset(dets8, TagID %in% chntags$TagID)
# chn_dets = tag_tales(chn_dets, chn_dets$TagID, chn_dets$GroupedStn, "DateTimePST")
# chn first and last detection locations
chn_fl = first_last(chn_dets, "TagID", "DateTimePST", "GroupedStn")


wst_dets = subset(dets8, TagID %in% alltags$TagID[alltags$Sp == "wst"])

wst_dets = get_det_year(wst_dets, "DateTimePST")

# with white sturgeon, have to go by year.
