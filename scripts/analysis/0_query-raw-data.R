#--------------------------------------------#
# Query raw data from Yolo Bypass .sqlite database
# Myfanwy Johnston
# Wed Dec 18 11:26:08 2019 ------------------------------

# This shows how the original .sqlite database (not included in this repo) was queried.  The results of this query are saved to data_raw/ and munged in scripts/analysis/1_munge-detection-data.R.

library(dplyr)

# database connection: requires >= RSQLite 2.1.1.  
db = RSQLite::dbConnect(RSQLite::SQLite(), "yb_database.sqlite")

#--------------------------------------------#

# Chinook tagging metadata (includes RAMP data)
tbl(db, "chn") %>% collect() %>% saveRDS("data_raw/tag_data_raw/chn_tags_raw.rds")

# All tagging metadata (including both wst and chinook)
tbl(db, "tags") %>% collect() %>% saveRDS("data_raw/tag_data_raw/alltags_raw.rds")

# Receiver deployments table
tbl(db, "deployments") %>% collect() %>% saveRDS("data_raw/deployment_data_raw/deps_data_raw.rds")

# All detections
tbl(db, "detections") %>% collect() %>% saveRDS("data_raw/detection_data_raw/dets_data_raw.rds")

#--------------------------------------------#
RSQLite::dbDisconnect(db) # disconnect from database
