#--------------------------------------------#
# Munge data: Step 1 of analysis
# Myfanwy Johnston
# Wed Dec 18 11:00:43 2019 ------------------------------

# LOAD data
# CLEAN data
# Handle duplicate deployments of same receiver SN with get_stations()
# Scan for orphan detections
# Group detections at gated receivers
# Discard simultaneous detections within grouped stations
# Scan for shed tags/mortalities

source("scripts/functions/munging_fxns.R")
source("scripts/functions/convenience_fxns.R")

# LOAD DATA # all raw data created in 0_query-raw-data.R

alltags = readRDS("data_raw/tag_data_raw/alltags_raw.rds") 
chntags = readRDS("data_raw/tag_data_raw/chn_tags_raw.rds")

deps = readRDS("data_raw/deployment_data_raw/deps_data_raw.rds") # YB array deployments only; 
bard_deps = read.csv("data_raw/deployment_data_raw/BARDDeps2011thru18.csv", stringsAsFactors = FALSE) # BARD deployments; contains some YB receivers, have to parse

dets = readRDS("data_raw/detection_data_raw/dets_data_raw.rds")
bard_dets = readRDS("data_raw/detection_data_raw/BARD_query_fcatags.rds")
