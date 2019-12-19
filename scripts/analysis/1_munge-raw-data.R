#--------------------------------------------#
# Munge data: Step 1 of analysis
# Myfanwy Johnston
# Wed Dec 18 11:00:43 2019 ------------------------------

# LOAD data
# CLEAN data
# Handle duplicate deployments; pull in correct station metadata
# Group detections at gated receivers
# Discard simultaneous detections within grouped stations
# Handle redeployed tag
# Discard detections for a given tag prior to its tag date
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


# CLEAN DATA
alltags = format_tags(alltags)
chntags = format_tags(chntags)

deps = format_deps(deps)
bard_deps = format_bard_deps(bard_deps)

dets = format_dets(dets)
bard_dets = format_bard_dets(bard_dets)

dets2 = rm_dup_dets_within_recs(dets) # slow function; removes duplicate data within TagIDs and Receivers

dets2 = subset_to_study_period(dets2, 
                               start = force_tz(ymd_hms("2012-01-01 00:00:00"), "Pacific/Pitcairn"),
                               end =   force_tz(ymd_hms("2018-07-01 00:00:00"), "Pacific/Pitcairn")) # removes all detections outside study period

# HANDLE DUPLICATE DEPLOYMENTS; PULL IN CORRECT STATION METADATA 
(duprecs <- get_dup_deps(dep_df = as.data.frame(deps))) # 2 locations associated with >1 station names
dets2 = get_stations(dets2, deps) # pull in correct station data

# GROUP STATIONS at gated receiver locations

dets2 = group_stations(dets2)

# DISCARD SIMULTANEOUS DETECTIONS at grouped receiver locations

dets3 = discard_simuls(dets2)

# Remove additional duplicated at Lisbon Weir (where there were 2 receivers but they have the same name in the deployments table)
dups = duplicated(dets3[, c("TagID", "GroupedStn", "DateTimePST")]) # slow; has to check every row

dets4 <- dets3[!dups, ] # filter out duplicate Lisbon Weir detections


# HANDLE REDEPLOYED TAG
alltags %>% filter(TagID == 55555) %>% pull(Comments) # confirm recap tagID
dets5 <- handle_redeployed_tag(dets4)

# DISCARD FALSE DETECTIONS for a given tag prior to its tag date; these represent detections from tags with different codespaces

dets6 <- check_dets_against_tagdates(dets5)


# DISCARD FALSE DETECTIONS identified by VUE software
