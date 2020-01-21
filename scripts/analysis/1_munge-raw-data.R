#--------------------------------------------#
# Munge data: Step 1 of analysis
# Myfanwy Johnston
# Wed Dec 18 11:00:43 2019 ------------------------------

# LOAD data
# CLEAN data - format, remove duplicates within TagIDs/receivers
# Handle duplicate deployments; pull in correct station metadata*
# Discard redundant stations^
# Group detections at gated receivers
# Discard simultaneous detections within grouped stations
# Handle redeployed tag
# Discard detections for a given tag prior to its tag date
# Verify & discard false detections identified by VUE software*

# *applies to YB detections only
# ^applies to BARD detections only

source("scripts/functions/munging_fxns.R")
source("scripts/functions/convenience_fxns.R")
stns <- readRDS("data_clean/deployment_data/stns.rds")

# LOAD DATA # all raw data created in 0_query-raw-data.R
alltags = readRDS("data_raw/tag_data_raw/alltags_raw.rds") 
chntags = readRDS("data_raw/tag_data_raw/chn_tags_raw.rds")

deps = readRDS("data_raw/deployment_data_raw/deps_data_raw.rds") # YB array deployments only; 

dets = readRDS("data_raw/detection_data_raw/dets_data_raw.rds")

bard_dets1 = data.frame(readRDS("data_raw/detection_data_raw/BARD_query_fcatags.rds"))
bard_dets2 = readRDS("data_raw/detection_data_raw/2017WST_queryresults.rds") 


# CLEAN DATA
alltags = format_tags(alltags)
chntags = format_tags(chntags)

deps = format_deps(deps)

dets = format_dets(dets)

dets2 = rm_dup_dets_within_recs(dets) # slow function; removes duplicate data within TagIDs and Receivers
dets2 = subset_to_study_period(dets2, 
                               start = force_tz(ymd_hms("2012-01-01 00:00:00"), "Pacific/Pitcairn"),
                               end =   force_tz(ymd_hms("2018-07-01 00:00:00"), "Pacific/Pitcairn")) # removes all detections outside study period
#--------------------------------------------#

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
dets7 <- add_rkms(dets6)
# DISCARD FALSE DETECTIONS identified by VUE software

fda = readRDS("data_clean/detection_data/FDA_screen2012-2018.rds") # Exported from VUE; cleaned in separate project repository (YBAnalysis)
# make one large df with both false and real detections; plot to compare:
mm <- merge(dets7, fda[,c("TagID","Receiver","DateTimePST")], all.x = FALSE, all.y = FALSE)

library(ggplot2)
#--------------------------------------------#
fds_tagids = unique(mm$TagID)
# series of faceted plots to inspect:
p = list(
aa = plot_fdas(dets7, mm, tagids_subset = fds_tagids[1:6]),
bb = plot_fdas(dets7, mm, tagids_subset = fds_tagids[7:12]),
cc = plot_fdas(dets7, mm, tagids_subset = fds_tagids[13:18]),
dd = plot_fdas(dets7, mm, tagids_subset = fds_tagids[19:24]),
ee = plot_fdas(dets7, mm, tagids_subset = fds_tagids[25:30]),
ff = plot_fdas(dets7, mm, tagids_subset = fds_tagids[31:36]),
gg = plot_fdas(dets7, mm, tagids_subset = fds_tagids[37:42]),
hh = plot_fdas(dets7, mm, tagids_subset = fds_tagids[43:48]),
ii = plot_fdas(dets7, mm, tagids_subset = fds_tagids[49:50])
)
#--------------------------------------------#

library(gridExtra)
plots <- do.call(marrangeGrob, args = list(grobs = p, ncol=1, nrow=1))
ggsave("figures/false_det_screen.pdf", plots, width=11, height=8.5)

# After visually checking spatiotemporal history of each fish, tags with likely true false detections to discard:
dis <- c(13728, 31563, 37835,  46644,  56473,  56483,  56492, 56494) # 56494; only discard the 2013 detection
keep56494 <- filter(mm, TagID == 56494 & DateTimePST == "2017-02-11 03:40:23")
mm <- anti_join(mm, keep56494)

# Notes on detection histories:
#--------------------------------------------#
# false or shed: 13728 (chn, 2015), 23053 (chn, 2013)
# Possible overtopping: 46644 - discarded for now, but would be good to check
# Just plain weird: 56473, 56492
# good story of returns: 56477, 56486
# uneven gaps inconsistent with a shed: 2619
# Wallace weir rescues: 37835, 37845 (both in 2015), probably 20161, and 20164 (2014)

fd_discard <- filter(mm, TagID %in% dis)
filter(fd_discard, TagID == 56494) %>% pull(DateTimePST) # good; this is the one we want to discard, not the other one
filter(fd_discard, TagID == 13728)
filter(fd_discard, TagID == 56483)

# Remove false detections from dets7 with an anti-join; should remove 12 dets from 9 fish
filter(dets7, TagID == 56483, Station == "YBBLR") # there's actually 2 false dets; VUE didn't catch one of them
dets8 <- anti_join(dets7, fd_discard[,c("TagID","Receiver","DateTimePST")])

# get stragglers:
rm56483 <- filter(dets8, TagID == 56483, Station == "YBBLR")
rm31555 <- filter(dets8, TagID == 31555, DateTimePST > as.Date("2015-01-01")) # different codespace tag

dets8 = anti_join(dets8, rm56483)
dets8 = anti_join(dets8, rm31555)


dets9 = join_with_bard(dets_df = dets8, bard_dets_df = bard_dets)

saveRDS(dets9, "data_clean/detection_data/1_all_detections.rds")
