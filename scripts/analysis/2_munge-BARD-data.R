# Munge BARD detections - get into same format as YB detections to join
# Fri Jan  3 15:14:02 2020 ------------------------------

source("scripts/functions/munging_fxns.R")
source("scripts/functions/convenience_fxns.R")
source("scripts/functions/munging_bard_fxns.R")
stns <- readRDS("data_clean/deployment_data/stns.rds")

# LOAD DATA # all raw data created in 0_query-raw-data.R
alltags = readRDS("data_raw/tag_data_raw/alltags_raw.rds") 
chntags = readRDS("data_raw/tag_data_raw/chn_tags_raw.rds")

# deployment data
bard_deps = read.csv("data_raw/deployment_data_raw/BARDDeps2011thru18.csv", stringsAsFactors = FALSE) # BARD deployments; contains some YB receivers, have to parse

# BARD detections
bard_chn = data.frame(readRDS("data_raw/detection_data_raw/BARD_query_fcatags.rds"))
bard_wst = readRDS("data_raw/detection_data_raw/2017WST_queryresults.rds") 

# CLEAN DATA
alltags = format_tags(alltags)
chntags = format_tags(chntags)

bard_deps = format_bard_deps(bard_deps)

bard_dets1 = format_bard_dets(bard_chn)
bard_dets2 = format_bard_dets(bard_wst)

setdiff(unique(bard_dets1$Station), unique(bard_dets2$Station)) # mostly northern recs

setdiff(unique(bard_dets2$Station), unique(bard_dets1$Station)) # mostly estuary recs

bc = sort(unique(bard_dets1$Station))
bw = sort(unique(bard_dets2$Station))

bc[grep("YB", bc)] #omg they have DIFFERENT YB names
bw[grep("YB", bw)]

bard_dets = bind_rows(bard_chn, bard_wst)

bd_check = bind_rows(bard_dets1, bard_dets2)


# Finish BARD cleaning
bard_dets = get_det_year(bard_dets, "DateTimePST")
bard_dets = rm_redundant_yb_dets(bard_dets) # should remove 276891 dets
bard_dets = BARD_group_stns_AND_rm_simuls(bard_dets) # need to add NULL stns
bard_detsf = BARD_fix_NULL_values(bard_dets)
csn(bard_dets)
csn(bard_detsf)
unique(bard_detsf$Receiver[is.na(bard_detsf$rkms)])
#--------------------------------------------#
