# Munge BARD detections - get into same format as YB detections to join
# Fri Jan  3 15:14:02 2020 ------------------------------

source("scripts/functions/munging_fxns.R")
source("scripts/functions/convenience_fxns.R")
source("scripts/functions/munging_bard_fxns.R")
stns <- readRDS("data_clean/deployment_data/stns.rds")

# LOAD DATA # all raw data created in 0_query-raw-data.R
 alltags = readRDS("data_raw/tag_data_raw/alltags_raw.rds") 
 chntags = readRDS("data_raw/tag_data_raw/chn_tags_raw.rds")
# 
# # Cleaning BARD detections: orig BARD query db NOT included in this repo, so this part is read-only
# bard <- data.table::fread("~/DropboxCFS/NewPROJECTS/AECCA-2018-YoloTelemetry/WORKING/YB_Vemco/YB_outside_core_array_alldetyears/20200116_MJ_WST_CHN.csv")
# 

# 
# bard_deps = format_bard_deps(bard_deps)
# bard_dets = format_bard_dets(bard)
# 
# # Finish BARD cleaning
# bard_dets = get_det_year(bard_dets, "DateTimePST")
# bard_dets = rm_redundant_yb_dets(bard_dets) #rm ~281K dets
# 
# bard_dets = BARD_fix_NULL_values(bard_dets)
# bard_dets = BARD_group_stns_AND_rm_simuls(bard_dets) # ~2.5 million dets remaining
#--------------------------------------------#
# saveRDS(bard_dets, "data_clean/bard_data/bard_dets.rds")
 
# LOAD/FORMAT DATA to join
alltags = format_tags(alltags)
chntags = format_tags(chntags)
bard_dets <- readRDS("data_clean/bard_data/bard_dets.rds")

# Join with dets 8, detection data cleaned in analysis step 1 (1_munge-raw-data.R):
dets8 = readRDS("data_clean/detection_data/1_yb_detections.rds")
dets9 = join_with_bard(dets_df = dets8, bard_dets_df = bard_dets)

# Save for step 3 of analysis
saveRDS(dets9, "data_clean/detection_data/2_all_detections.rds")
