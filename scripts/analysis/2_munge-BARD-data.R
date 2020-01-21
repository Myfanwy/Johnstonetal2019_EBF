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
bard <- data.table::fread("~/DropboxCFS/NewPROJECTS/AECCA-2018-YoloTelemetry/WORKING/YB_Vemco/YB_outside_core_array_alldetyears/20200116_MJ_WST_CHN.csv")

# CLEAN DATA
alltags = format_tags(alltags)
chntags = format_tags(chntags)

bard_deps = format_bard_deps(bard_deps)
bard_dets1 = format_bard_dets(bard)

nullrecs = unique(bard_dets1$Receiver[is.na(bard_dets1$rkms)])

bc = sort(unique(bard_dets1$Station))
bc[grep("YB", bc)] #YB names

bard_dets = bard_dets1

# Finish BARD cleaning
bard_dets = get_det_year(bard_dets, "DateTimePST")
bard_dets = rm_redundant_yb_dets(bard_dets) #rm ~281K dets

b = bard_dets
head(b)
csn(b)

bdep = b %>% 
  filter(Receiver %in% nullrecs) %>% 
  group_by(Receiver) %>% 
  filter(!duplicated(Deployment_start), !is.na(rkms)) %>% 
  ungroup() %>% 
  arrange(Receiver) %>% 
  select(Receiver, Station, rkms, Deployment_start, Deployment_end, Detyear)

bdep

bnull = b %>% 
  filter(Station == "NULL") %>% 
  arrange(Receiver, DateTimePST)


bnull %>% 
  group_by(TagID, Receiver) %>% 
  summarise(early = min(DateTimePST),
            late = max(DateTimePST)) -> bnull

bnull = get_det_year(bnull, "late")
arrange(bnull, Receiver)
bdep

bard_dets = BARD_group_stns_AND_rm_simuls(bard_dets) # need to add NULL stns
bard_detsf = BARD_fix_NULL_values(bard_dets)
csn(bard_dets)
csn(bard_detsf)
unique(bard_dets$Receiver[is.na(bard_dets$rkms)])
unique(bard_dets$Station[bard_dets$Receiver %in% c(104441, 104440, 101256, 109544)])
#--------------------------------------------#
unique(bard_deps$rkms[bard_deps$Station %in% unique(bard_dets$Station[bard_dets$Receiver %in% c(104441, 104440, 101256, 109544)])])
