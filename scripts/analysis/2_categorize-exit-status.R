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


# with white sturgeon, have to go by year.  Goal: make a list of all wst tags and detection years
wst_tags = readxl::read_excel("data_raw/tag_data_raw/WhiteSturgeon_tags.xlsx", na="") %>% 
  select(DateTagged, TagID, CodeSpace, EstTagLife_days, TagGroup)

wst_dets = subset(dets8, TagID %in% alltags$TagID[alltags$Sp == "wst"])

wst_dets = get_det_year(wst_dets, "DateTimePST")

wst_exits = mk_exit_tbl()
wst_tbl = mk_wst_tbl()

wst_exits = merge(wst_exits, wst_tbl, all.x = TRUE, all.y = TRUE, by = "TagID")

wst_tags = add_tag_end_col(wst_tags)
wst_dets = left_join(wst_dets, select(wst_tags, TagID, EstTagEnd))
wst_dets2 = rm_post_tag_end_dets(wst_dets) 
stopifnot(nrow(wst_dets) - nrow(wst_dets2) == 68) # removed the 68 dets from Tag 2860

wst_exits2 <- wst_exits %>% 
  group_by(TagID) %>% 
  filter(Detyear >= DetyearTagged) %>% 
  ungroup() %>% 
  arrange(TagID, Detyear) 

#--------------------------------------------#

# Make table of detections per fish per year
dettable <- as.data.frame.matrix(table(wst$TagID, wst$Detyear))
write.csv(dettable, file = "data/wst_returns.csv", quote = FALSE)

# Check 2011 fish's last locations:
wst_paths <- fishpaths(wst, wst$TagID, wst$Station)
wst_paths <- wst_paths %>% 
  arrange(TagID, DateTimeUTC) %>% 
  select(TagID, DateTimeUTC, Station, Detyear, DateTagged)

filter(wst_paths, TagID == 2841) # iterated through each tagID and verified that final detection was at BC_joint or Base_TD; if so, 1, if not, 0. Saved in wst_returns_handchecked.xlsx

#--------------------------------------------#
wst_returns_handchecked <- readxl::read_excel("data/wst_returns_handchecked.xlsx", na = "NA")

wst_returns_hc_tidy <- tidyr::gather(wst_returns_handchecked, key = "Detyear", value = "ExitStatus", -TagID) %>% 
  mutate(Detyear = as.integer(Detyear))

wst_exits_final <- left_join(wst_exits, wst_returns_hc_tidy) %>% 
  filter(!is.na(ExitStatus)) %>% 
  mutate(Species = "wst") %>% 
  select(TagID, Species, Detyear, TagDetyear, ExitStatus)

saveRDS(wst_exits_final, "data/wst_exits_final.rds")
