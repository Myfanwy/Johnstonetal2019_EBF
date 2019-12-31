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

# BEGIN
#--------------------------------------------#
source("scripts/setup.R") # loads detections cleaned in scripts/analysis/1_munge-raw-data.R, subsets down to wst/chn dets

# White Sturgeon
#--------------------------------------------#
# Get fish paths with tag_tales() function, split paths into detection years, get last location in each detection year.

# TAGS
wst_tags = readxl::read_excel("data_raw/tag_data_raw/WhiteSturgeon_tags.xlsx", na="") %>% 
  select(DateTagged, TagID, CodeSpace, EstTagLife_days, TagGroup)
wst_tags = add_tag_end_col(wst_tags)

# DETECTIONS
wst_dets = subset(dets8, TagID %in% alltags$TagID[alltags$Sp == "wst"])
wst_dets = get_det_year(wst_dets, "DateTimePST")

# Join dets with tagend info; remove detections past tag end date
wst_dets = left_join(wst_dets, select(wst_tags, TagID, EstTagEnd))
wst_dets2 = rm_post_tag_end_dets(wst_dets) 
stopifnot(nrow(wst_dets) - nrow(wst_dets2) == 68) # removed the 68 dets from Tag 2860

#--------------------------------------------#
# Make table of detections per fish per year
dettable <- as.data.frame.matrix(table(wst_dets2$TagID, wst_dets2$Detyear))

# Check 2011 fish's last locations:
wst_paths <- tag_tales(wst_dets2, wst_dets2$TagID, wst_dets2$GroupedStn, "DateTimePST")

wst_paths <- wst_paths %>% 
  arrange(TagID, DateTimePST) %>% 
  select(TagID, CodeSpace, DateTimePST, GroupedStn, Detyear, rkms, EstTagEnd)

wp = split(wst_paths, wst_paths$Detyear)
str(wp) # list of dataframes - need to apply first_last() to each one

wpfl = list(2011:2017)
for(i in 1:length(wp)) {
wpfl[i] = lapply(wp[i], first_last, 
                              tagidcol = "TagID", 
                              datetimecol = "DateTimePST", 
                              stationcol = "GroupedStn") }


wpfl = rbindlist(wpfl)
wpfl = get_det_year(wpfl, "last_det")
filter(wpfl, TagID == 56494)

wpfl %>% 
  group_by(Detyear, TagID) %>% 
  mutate(exit_status = case_when(last_stn %in% exitstations ~ 1,
                                 TRUE ~ 0))


plot_track(wst_paths, 2841)

saveRDS(wst_exits_final, "data/wst_exits_final.rds")


# CHINOOK
chn_fl = first_last(chn_dets, "TagID", "DateTimePST", "GroupedStn")


