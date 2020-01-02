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
# Split into detection years, get last location in each detection year.
#--------------------------------------------#


wp = split(wst_dets, wst_dets$Detyear)

# apply first_last to each detection year
wfl = list()
for(i in 1:7) {
  wfl[i] = lapply(wp[i], first_last, "TagID", "DateTimePST", "GroupedStn")
}
names(wfl) = names(wp)
wfl = data.frame(rbindlist(wfl, use.names = TRUE, idcol = "Detyear"), stringsAsFactors = FALSE)

table(wfl$last_stn, wfl$Detyear)
as.data.frame.matrix(table(wst_dets2$TagID, wst_dets2$Detyear)) #compare with raw dets
filter(wfl, TagID == 56494)

wpfl = wfl %>% 
  group_by(Detyear, TagID) %>% 
  mutate(exit_status = case_when(last_stn %in% exitstations ~ 1,
                                 TRUE ~ 0))


plot_track(wst_paths, 2841)

saveRDS(wst_exits_final, "data/wst_exits_final.rds")


# CHINOOK
chn_fl = first_last(chn_dets, "TagID", "DateTimePST", "GroupedStn")


