#-------------------------------------------------------#
# Step 3 of analysis
# M. Johnston
# Fri Dec 27 13:08:02 2019 ------------------------------

# goal: categorize all fish, each year, into one of three "exit" statuses.  Note that these categories differ slightly from the paper (i.e. categories 1 and 2 are reversed here).
#-------------------------------------------------------#
# 1) Shed/Mortality/Did not Exit the Yolo Bypass
# see paper for definition (listed as category 2).
#
# 2) Exited 
# occurs when a fish's last detection location is at BCN, 
# BCS, the base of the Toe drain, OR in the BARD array, 
# AND that fish is not detected again within the Yolo Bypass 
# array in the same detection year (i.e., white sturgeon 
# have a clear final exit in a given detection year). 
#
# 3) Tag shed in Bypass 
# Ocurs when a tagged fish is recovered by hand somewhere after 
# being released, and their detection history shows their final 
# recorded detection somewhere in the Yolo Bypass array.  This 
# applied to three fish in the study, all recovered at Wallace 
# Weir fyke trap and identified either by their floy tags or 
# their fork lengths + detection histories: 

known_sheds = c(37835 , # this fish was recovered in fyke and identified by its floy tag in 2015
                20161 , # captured in WW fyke and identified by fork length/detection history consistent with capture time
                20164  # captured in WW fyke and identified by fork length/detection history consistent with capture time
                )
#-------------------------------------------------------#
# BEGIN CATEGORIZING (1) and (2) fish:
#--------------------------------------------#
source("scripts/setup.R") # loads detections cleaned in scripts/analysis/1_munge-raw-data.R, subsets down to wst/chn dets

# White Sturgeon
#-------------------------------------------------------#
# for each tagID and year, what stations were they detected at?
wst_dets %>% 
  group_by(TagID, Detyear) %>% 
  filter(!duplicated(GroupedStn)) %>% 
  ungroup() %>% 
  select(TagID, Detyear, GroupedStn) %>% 
  arrange(TagID) -> yb_peryear

# split by TagID/Detyear
syd = split(yb_peryear, list(yb_peryear$TagID, yb_peryear$Detyear))
syd <- syd[sapply(syd, nrow) > 0] # keep the combos that have >1 detection

# apply to a single fish
yb_enter_onefish <- function(df) {
  
  df = as.data.frame(df)
  
  dfnew = data.frame(TagID = as.numeric(unique(df$TagID)),
                   Detyear = as.numeric(unique(df$Detyear)),
                   ybstatus = ifelse(sum(df$GroupedStn %in% ybrecs) > 0, 1, 0) # if they entered YB that year, 1, else 0
                   )
return(dfnew)
}

ybchk = lapply(syd, yb_enter_onefish)
ybchk = do.call(rbind, ybchk)


tail(arrange(ybchk, TagID), 25) # if they weren't detected at all that year, they wouldn't be in the detections. Of those detected, should only analyze exits for fish that have a 1 in that year.

wp = left_join(wst_dets, ybchk)
wp <- filter(wp, ybstatus == 1)
wp = data.frame(wp)

# apply first_last to each detection year
wp = split(wp, wp$Detyear)
wfl = list()
for(i in 1:7) {
  wfl[i] = lapply(wp[i], first_last, "TagID", "DateTimePST", "GroupedStn")
}
names(wfl) = names(wp)

wfl = data.frame(rbindlist(wfl, use.names = TRUE, idcol = "Detyear"), stringsAsFactors = FALSE)

wpfl = wfl %>% 
  group_by(Detyear, TagID) %>% 
  mutate(exit_status = case_when(last_stn %in% ybrecs ~ 1, # 1 = did not exit/mortality/shed tag
                                 TRUE ~ 2)) %>% # 2 = exited
  mutate(exit_status = ifelse(TagID %in% known_sheds, 3, exit_status)) %>% # 3 = shed tag
  ungroup()

# manual assignments: we know that 56487 exited in 2013 because it's detected elsewhere in the delta in later years:

wpfl$exit_status[wpfl$TagID == 56487 & wpfl$Detyear == 2013] <- 2

wpfl$Detyear = as.numeric(wpfl$Detyear)
wpfl$Sp = "wst"

table(wpfl$exit_status) # 177 exits, 5 non-exits

saveRDS(wpfl, "data_clean/model_data/wst_exits_modeldata.rds")

#--------------------------------------------#
# CHINOOK SALMON
#--------------------------------------------#
chn_fl = first_last(chn_dets, "TagID", "DateTimePST", "GroupedStn")
chnidx <- select(alltags, TagID, TagGroup, Sp) %>% 
  filter(!duplicated(TagID), Sp == "chn")

fl <- left_join(chn_fl, chnidx)
fl <- get_det_year(fl, "first_det")

fl <- fl %>% 
  filter(TagGroup != "fca_2012") %>%  #filter out 2012 fish; not included in analyses
  group_by(Detyear, TagID) %>% 
  mutate(exit_status = case_when(last_stn %in% ybrecs ~ 1, # 1 = did not exit/mortality/shed tag
                                 TRUE ~ 2)) %>% # 2 = exited
  mutate(exit_status = ifelse(TagID %in% known_sheds, 3, exit_status)) %>% # 3 = shed tag
  ungroup()

table(fl$exit_status)

saveRDS(fl, "data_clean/model_data/chn_exits_modeldata.rds")
